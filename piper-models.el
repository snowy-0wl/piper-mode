;;; piper-models.el --- Voice model management for piper-mode -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Keywords: multimedia, tts, accessibility, speech

;;; Commentary:
;; Voice model fetching, parsing, and management for piper-mode.

;;; Code:

(require 'piper-infra)
(require 'json)

(defcustom piper-voice-model nil
  "Voice model to use for text-to-speech.
Can be either:
- A filename (e.g., \"en_US-joe-medium.onnx\") - will be looked up
  in models directory
- An absolute path to a model file
- nil to use the default model

The model file will be automatically downloaded if it doesn't exist."
  :type '(choice (const :tag "Default model" nil)
                 (string :tag "Model filename or path"))
  :group 'piper)

(defcustom piper-models-url "https://raw.githubusercontent.com/rhasspy/piper/master/VOICES.md"
  "URL to fetch available voice models from."
  :type 'string
  :group 'piper)

(defconst piper-default-model "en_US-joe-medium.onnx"
  "Default voice model filename.")

(defvar piper--available-models nil
  "Cache of available voice models.")

(cl-defstruct (piper-model (:constructor piper-model-create)
                           (:copier piper-model-copy))
  "Structure for storing voice model information."
  name model-url config-url lang-name lang-code voice quality)

(defun piper--ensure-model (model)
  "Ensure the voice MODEL exists and return its full path.
If MODEL is nil, use `piper-default-model`.
If the model file does not exist, attempt to download it."
  (let* ((model-name (or model piper-default-model))
         (full-path (if (file-name-absolute-p model-name)
                        model-name
                      (expand-file-name model-name (expand-file-name "models" (piper--get-install-dir))))))
    
    ;; Check if model exists
    (unless (file-exists-p full-path)
      (message "Model %s not found, attempting to download..." (file-name-nondirectory full-path))
      
      ;; Ensure we have available models
      (unless piper--available-models
        (piper--fetch-available-models))
      
      ;; Find the model URL
      (let* ((target-name (file-name-nondirectory full-path))
             (model-info (seq-find (lambda (m)
                                     (string-suffix-p target-name (piper--clean-url (piper-model-model-url m))))
                                   piper--available-models)))
        
        (if model-info
            (piper--download-model (piper-model-model-url model-info) full-path)
          (error "Could not find download URL for model: %s" target-name))))
    
    full-path))

(defun piper--clean-url (url)
  "Clean URL by removing query parameters and fragments."
  (car (split-string url "[?#]")))

(defun piper--get-config-url (model-url)
  "Get config URL from model URL."
  (let* ((clean-url (piper--clean-url model-url))
         (config-url (if (string-match "\\.onnx$" clean-url)
                        (concat (substring clean-url 0 (match-beginning 0)) ".onnx.json")
                      (concat clean-url ".json"))))
    (piper--log "Generated config URL: %s" config-url)
    config-url))

(defun piper--extract-model-info (url &optional known-lang-name known-lang-code)
  "Extract model information from URL.
Returns a piper-model structure, or nil if URL is invalid.
Optional KNOWN-LANG-NAME and KNOWN-LANG-CODE can be provided from context."
  (piper--log "Extracting model info from URL: %s" url)
  
  (let (lang lang-code voice quality model-name)
    ;; Try the full URL pattern first
    (cond
     ;; Try to match the full huggingface URL pattern
     ;; Matches structure: .../lang_family/lang_code/voice/quality/filename.onnx
     ;; Example: .../en/en_US/ryan/high/en_US-ryan-high.onnx
     ((string-match "/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/[^/]+\\.onnx" url)
      (setq lang (match-string 1 url)
            lang-code (match-string 2 url)
            voice (match-string 3 url)
            quality (match-string 4 url)))
     
     ;; Fallback: Try to extract from filename pattern (e.g., en_US-joe-medium.onnx)
     ((string-match "/\\([^/]+\\)\.onnx" url)
      (setq model-name (match-string 1 url))
      (let* ((parts (split-string model-name "-")))
        (when (>= (length parts) 3)
          (setq lang-code (car parts)
                voice (nth 1 parts)
                quality (nth 2 parts)
                lang (car (split-string lang-code "_")))))))
    
    ;; If we have the necessary components, create the model structure
    (when (and voice quality (or lang lang-code))
      (let* ((effective-lang-code (or known-lang-code lang-code lang))
             (lang-proper-name (or known-lang-name 
                                 (car (split-string effective-lang-code "_"))))
             (config-url (piper--get-config-url url)))
        
        (piper--log "Extracted model info: lang=%s code=%s voice=%s quality=%s" 
                   lang-proper-name effective-lang-code voice quality)
        
        (piper-model-create
         :name (format "%s (%s) - %s" voice quality effective-lang-code)
         :model-url url
         :config-url config-url
         :lang-name lang-proper-name
         :lang-code effective-lang-code
         :voice voice
         :quality quality)))))

(defun piper--extract-model-name (url)
  "Extract model name from URL."
  (when (string-match "/\\([^/]+\\)\.onnx" url)
    (match-string 1 url)))

(defun piper--parse-models-file ()
  "Parse the models file to find .onnx files and their configs.
Returns a list of (model-name full-model-url config-url lang-name lang-code)."
  (let (models
        current-lang-name
        current-lang-code)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties 
                   (line-beginning-position) 
                   (line-end-position))))
        
        ;; Check for language header: * Language Name (`code`, ...) or * Language Name (code)
        ;; Example: * English (`en_US`, ...) or * English (en_GB)
        (when (string-match "^\\* \\(.+?\\) ([`]?\\([^`)]+\\)[`]?" line)
          (setq current-lang-name (string-trim (match-string 1 line))
                current-lang-code (match-string 2 line))
          (piper--log "Found language section: %s (%s)" current-lang-name current-lang-code))
        
        ;; Look for .onnx URLs
        (when (string-match "huggingface\.co/[^)]+\.onnx" line)
          (let* ((model-url (match-string 0 line))
                 (full-model-url (concat "https://" model-url "?download=true"))
                 (config-url (concat "https://" model-url ".json?download=true"))
                 (model-name (piper--extract-model-name full-model-url)))
            (when model-name
              (piper--log "Found model: %s (Lang: %s)" model-name current-lang-name)
              ;; Push with language context
              (push (list model-name full-model-url config-url current-lang-name current-lang-code) models))))
        (forward-line)))
    
    (if models
        (let ((sorted-models (sort (nreverse models)
                                  (lambda (a b)
                                    (string< (car a) (car b))))))
          (piper--log "Found %d models" (length sorted-models))
          sorted-models)
      (message "No models found in the file")
      nil)))

(defun piper--find-models-in-buffer ()
  "Find all model/config pairs in current buffer."
  (let ((raw-models (piper--parse-models-file))
        models)
    (dolist (model-info raw-models)
      (let* ((model-url (nth 1 model-info))
             (lang-name (nth 3 model-info))
             (lang-code (nth 4 model-info))
             ;; Pass language context to extraction
             (model (piper--extract-model-info model-url lang-name lang-code)))
        (when model
          (push model models))))
    (if models
        (progn
          (piper--log "Successfully parsed %d models" (length models))
          (nreverse models))
      (error "No models found in the file"))))

(defun piper--fetch-available-models ()
  "Fetch and parse available models from piper-models-url."
  (let ((temp-file (make-temp-file "piper-models"))
        (models nil))
    (unwind-protect
        (progn
          (piper--log "Starting model fetch from %s" piper-models-url)
          (let ((curl-status (shell-command
                            (format "curl -s -f -L -o %s %s"
                                    (shell-quote-argument temp-file)
                                    (shell-quote-argument piper-models-url)))))
            (if (not (eq curl-status 0))
                (error "Failed to download models file")
              (piper--log "Fetching models from %s" piper-models-url)
              (with-temp-buffer
                (insert-file-contents temp-file)
                (let ((content (buffer-string)))
                  (piper--log "File size: %d bytes" (length content))
                  (setq models (piper--find-models-in-buffer))
                  (if (null models)
                      (error "No models found in the file")
                    (progn
                      (piper--log "Setting piper--available-models to %d models" (length models))
                      (setq piper--available-models models)
                      piper--available-models))))))
      (when (file-exists-p temp-file)
        (piper--log "Cleaned up temp file %s" temp-file)
        (delete-file temp-file))))))

(defun piper--download-model (url output-file)
  "Download model from URL to OUTPUT-FILE using curl."
  (make-directory (file-name-directory output-file) t)
  (let* ((model-url (if (string-match-p "\?download=true" url)
                       url
                     (concat url "?download=true")))
         (clean-output-file (piper--clean-url output-file))
         (model-reporter (make-progress-reporter "Downloading voice model..." 0 100))
         (config-reporter (make-progress-reporter "Downloading model config..." 0 100))
         (process-connection-type nil)  ; Use pipes for curl output
         (temp-buffer (generate-new-buffer " *curl-output*")))
    (unwind-protect
        (progn
          (piper--log "Starting download of %s to %s" model-url output-file)
          (piper--log "Clean output file: %s" clean-output-file)
          
          ;; Download model file
          (let ((result (call-process "curl" nil temp-buffer nil
                                     "--silent" "--fail" "--location"
                                     "--output" clean-output-file
                                     "--create-dirs"
                                     "--progress-bar"
                                     "-A" "Mozilla/5.0"
                                     "-H" "Accept: application/octet-stream"
                                     model-url)))
            (if (= result 0)
                (progn
                  (progress-reporter-done model-reporter)
                  ;; Download config file
                  (let* ((config-url (concat (string-remove-suffix "?download=true" model-url) ".json?download=true"))
                         (config-result (call-process "curl" nil temp-buffer nil
                                                    "--silent" "--fail" "--location"
                                                    "--output" (concat clean-output-file ".json")
                                                    "--create-dirs"
                                                    "--progress-bar"
                                                    "-A" "Mozilla/5.0"
                                                    "-H" "Accept: application/json"
                                                    config-url)))
                    (progress-reporter-done config-reporter)
                    (if (= config-result 0)
                        (progn
                          (message "Model and config downloaded successfully.")
                          (piper--log "Download completed successfully"))
                      (error "Failed to download config file: %s" config-result))))
              (error "Failed to download model file: %s" result))))
      ;; Cleanup
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

(defun piper-select-model (&optional return-only)
  "Select and switch to a different voice model.
If RETURN-ONLY is non-nil, return the selected model structure
instead of setting it."
  (interactive)
  (unless piper--available-models
    (message "Fetching available models...")
    (piper--fetch-available-models))
  
  ;; Group models by language
  (let* ((lang-groups (delete-dups
                       (mapcar (lambda (m)
                                (let ((code (piper-model-lang-code m))
                                      (name (piper-model-lang-name m)))
                                  (cons code name)))
                               piper--available-models)))
         ;; Sort languages by name
         (sorted-lang-groups (sort lang-groups
                                  (lambda (a b)
                                    (string< (cdr a) (cdr b)))))
         ;; First select language
         (selected-lang-pair (completing-read
                              "Select language: "
                              (mapcar (lambda (pair)
                                       (format "%s (%s)" (cdr pair) (car pair)))
                                     sorted-lang-groups)
                              nil t))
         ;; Extract the lang code from selection "Name (code)"  
         (lang-code (progn
                     (string-match "(\\([^)]+\\))" selected-lang-pair)
                     (match-string 1 selected-lang-pair)))
         ;; Filter models for selected language using lang-code
         (lang-models (seq-filter
                      (lambda (m)
                       (string= (piper-model-lang-code m) lang-code))
                      piper--available-models))
         ;; Then select specific model
         (model-choices (mapcar (lambda (m)
                                (let* ((url (piper-model-model-url m))
                                       (filename (file-name-nondirectory (piper--clean-url url)))
                                       (local-path (expand-file-name filename (expand-file-name "models" (piper--get-install-dir))))
                                       (installed (file-exists-p local-path))
                                       (status (if installed " [Installed]" "")))
                                  (cons (format "%s (%s)%s"
                                                (piper-model-voice m)
                                                (piper-model-quality m)
                                                status)
                                        m)))
                               lang-models))
         (selected-model-name (completing-read
                              (format "Select %s voice: " (or lang-code "voice"))
                              (mapcar #'car model-choices)
                              nil t))
         (selected-model (cdr (assoc selected-model-name model-choices))))
    
    (if return-only
        selected-model
      (let* ((model-url (piper-model-model-url selected-model))
             (clean-url (piper--clean-url model-url))
             (model-file (expand-file-name
                         (file-name-nondirectory clean-url)
                         (expand-file-name "models" (piper--get-install-dir)))))
        
        ;; Download if needed
        (unless (file-exists-p model-file)
          (message "Downloading model %s..." (piper-model-name selected-model))
          (piper--download-model model-url model-file))
        
        (setq piper-voice-model model-file)
        (message "Switched to model: %s (%s)"
                 (piper-model-voice selected-model)
                 (piper-model-quality selected-model))))))

(defun piper-describe-model (&optional model)
  "Display information about the current or specified voice MODEL.
If MODEL is nil, use the currently configured `piper-voice-model`.
Interactively, with a prefix argument, prompt to select a model to describe."
  (interactive
   (list (if current-prefix-arg
             (piper-select-model t)
           nil)))
  
  (let* ((model-path (if (stringp model) model piper-voice-model))
         (model-struct (if (piper-model-p model)
                           model
                         ;; Try to find the struct in available models that matches the file/url
                         (when piper--available-models
                           (seq-find (lambda (m)
                                       (let ((m-url (piper-model-model-url m)))
                                         (or (string= m-url model-path)
                                             (and model-path
                                                  (string-suffix-p (file-name-nondirectory (piper--clean-url m-url))
                                                                 (file-name-nondirectory model-path))))))
                                     piper--available-models)))))
    
    (with-help-window "*Piper Model*"
      (with-current-buffer "*Piper Model*"
        (insert (propertize "Piper Voice Model Information\n" 'face 'bold 'font-lock-face 'bold))
        (insert (make-string 30 ?=) "\n\n")
        
        (if model-struct
            (progn
              (insert (format "Name:      %s\n" (piper-model-name model-struct)))
              (insert (format "Language:  %s (%s)\n" (piper-model-lang-name model-struct) (piper-model-lang-code model-struct)))
              (insert (format "Voice:     %s\n" (piper-model-voice model-struct)))
              (insert (format "Quality:   %s\n" (piper-model-quality model-struct)))
              (insert (format "URL:       %s\n" (piper-model-model-url model-struct))))
          (insert "Model metadata not found in cache (or custom model).\n"))
        
        (insert (propertize "\nConfiguration\n" 'face 'bold 'font-lock-face 'bold))
        (insert (make-string 13 ?-) "\n")
        
        (let ((current-path (if (piper-model-p model)
                                (let ((url (piper-model-model-url model)))
                                  (expand-file-name (file-name-nondirectory (piper--clean-url url))
                                                  (expand-file-name "models" (piper--get-install-dir))))
                              model-path)))
          
          (insert (format "Path:       %s\n" (or current-path "None configured")))
          (insert (format "Status:     %s\n" 
                          (if (and current-path (file-exists-p current-path))
                              (propertize "Installed" 'face 'success)
                            (propertize "Not Installed" 'face 'error))))
          
          (when (and current-path (file-exists-p current-path))
            (insert (format "Size:       %s\n" (file-size-human-readable (file-attribute-size (file-attributes current-path)))))))))))

(provide 'piper-models)
;;; piper-models.el ends here
