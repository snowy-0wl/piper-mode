;;; piper-mode.el --- Text-to-speech using Piper TTS -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Version: 0.5
;; Package-Requires: ((emacs "27.1") (json "1.4") (cl-lib "0.5"))
;; Keywords: multimedia, tts, accessibility, speech
;; URL: https://github.com/snowy-0wl/piper-mode

;;; Commentary:

;; A text-to-speech package that uses the Piper TTS engine.
;; Provides natural-sounding speech synthesis with various voice models.
;;
;; Available commands:
;; - piper-speak: Speak the given text
;; - piper-speak-region: Speak the selected region
;; - piper-speak-buffer: Speak the entire buffer
;; - piper-speak-paragraph: Speak the current paragraph
;; - piper-speak-line: Speak the current line
;; - piper-speak-word: Speak the current word
;; - piper-read-from-point-to-end: Read from current point to end of buffer
;; - piper-stop: Stop speaking and clean up

(require 'json)
(require 'cl-lib)

(defgroup piper nil
  "Text-to-speech using Piper TTS."
  :group 'multimedia
  :prefix "piper-")

(defface piper-highlight-face
  '((((class color) (min-colors 88) (background light))
     (:background "blue" :foreground "white" :weight bold))
    (((class color) (min-colors 88) (background dark))
     (:background "orange" :foreground "black" :weight bold))
    (t (:inverse-video t :weight bold)))
  "Face for highlighting text during TTS playback."
  :group 'piper)

(defcustom piper-highlight-mode 'sentence
  "Mode for highlighting text during TTS playback.
Possible values are:
- \='sentence to highlight sentence by sentence
- \='line to highlight line by line
- nil to disable highlighting"
  :type '(choice (const :tag "Sentence" sentence)
                 (const :tag "Line" line)
                 (const :tag "Off" nil))
  :group 'piper)

(defcustom piper-debug nil
  "Whether to enable debug logging."
  :type 'boolean
  :group 'piper)

(defcustom piper-auto-setup t
  "Whether to automatically run the setup script when needed.
When non-nil, the setup script will be run automatically when piper is
first used and the binaries are not found or not executable."
  :type 'boolean
  :group 'piper)

(defcustom piper-install-dir (expand-file-name "piper-tts" user-emacs-directory)
  "Directory where Piper TTS binaries and models will be installed.
Defaults to `piper-tts' in your Emacs configuration directory.
If nil, installs inside the package directory (not recommended for straight.el
users)."
  :type 'directory
  :group 'piper)

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

(defcustom piper-highlight-lookahead 0.05
  "Lookahead time in seconds for text highlighting.
Higher values will highlight the text earlier, lower values will be more precise
but might lag behind the audio. Adjust this for your preference."
  :type 'number
  :group 'piper)

;; Internal chunking constants
(defconst piper--chunk-size 1500
  "Optimal number of characters to process in a single TTS chunk.")

(defconst piper--chunk-overlap 100
  "Number of characters to overlap between chunks for sentence boundary detection.")

(defvar piper--available-models nil
  "Cache of available voice models.")

(defvar piper--source-dir (file-name-directory (or load-file-name buffer-file-name))
  "Source directory of the piper-mode package.")

(defvar piper-script-path nil
  "Path to the Piper run script.")

(defvar piper-temp-dir nil
  "Temporary directory for WAV files.")

(defun piper--log (format-string &rest args)
  "Log a message if debug is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when piper-debug
    (apply #'message (concat "piper-debug: " format-string) args)))

;; Define the Straight.el build directory variable
(defvar straight-build-dir nil
  "Straight.el build directory.")

(defun piper--get-install-dir ()
  "Get the installation directory for Piper.
This is either the custom directory set in `piper-install-dir', or
derived from the package location."
  (or piper-install-dir
      (let* ((source-dir (directory-file-name piper--source-dir))

             (straight-build-dir (when (string-match "/straight/repos/" source-dir)
                                 (replace-match "/straight/build/" t t source-dir))))
        (or straight-build-dir source-dir))))

(defun piper--initialize-paths ()
  "Initialize paths based on install directory."
  (let ((install-dir (piper--get-install-dir)))
    (piper--log "Initializing paths with install-dir: %s" install-dir)
    (unless piper-script-path
      (setq piper-script-path (piper--get-script-path)))
    ;; Voice model will be auto-downloaded by piper--ensure-model if needed
    (unless piper-temp-dir
      (setq piper-temp-dir (piper--get-temp-dir)))))

(defun piper--get-script-path ()
  "Get the path to the Piper run script."
  (expand-file-name "bin/piper-with-phonemes" (piper--get-install-dir)))

(defun piper--get-temp-dir ()
  "Get the temporary directory for WAV files."
  (let ((tmp-dir (expand-file-name "tmp" (piper--get-install-dir))))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir t))
    tmp-dir))

(defun piper--expand-model-path (model)
  "Expand MODEL to a full path.
If MODEL is nil, return nil.
If MODEL is already an absolute path, return it as-is.
If MODEL is a relative path or just a filename, expand it relative
to the models directory."
  (when model
    (if (file-name-absolute-p model)
        model
      (expand-file-name model (expand-file-name "models" (piper--get-install-dir))))))

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

(cl-defstruct (piper-model (:constructor piper-model-create)
                           (:copier piper-model-copy))
  "Structure for storing voice model information."
  name model-url config-url lang-name lang-code voice quality)

(defun piper--extract-model-info (url)
  "Extract model information from URL.
Returns a piper-model structure, or nil if URL is invalid."
  (piper--log "Extracting model info from URL: %s" url)
  
  (let (lang lang-code voice quality model-name)
    ;; Try the full URL pattern first
    (cond
     ;; Try to match the full huggingface URL pattern
     ((string-match "huggingface\\.co/[^/]+/[^/]+/[^/]+/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/[^?]+\\.onnx" url)
      (setq lang (match-string 1 url)
            lang-code (match-string 2 url)
            voice (match-string 3 url)
            quality (match-string 4 url)))
     
     ;; Try to extract from filename pattern (e.g., en_US-joe-medium.onnx)
     ((string-match "/\\([^/]+\\)\.onnx" url)
      (setq model-name (match-string 1 url))
      (let* ((parts (split-string model-name "-")))
        (when (>= (length parts) 3)
          (setq lang-code (car parts)
                voice (nth 1 parts)
                quality (nth 2 parts)
                lang lang-code)))))
    
    ;; If we have the necessary components, create the model structure
    (when (and voice quality (or lang lang-code))
      (let* ((effective-lang (or lang lang-code))
             (effective-lang-code (or lang-code lang))
             (config-url (piper--get-config-url url)))
        (piper--log "Extracted model info: lang=%s code=%s voice=%s quality=%s" 
                   effective-lang effective-lang-code voice quality)
        (piper--log "Generated config URL: %s" config-url)
        (piper--log "Model name: %s" model-name)
        (piper--log "Model URL: %s" url)
        (piper--log "Config URL: %s" config-url)
        (piper--log "Creating model structure")
        (piper-model-create
         :name (format "%s - %s (%s)" voice quality effective-lang)
         :model-url url
         :config-url config-url
         :lang-name effective-lang
         :lang-code effective-lang-code
         :voice voice
         :quality quality)))))

(defun file-basename (path &optional ext)
  "Return the basename of PATH, optionally removing extension EXT if specified."
  (let ((base (file-name-nondirectory path)))
    (if ext
        (if (string-match (concat (regexp-quote ext) "$") base)
            (substring base 0 (match-beginning 0))
          base)
      base)))

(defun piper--download-models-file (url)
  "Download models file from URL and return path to temp file."
  (let ((temp-file (make-temp-file "piper-models")))
    (piper--log "Fetching models from %s" url)
    (let ((curl-status (call-process "curl" nil nil nil
                                    "--silent" "--fail" "--location"
                                    "--output" temp-file
                                    url)))
      (unless (= curl-status 0)
        (message "Failed to fetch models: curl exited with status %s" curl-status)
        (signal 'file-error (list "Failed to download models file" url))))
    temp-file))

(defun piper--test-line-match (pattern line)
  "Test if PATTERN matches LINE and show match groups."
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (when (looking-at pattern)
      (let ((matches nil)
            (i 0))
        (while (<= i (/ (length (match-data)) 2))
          (when (match-string i)
            (push (cons i (match-string i)) matches))
          (setq i (1+ i)))
        (piper--log "Pattern matched: %S" (nreverse matches))
        t))))

(defun piper--line-indentation ()
  "Get the indentation level of the current line."
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun piper--extract-model-name (url)
  "Extract model name from URL."
  (when (string-match "/\\([^/]+\\)\.onnx" url)
    (match-string 1 url)))

(defun piper--parse-models-file ()
  "Parse the models file to find .onnx files and their configs."
  (let (models)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties 
                   (line-beginning-position) 
                   (line-end-position))))
        ;; Look for .onnx URLs
        (when (string-match "huggingface\.co/[^)]+\.onnx" line)
          (let* ((model-url (match-string 0 line))
                 (full-model-url (concat "https://" model-url "?download=true"))
                 (config-url (concat "https://" model-url ".json?download=true"))
                 (model-name (piper--extract-model-name full-model-url)))
            (when model-name
              (piper--log "Found model: %s" model-name)
              (push (list model-name full-model-url config-url) models))))
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
             (model (piper--extract-model-info model-url)))
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
                  (piper--log "First 500 chars: %s" (substring content 0 (min 500 (length content))))
                  (setq models (piper--find-models-in-buffer))
                  (if (null models)
                      (error "No models found in the file")
                    (progn
                      (piper--log "Setting piper--available-models to %d models" (length models))
                      (setq piper--available-models models)
                      (piper--log "First model: %S" (car models))
                      (piper--log "Total models now available: %d" (length piper--available-models))
                      piper--available-models))))))
      (when (file-exists-p temp-file)
        (piper--log "Cleaned up temp file %s" temp-file)
        (delete-file temp-file))))))

(defun piper--handle-download (_output-file reporter callback)
  "Handle download, update REPORTER, and call CALLBACK when done."
  (when reporter
    (progress-reporter-done reporter))
  (when callback
    (funcall callback)))

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

;;;###autoload
(defun piper-select-model (&optional return-only)
  "Select and switch to a different voice model.
If RETURN-ONLY is non-nil, return the selected model structure
instead of setting it."
  (interactive)
  (unless piper--available-models
    (message "Fetching available models...")
    (piper--fetch-available-models))
  
  ;; Group models by language - use lang-code for consistency
  (let* ((lang-groups (delete-dups
                       (mapcar (lambda (m)
                                (let ((code (piper-model-lang-code m))
                                      (name (piper-model-lang-name m)))
                                  (cons code name)))
                              piper--available-models)))
         ;; First select language
         (selected-lang-pair (completing-read
                              "Select language: "
                              (mapcar (lambda (pair)
                                       (format "%s (%s)" (cdr pair) (car pair)))
                                     lang-groups)
                              nil t))
         ;; Extract the lang code from selection "name (code)"  
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
                                  (cons (format "%s - %s%s"
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
                 selected-model-name
                 (piper-model-lang-name selected-model))))))

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

(defcustom piper-process-timeout 30
  "Timeout in seconds for Piper TTS process."
  :type 'integer
  :group 'piper)

(defvar piper--source-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the piper-mode source files.")

(defvar piper--setup-done nil
  "Flag to track if setup has been completed.")

(defvar piper--current-process nil
  "Current Piper TTS process.")

(defvar piper--play-process nil
  "Current audio playback process.")


(defvar piper--text-offset 0
  "Offset of the current text in the buffer, for positioning highlights.")

(defvar piper--highlight-start-pos nil
  "Explicit start position for highlighting.")

(defvar piper--highlight-end-pos nil
  "Explicit end position for highlighting.")

(defvar piper--phoneme-data nil
  "Phoneme timing data for current audio.")

(defvar piper--current-phoneme-index 0
  "Index of the current phoneme being played.")

(defun piper--load-phoneme-data (wav-file)
  "Load phoneme data for WAV-FILE from the corresponding .pho file."
  (let* ((pho-file (concat (file-name-sans-extension wav-file) ".pho"))
         (phoneme-data nil))
    (when (file-exists-p pho-file)
      (piper--log "Loading phoneme data from %s" pho-file)
      (with-temp-buffer
        (insert-file-contents pho-file)
        (goto-char (point-min))
        ;; Parse phoneme data in format: time phoneme text-pos
        (while (not (eobp))
          (when (looking-at "\\([0-9.]+\\)\\s-+\\(\\S-+\\)\\s-+\\([0-9]+\\)")
            (let ((time (string-to-number (match-string 1)))
                  (phoneme (match-string 2))
                  (text-pos (string-to-number (match-string 3))))
              (push (list time phoneme text-pos) phoneme-data)))
          (forward-line 1)))
      (setq phoneme-data (nreverse phoneme-data))
      (piper--log "Loaded %d phoneme entries" (length phoneme-data)))
    (setq piper--phoneme-data phoneme-data)
    (setq piper--current-phoneme-index 0)))

(defvar piper--mid-sentence nil
  "Whether the current text starts mid-sentence.
Used to adjust highlighting for piper-speak-to-end.")

(defvar piper--current-highlight-overlay nil
  "Current overlay used for TTS highlighting.")

(defvar piper--current-text nil
  "Text currently being spoken.")

(defvar piper--speak-marker nil
  "Marker for the start position of the text being spoken.")

(defvar piper-script-path nil
  "Path to the piper run script.
This is set automatically by `piper--initialize-paths'.")

(defvar piper-temp-dir nil
  "Directory for temporary wav files.
This is set automatically by `piper--initialize-paths'.")

;; Chunking variables
(defvar piper--chunk-queue nil
  "Queue of text chunks waiting to be processed.")

(defvar piper--current-chunk-index 0
  "Index of the current chunk being processed.")

(defvar piper--total-chunks 0
  "Total number of chunks for the current text.")

(defvar piper--original-text nil
  "Original complete text before chunking.")

(defvar piper--original-text-offset 0
  "Original text offset before chunking.")

(defvar piper--chunk-processing nil
  "Whether we're currently processing chunks.")

(defun piper--ensure-script ()
  "Ensure the Piper script is available and executable."
  (let* ((install-dir (piper--get-install-dir))
         (source-dir (if (string-match "/straight/build/" install-dir)
                        (replace-match "/straight/repos/" t t install-dir)
                      install-dir))
         (setup-script (expand-file-name "setup-piper.sh" source-dir))
         (run-script (piper--get-script-path)))
    
    (piper--log "Source dir: %s" source-dir)
    (piper--log "Setup script: %s" setup-script)
    (piper--log "Install dir: %s" install-dir)
    
    (unless (and (file-exists-p run-script)
                 (file-executable-p run-script))
      (error "Piper script not found or not executable at %s" run-script))))

(defun piper--verify-installation ()
  "Verify that all required Piper components are installed and accessible."
  (let* ((install-dir (piper--get-install-dir))
         (bin-dir (expand-file-name "bin" install-dir))
         (piper-bin (expand-file-name "piper" bin-dir))
         (run-script (piper--get-script-path))
         ;; (espeak-data (expand-file-name "espeak-ng-data" bin-dir))
         (onnx-lib (expand-file-name "libonnxruntime.1.14.1.dylib" bin-dir))
         (_espeak-lib (expand-file-name "libespeak-ng.1.dylib" bin-dir))
         (_piper-lib (expand-file-name "libpiper_phonemize.1.dylib" bin-dir)))
    
    (piper--log "Verifying Piper installation:")
    (piper--log "  Install dir: %s" install-dir)
    (piper--log "  Piper binary: %s (exists: %s)" piper-bin (file-exists-p piper-bin))
    (piper--log "  Run script: %s (exists: %s)" run-script (file-exists-p run-script))
    ;; (piper--log "  Espeak data: %s (exists: %s)" espeak-data (file-exists-p espeak-data))
    (piper--log "  ONNX lib: %s (exists: %s)" onnx-lib (file-exists-p onnx-lib))
    
    (unless (file-exists-p piper-bin)
      (error "Piper binary not found at %s" piper-bin))
    (unless (file-exists-p run-script)
      (error "Run script not found at %s" run-script))))

;; Build directory handling


(defun piper--handle-straight-build ()
  "Delete temporary files in straight build directory.
Only runs if we are using the default installation directory within straight."
  (when (and (null piper-install-dir)
             (string-match-p "/straight/build/" (piper--get-install-dir)))
    (piper--log "Handling straight build directory")
    (let ((straight-dir (file-name-directory (piper--get-install-dir))))
      (when (and straight-dir (file-exists-p straight-dir))
        (let ((files (directory-files straight-dir t "^[^\\.]")))
          (dolist (file files)
            (let ((base-name (file-name-nondirectory file)))
              (when (or (string= base-name "bin")
                       (string= base-name "tmp"))
                (if (file-directory-p file)
                    (delete-directory file t)
                  (delete-file file))))))))))

(defcustom piper-development-mode nil
  "When non-nil, optimize the rebuild process for faster development.
This prevents rebuilding native components when only elisp files have changed,
resulting in a much faster feedback loop during development."
  :type 'boolean
  :group 'piper)

(defun piper-toggle-development-mode ()
  "Toggle development mode for faster feedback loops."
  (interactive)
  (setq piper-development-mode (not piper-development-mode))
  (message "Piper development mode %s" (if piper-development-mode "enabled" "disabled")))

(defun piper--elisp-only-changes-p ()
  "Check if only Elisp files have been changed.
  Returns t if only *.el files were modified, nil otherwise.
  This handles both git changes and source vs build differences."
  (let* ((install-dir (piper--get-install-dir))
         (build-dir install-dir)
         (repo-dir (if (string-match "/straight/build/" install-dir)
                     (replace-match "/straight/repos/" t t install-dir)
                   install-dir))
         (source-repo-dir (file-truename "/Users/taras/repos/piper-mode"))
         ;; Check if pull updated any non-elisp files
         (default-directory repo-dir)
         (git-changes-cmd "git diff --name-only HEAD@{1} HEAD 2>/dev/null || echo ''")
         (changed-files (shell-command-to-string git-changes-cmd))
         (has-non-elisp-changes nil))
    
    ;; Check git changes
    (with-temp-buffer
      (insert changed-files)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when (and (not (string-empty-p line)) 
                     (not (string-match-p "\\.el$" line)))
            (setq has-non-elisp-changes t))
          (forward-line 1))))
    
    ;; For straight-pull, check if the source repo has non-elisp changes compared to build
    (when (and (not has-non-elisp-changes) (not (equal source-repo-dir repo-dir)))
      ;; Check if there are changes in setup-piper.sh
      (let ((source-setup (expand-file-name "setup-piper.sh" source-repo-dir))
            (build-setup (expand-file-name "setup-piper.sh" build-dir)))
        (when (and (file-exists-p source-setup) (file-exists-p build-setup))
          (setq has-non-elisp-changes 
                (not (string= 
                      (with-temp-buffer 
                        (insert-file-contents source-setup) 
                        (buffer-string))
                      (with-temp-buffer 
                        (insert-file-contents build-setup) 
                        (buffer-string)))))))
      ;; Check for changes in bin directory if necessary
      (when (not has-non-elisp-changes)
        (let ((source-bin (expand-file-name "bin" source-repo-dir))
              (build-bin (expand-file-name "bin" build-dir)))
          (when (and (file-directory-p source-bin) (file-directory-p build-bin))
            (let ((source-files (directory-files source-bin))
                  (build-files (directory-files build-bin)))
              (setq has-non-elisp-changes (not (equal source-files build-files))))))))

    (when piper-debug
      (message "[piper] Non-elisp changes detected: %s" has-non-elisp-changes))

    (not has-non-elisp-changes)))

(defun piper--before-straight-rebuild (orig-fun &rest args)
  "Advice to run before straight-rebuild-package for piper-mode.
  When in development mode and only Elisp files have changed,
  this will avoid rebuilding native components."
  (when (equal (car args) "piper-mode")
    (if (and piper-development-mode (piper--elisp-only-changes-p))
        (progn
          (when piper-debug
            (message "[piper] Development mode: Skipping native component rebuild"))
          ;; Still need to load the Elisp files
          (let ((default-directory (file-name-directory (locate-library "piper-mode"))))
            (load (expand-file-name "piper-mode.el") nil t))
          ;; Skip the original function by returning a dummy value
          'development-mode-skip)
      ;; Not in development mode or non-elisp changes detected
      (when piper-debug
        (message "[piper] Full rebuild: %s" 
                 (if piper-development-mode 
                     "Non-elisp changes detected" 
                     "Development mode disabled")))
      (piper--handle-straight-build)
      (apply orig-fun args))))

(advice-add 'straight-rebuild-package :around #'piper--before-straight-rebuild)

;; Process management functions
(defun piper--start-process (input-text output-file model-file callback)
  "Start the Piper TTS process to convert INPUT-TEXT to speech.
Uses MODEL-FILE to generate audio in OUTPUT-FILE.
Calls CALLBACK with the process status when done."
  (let* ((default-directory (piper--get-install-dir))
         (run-script (piper--get-script-path))
         (process-name "piper-tts")
         (process-buffer (get-buffer-create "*piper-process*"))
         ;; Convert model path to be relative to install directory
         (relative-model-file (file-relative-name model-file default-directory))
         (process))
    
    ;; Clear the process buffer
    (with-current-buffer process-buffer
      (erase-buffer))
    
    (condition-case err
        (progn
          (piper--log "Starting Piper process...")
          (piper--log "  Text: %s" input-text)
          (piper--log "  Output: %s" output-file)
          (piper--log "  Model: %s" model-file)
          (piper--log "  Relative model: %s" relative-model-file)
          (piper--log "  Script: %s" run-script)
          (piper--log "  Directory: %s" default-directory)
          
          ;; Verify files exist
          (unless (file-exists-p run-script)
            (error "Run script not found: %s" run-script))
          (unless (file-exists-p model-file)
            (error "Model file not found: %s" model-file))
          
          ;; Save the text for later highlighting
          (setq piper--current-text input-text)
          
          ;; Start the process
          (setq process
                (make-process
                 :name process-name
                 :buffer process-buffer
                 :command (list run-script
                              "--model" relative-model-file
                              "--output_file" output-file)
                 :sentinel (lambda (proc event)
                           (piper--log "Process event: %s" (string-trim event))
                           (piper--log "Process output:\n%s" 
                                    (with-current-buffer (process-buffer proc)
                                      (buffer-string)))
                           (when (string-match-p "finished" event)
                             (funcall callback (process-exit-status proc))))))
          
          ;; Save the process for later
          (setq piper--current-process process)
          
          ;; Send the text to the process
          (process-send-string process (concat input-text "\n"))
          (process-send-eof process)
          
          ;; Set process timeout
          (run-with-timer piper-process-timeout nil
                         (lambda ()
                           (when (process-live-p process)
                             (piper--log "Process timeout reached")
                             (delete-process process)
                             (funcall callback 1)))))
      
      (error
       (piper--log "Failed to start process: %s" (error-message-string err))
       (funcall callback 1)))))

(defun piper--play-wav-file (file-path)
  "Play the WAV file at FILE-PATH using tts-play-with-timing.
This enables synchronized text highlighting during audio playback."
  (piper--log "[DEBUG] Playing WAV file with timing: %s" file-path)
  (piper--log "[DEBUG] WAV file exists: %s" (if (file-exists-p file-path) "YES" "NO"))
  (when (file-exists-p file-path)
    (piper--log "[DEBUG] WAV file size: %d bytes" (file-attribute-size (file-attributes file-path))))
  
  ;; Set up highlighting if mode is enabled
  (when piper-highlight-mode
    (piper--log "Highlight mode is enabled: %s" piper-highlight-mode)
    
    ;; Log highlight positions before setup
    (piper--log "BEFORE PLAYBACK SETUP: Highlight positions: start=%s, end=%s" 
               (if (numberp piper--highlight-start-pos) 
                   (format "%d" piper--highlight-start-pos) "nil")
               (if (numberp piper--highlight-end-pos) 
                   (format "%d" piper--highlight-end-pos) "nil"))
    
    ;; Load phoneme data
    (piper--load-phoneme-data file-path)
    (piper--log "Loaded %d phoneme entries for highlighting" (length piper--phoneme-data))
    
    ;; Clear overlays but preserve positions
    (piper--clear-highlights t)
    
    ;; Log again after setup
    (piper--log "AFTER PLAYBACK SETUP: Highlight positions: start=%s, end=%s" 
               (if (numberp piper--highlight-start-pos) 
                   (format "%d" piper--highlight-start-pos) "nil")
               (if (numberp piper--highlight-end-pos) 
                   (format "%d" piper--highlight-end-pos) "nil")))
  
  ;; Create the process for playback with timing
  (let ((process-buffer (get-buffer-create "*piper-play*")))
    ;; Clear the buffer before starting
    (with-current-buffer process-buffer
      (erase-buffer))
    
    (setq piper--play-process
          (make-process
           :name "piper-play"
           :buffer process-buffer
           :command (list (expand-file-name "bin/tts-play-with-timing" 
                                           (piper--get-install-dir))
                         file-path)
           :filter (lambda (proc output)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-max))
                      (insert output))
                    (piper--handle-timing-output output))
           :sentinel (lambda (_proc event)
                      (piper--log "Play process event: %s" event)
                      (when (string-match-p "\\(finished\\|exited\\)" event)
                        (piper--clear-highlights))))))
  
  (piper--log "Started playback process: %s" piper--play-process)
  
  ;; Add direct afplay fallback if the primary process fails
  (run-with-timer 1 nil
                 (lambda ()
                   (when (and piper--play-process
                              (not (process-live-p piper--play-process)))
                     (piper--log "[DEBUG] Primary play process failed, trying direct afplay")
                     (when (file-exists-p file-path)
                       (piper--log "[DEBUG] Using direct afplay on: %s" file-path)
                       (start-process "piper-afplay-fallback" nil "afplay" file-path))))))








(defun piper--find-phoneme-position-at-time (current-time)
  "Find the text position corresponding to CURRENT-TIME in phoneme data."
  (let ((position nil)
        (found-phoneme nil)
        (i 0)
        (phonemes piper--phoneme-data))
    ;; Find the last phoneme with a time less than or equal to current-time
    (while (and phonemes (not found-phoneme))
      (let* ((phoneme (car phonemes))
             (phoneme-time (nth 0 phoneme))
             (phoneme-pos (nth 2 phoneme)))
        (if (<= phoneme-time current-time)
            (progn
              (setq position phoneme-pos)
              (setq i (1+ i))
              (setq phonemes (cdr phonemes)))
          (setq found-phoneme t))))
    
    (when position
      (setq piper--current-phoneme-index i))
    position))

(defun piper--handle-timing-output (output)
  "Handle timing OUTPUT from the playback process to update highlighting."
  (when piper-debug
    (piper--log "Timing output: %s" output))
  
  (cond
   ;; Handle debug output
   ((string-match "DEBUG \\(.*\\)" output)
    (piper--log "Timing debug: %s" (match-string 1 output)))
   
   ;; Handle time updates - these drive the highlighting
   ((string-match "TIME \\([0-9.]+\\)" output)
    (let* ((current-time (string-to-number (match-string 1 output))))
      
          ;; Using phoneme-based highlighting only
      (piper--log "Using phoneme-based highlighting at time %.2fs" current-time)
      (let ((position (piper--find-phoneme-position-at-time current-time)))
        (when position
          (piper--log "Found phoneme at time %.2f: position %d" current-time position)
          (piper--update-highlight-from-phoneme position)))))))

(defun piper--update-highlight-from-phoneme (position)
  "Update the highlight overlay based on POSITION from phoneme data."
  (piper--clear-highlights t)
  (when (and position
             (buffer-live-p (marker-buffer piper--speak-marker)))
    (let* ((buffer (marker-buffer piper--speak-marker))
           (base-pos (if (and piper--speak-marker (marker-buffer piper--speak-marker))
                        (marker-position piper--speak-marker)
                      (point-min)))
           (_text-offset (if piper--chunk-processing
                          (piper--calculate-chunk-offset)
                        piper--text-offset))
           ;; Calculate actual start and end positions
           (start-pos (+ base-pos position))
           ;; Try to find next phoneme for end position
           (end-pos (if (and piper--phoneme-data 
                           (< (1+ piper--current-phoneme-index) (length piper--phoneme-data)))
                       (+ base-pos (nth 2 (nth (1+ piper--current-phoneme-index) piper--phoneme-data)))
                     ;; Fallback: add a few characters
                     (+ start-pos 15))))
      
      ;; Extend to sentence boundary for better readability
      (when piper--current-text
        (setq end-pos (piper--extend-to-sentence-boundary piper--current-text 
                                                        (- end-pos base-pos))))
      (setq end-pos (+ base-pos end-pos))
      
      ;; Ensure we have a visible overlay (at least 1 character)
      (when (<= end-pos start-pos)
        (setq end-pos (1+ start-pos)))
      
      (piper--log "Creating phoneme highlight in %s from %d to %d" 
                 (buffer-name buffer) start-pos end-pos)
      
      (with-current-buffer buffer
        ;; Ensure positions are within buffer boundaries
        (let* ((valid-start (max (point-min) (min start-pos (point-max))))
               (valid-end (max (point-min) (min end-pos (point-max)))))
          
          ;; Only create overlay if we have valid positions
          (when (< valid-start valid-end)
            ;; Create the overlay with a high priority to ensure visibility
            (let ((overlay (make-overlay valid-start valid-end buffer)))
              (overlay-put overlay 'face 'piper-highlight-face)
              (overlay-put overlay 'priority 1000) ;; Higher priority
              (overlay-put overlay 'piper-highlight t)
              (setq piper--current-highlight-overlay overlay))))))
  
  ;; Report if highlight creation failed
  (unless piper--current-highlight-overlay
    (piper--log "Warning: Failed to create phoneme highlight overlay"))))


(defun piper--clear-highlights (&optional preserve-positions)
  "Clear all TTS highlighting overlays.
If PRESERVE-POSITIONS is non-nil, don't reset highlight positions."
  (piper--log "Clearing highlight overlays (preserve positions: %s)" preserve-positions)
  (when piper--current-highlight-overlay
    (delete-overlay piper--current-highlight-overlay)
    (setq piper--current-highlight-overlay nil))
  
  ;; Reset explicit highlight positions only if not preserving
  (unless preserve-positions
    (setq piper--highlight-start-pos nil)
    (setq piper--highlight-end-pos nil)))

(defun piper--get-wav-duration (wav-file)
  "Get the duration in seconds of WAV-FILE."
  (piper--log "Getting duration for: %s" wav-file)
  (if (and wav-file (file-exists-p wav-file))
      (let ((output (shell-command-to-string
                     (format "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 %s 2>/dev/null"
                             (shell-quote-argument wav-file)))))
        (if (string-match-p "[0-9]" output)
            (let ((duration (string-to-number (string-trim output))))
              (piper--log "Extracted duration: %.2f seconds" duration)
              duration)
          (piper--log "Could not extract duration, falling back to default")
          10.0)) ; default duration if extraction fails
    (piper--log "Invalid or missing wav file")
    10.0)) ; default duration if no file

(defun piper--stop ()
  "Stop any currently running TTS process."
  (when piper--current-process
    (when (process-live-p piper--current-process)
      (piper--log "Killing piper process")
      (kill-process piper--current-process))
    (setq piper--current-process nil))
  (when piper--play-process
    (when (process-live-p piper--play-process)
      (piper--log "Killing play process")
      (kill-process piper--play-process))
    (setq piper--play-process nil))
  ;; Store current highlight positions before clearing
  (let ((saved-highlight-start piper--highlight-start-pos)
        (saved-highlight-end piper--highlight-end-pos))
    
    ;; Clear highlights and reset state
    (piper--clear-highlights t) ;; Clear overlays but preserve positions for now

    (setq piper--current-text nil)
    (setq piper--text-offset 0)
    
    ;; Restore highlight positions for next playback
    (when saved-highlight-start
      (setq piper--highlight-start-pos saved-highlight-start)
      (setq piper--highlight-end-pos saved-highlight-end)
      (piper--log "PRESERVED highlight positions in stop: %d to %d" 
                 piper--highlight-start-pos piper--highlight-end-pos))))

(defun piper--create-temp-wav ()
  "Create a temporary WAV file and return its path."
  (let* ((tmp-dir (piper--get-temp-dir))
         (wav-file (make-temp-file (expand-file-name "piper-" tmp-dir) nil ".wav")))
    (piper--log "Created temp WAV file: %s" wav-file)
    wav-file))

(defun piper--cleanup-temp-wav (wav-file)
  "Clean up temporary WAV file WAV-FILE."
  (when (and wav-file (file-exists-p wav-file))
    (piper--log "Deleting temp WAV file: %s" wav-file)
    (delete-file wav-file)))

(defun piper--play-wav (wav-file)
  "Play WAV file WAV-FILE using afplay."
  (piper--log "Starting audio playback of %s" wav-file)
  (when (file-exists-p wav-file)
    (let ((size (file-attribute-size (file-attributes wav-file))))
      (piper--log "WAV file size: %d bytes" size)
      (if (> size 0)
          (progn
            (setq piper--play-process
                  (start-process "piper-play" "*piper-play*" "afplay" wav-file))
            (set-process-sentinel 
             piper--play-process
             (lambda (_proc event)
               (piper--log "Audio playback process ended with: %s" event)
               (when (string= event "finished\n")
                 (piper--log "Audio playback completed successfully")
                 (piper--cleanup-temp-wav wav-file)))))
        (piper--log "WAV file is empty, skipping playback")
        (piper--cleanup-temp-wav wav-file)))))

;; Main interface functions
;;;###autoload
(defun piper-speak (text &optional no-reset-offset)
  "Speak TEXT using Piper TTS.
If NO-RESET-OFFSET is non-nil, don't reset the text offset.
Large texts are automatically chunked for faster playback startup."
  (interactive "MText to speak: ")
  (piper--log "Speaking text: %s" text)
  
  ;; Ensure setup is done
  (piper--ensure-setup)
  
  ;; Stop any current speech - piper--stop now preserves highlight positions
  (piper--stop)
  
  ;; Only create marker if it doesn't exist
  ;; Do NOT reset its position - we want to preserve the position set by the calling function
  (unless piper--speak-marker
    (setq piper--speak-marker (set-marker (make-marker) (point))))
  
  ;; Debug marker in speak function
  (piper--log "DEBUG PIPER-SPEAK: Using marker at position %d in buffer %s" 
             (marker-position piper--speak-marker) 
             (buffer-name (marker-buffer piper--speak-marker)))
  
  ;; Reset text offset for absolute positioning, unless asked not to
  (unless no-reset-offset
    (setq piper--text-offset 0)
    (setq piper--mid-sentence nil))
  
  ;; Save the original text and highlight positions (for highlighting purposes)
  (setq piper--original-text text)
  (setq piper--original-text-offset piper--text-offset)
  
  ;; Debug statement to check if highlight positions are set
  (piper--log "DEBUG HIGHLIGHT POSITIONS: start=%s, end=%s" 
             (if (numberp piper--highlight-start-pos) 
                 (format "%d" piper--highlight-start-pos) "nil")
             (if (numberp piper--highlight-end-pos)
                 (format "%d" piper--highlight-end-pos) "nil"))
  
  ;; Force positions to be numbers if they exist
  (when piper--highlight-start-pos
    (setq piper--highlight-start-pos (string-to-number (format "%d" piper--highlight-start-pos))))
  (when piper--highlight-end-pos
    (setq piper--highlight-end-pos (string-to-number (format "%d" piper--highlight-end-pos))))
  
  ;; Check if we need to chunk the text
  (let ((chunks (piper--chunk-text text)))
    (if (= (length chunks) 1)
        ;; Single chunk - process normally
        (progn
          (setq piper--current-text text)
          (let ((wav-file (piper--create-temp-wav)))
            (piper--start-process text wav-file (piper--expand-model-path piper-voice-model)
                                 (lambda (status)
                                   (piper--log "Process completed with status: %s" status)
                                   (if (eq status 0)
                                       (progn
                                         (piper--log "Playing WAV file: %s" wav-file)
                                         (piper--play-wav-file wav-file))
                                     (progn
                                       (piper--log "Failed to generate speech, cleaning up")
                                       (piper--cleanup-temp-wav wav-file)
                                       (message "Failed to generate speech: %s" status)))))))
      
      ;; Multiple chunks - process sequentially
      (progn
        (piper--log "Processing text in %d chunks" (length chunks))
        
        ;; Set up chunking variables
        (setq piper--chunk-queue (cdr chunks))
        (setq piper--current-chunk-index 0)
        (setq piper--total-chunks (length chunks))
        (setq piper--chunk-processing t)
        
        ;; Process the first chunk immediately
        (let ((first-chunk (car chunks)))
          (setq piper--current-text first-chunk)
          (piper-speak-chunk first-chunk))))))

;;;###autoload
(defun piper-speak-region (start end)
  "Speak the region from START to END using Piper TTS."
  (interactive "r")
  (piper--log "Speaking region from %d to %d" start end)
  (let* ((text (buffer-substring-no-properties start end))
         (cursor-pos (point)))
    
    ;; EXPLICITLY set highlight positions for the region - this is critical
    (setq piper--highlight-start-pos start)
    (setq piper--highlight-end-pos end)
    (piper--log "EXPLICIT highlight positions for region: start=%d, end=%d" 
               piper--highlight-start-pos piper--highlight-end-pos)
    
    ;; Set marker at cursor position
    (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) cursor-pos))
    
    ;; Set text offset to 0 since we're using explicit positions now
    (setq piper--text-offset 0)
    (piper--log "Setting speak-marker to cursor pos %d for speak-region" cursor-pos)
    
    (piper-speak text t)))

;;;###autoload
(defun piper-speak-buffer ()
  "Speak the entire buffer using Piper TTS."
  (interactive)
  (piper--log "Speaking entire buffer")
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    ;; Set marker at start of buffer for absolute positioning
    (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) (point-min)))
    ;; Reset text offset since we're speaking from the beginning
    (setq piper--text-offset 0)
    (piper--log "Resetting text offset to 0 for speak-buffer")
    (piper-speak text)))

;;;###autoload
(defun piper-speak-paragraph ()
  "Speak the current paragraph using Piper TTS."
  (interactive)
  (piper--log "Speaking paragraph")
  (let ((cursor-pos (point)))
    (save-excursion
      (let* ((start (progn (backward-paragraph) (point)))
             (end (progn (forward-paragraph) (point)))
             (text (buffer-substring-no-properties start end)))
        ;; Set marker at cursor position
        (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) cursor-pos))
        ;; Calculate text offset as the difference between paragraph start and cursor
        (setq piper--text-offset (- start cursor-pos))
        (piper--log "Setting speak-marker to cursor pos %d with text-offset %d for speak-paragraph" 
                   cursor-pos piper--text-offset)
        (piper-speak text t)))))

;;;###autoload
(defun piper-speak-line ()
  "Speak the current line using Piper TTS."
  (interactive)
  (piper--log "Speaking line")
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (text (buffer-substring-no-properties line-start line-end))
         (cursor-pos (point))
         (line-num (line-number-at-pos)))
    
    (piper--log "SUPER DEBUG: Line %d from pos %d to %d, cursor at %d, text length: %d" 
               line-num line-start line-end cursor-pos (length text))
    
    ;; SIMPLIFY OUR APPROACH:
    ;; 1. Set the explicit highlight start position as a property that highlight function can use
    ;; 2. No longer use text offset calculations which are confusing
    (setq piper--highlight-start-pos line-start)
    (setq piper--highlight-end-pos line-end)
    
    ;; Set marker to current cursor position (useful for other functions)
    (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) cursor-pos))
    
    ;; Set text offset to 0 - no longer using this for calculations
    (setq piper--text-offset 0)
    
    (piper--log "SIMPLIFIED: Setting explicit highlight positions: start=%d, end=%d" 
               piper--highlight-start-pos piper--highlight-end-pos)
    
    (piper-speak text t)))

;;;###autoload
(defun piper-speak-word ()
  "Speak the current word using Piper TTS."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (if bounds (car bounds) (point)))
         (end (if bounds (cdr bounds) (point)))
         (text (buffer-substring-no-properties start end))
         (cursor-pos (point)))
    ;; Set marker at cursor position
    (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) cursor-pos))
    ;; Calculate text offset as the difference between word start and cursor
    (setq piper--text-offset (- start cursor-pos))
    (piper--log "Setting speak-marker to cursor pos %d with text-offset %d for speak-word" 
               cursor-pos piper--text-offset)
    (piper-speak text t)))

;;;###autoload
(defun piper-stop ()
  "Stop speaking and clean up."
  (interactive)
  (piper--log "Stopping piper")
  ;; Clean up our processes and resources
  (piper--cleanup))

;;;###autoload
(defun piper-speak-to-end ()
  "Speak text from point to the end of buffer using Piper TTS."
  (interactive)
  (piper--log "Speaking from point to end of buffer")
  (let* ((start (point))
         (text (buffer-substring-no-properties start (point-max)))
         (is-mid-sentence nil))
    
    ;; Determine if we're in the middle of a sentence
    (save-excursion
      ;; If we're not at the beginning of buffer or beginning of line
      ;; assume we're mid-sentence for highlighting purposes
      (when (and (not (bobp)) (not (bolp)))
        (setq is-mid-sentence t)))
    
    ;; Set marker at the CURRENT POINT instead of beginning of buffer
    (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) start))
    
    ;; Set text offset to 0 since we're using current point as the reference
    (setq piper--text-offset 0)
    
    ;; Store whether we're in mid-sentence as a text property
    (setq piper--mid-sentence is-mid-sentence)
    
    (piper--log "Setting speak-marker to current point %d for speak-to-end (mid-sentence: %s)" 
               start is-mid-sentence)
    (piper-speak text t)))

;;;###autoload
(define-minor-mode piper-mode
  "Toggle Piper TTS mode.
When Piper mode is enabled, you can use various commands to have text read
aloud."
  :init-value nil
  :lighter " Piper"
  :group 'piper
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (when piper-mode
    (piper--initialize-paths)))

;; Initialize paths during load, but don't run setup yet
(when (and (not noninteractive)
           (not load-file-name))  ; Only run when actually loading the package
  (piper--initialize-paths))

;; Debug function for testing audio playback issues
(defun piper-test-audio ()
  "Test function to directly play audio using both methods for troubleshooting."
  (interactive)
  (let ((test-buffer (get-buffer-create "*piper-audio-test*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "Audio Playback Test\n==================\n\n")
      
      ;; Find the most recent WAV file
      (let ((recent-wavs nil))
        (dolist (file (directory-files temporary-file-directory t "\\.wav$"))
          (when (file-exists-p file)
            (push file recent-wavs)))
        (setq recent-wavs (sort recent-wavs
                               (lambda (a b)
                                 (time-less-p
                                  (nth 5 (file-attributes b))
                                  (nth 5 (file-attributes a))))))
        
        (if recent-wavs
            (let ((test-wav (car recent-wavs)))
              (insert (format "Using WAV file: %s\n" test-wav))
              (insert (format "File exists: %s\n" (if (file-exists-p test-wav) "YES" "NO")))
              (insert (format "File size: %d bytes\n\n" 
                            (file-attribute-size (file-attributes test-wav))))
              
              ;; Test method 1: Using direct afplay command
              (insert "TEST 1: Using direct 'afplay' command\n")
              (let ((afplay-process 
                     (start-process "afplay-test" nil "afplay" test-wav)))
                (insert (format "Process started: %s\n\n" afplay-process)))
              
              ;; Test method 2: Using the custom script
              (let ((play-script (expand-file-name "bin/tts-play-with-timing" 
                                                 (piper--get-install-dir))))
                (insert "TEST 2: Using tts-play-with-timing script\n")
                (insert (format "Script path: %s\n" play-script))
                (insert (format "Script exists: %s\n" 
                              (if (file-exists-p play-script) "YES" "NO")))
                (insert (format "Script executable: %s\n" 
                              (if (file-executable-p play-script) "YES" "NO")))
                
                (when (and (file-exists-p play-script) 
                           (file-executable-p play-script))
                  (let ((script-process 
                         (start-process "script-test" 
                                       (get-buffer-create "*piper-script-test*") 
                                       play-script test-wav)))
                    (insert (format "Process started: %s\n\n" script-process)))
                  
                  ;; Display script contents for reference
                  (insert "Script contents:\n---------------\n")
                  (condition-case err
                      (insert-file-contents play-script)
                    (error 
                     (insert (format "Error reading script: %s" err)))))))
          (insert "No WAV files found for testing.\n")))
      
      (display-buffer test-buffer))))

(provide 'piper-mode)

;;; piper-mode.el ends here

(defun piper--run-tts (text &optional no-block)
  "Run Piper TTS on TEXT.
If NO-BLOCK is non-nil, run asynchronously."
  (let* ((script-path (piper--get-script-path))
         (temp-dir (piper--get-temp-dir))
         (temp-wav (expand-file-name (format "piper-%d.wav" (random)) temp-dir))
         (process-environment (append process-environment
                                    (list (format "PIPER_NO_BLOCK=%s" (if no-block "1" ""))))))
    (piper--log "Running TTS with script: %s" script-path)
    (piper--log "Output WAV: %s" temp-wav)
    
    (with-temp-buffer
      (insert text)
      (let ((exit-code (call-process-region (point-min) (point-max)
                                          script-path nil t nil
                                          "--output_file" temp-wav)))
        (unless (= exit-code 0)
          (error "Piper TTS failed with exit code %d: %s" exit-code (buffer-string)))))
    temp-wav))

(defconst piper-default-model "en_US-joe-medium.onnx"
  "Default voice model filename.")

(defun piper--ensure-setup ()
  "Ensure Piper is properly set up by running the setup script if needed."
  (unless piper--setup-done
    (let* ((source-dir (directory-file-name piper--source-dir))
           (setup-script (expand-file-name "setup-piper.sh" source-dir))
           (install-dir (piper--get-install-dir))
           (bin-dir (expand-file-name "bin" install-dir))
           (piper-bin (expand-file-name "piper" bin-dir)))
      
      ;; Only run setup if the installation is not already valid
      (when (and piper-auto-setup
                 (file-exists-p setup-script)
                 (or (not (file-exists-p piper-bin))
                     (not (file-executable-p piper-bin))))
        (message "Setting up Piper TTS in %s..." install-dir)
        ;; Clean and recreate install directory
        (piper--handle-straight-build)
        (make-directory install-dir t)
        (let ((default-directory source-dir))
          (message "Running setup script from %s" default-directory)
          (let ((result (call-process "bash" nil "*piper-setup*" t setup-script install-dir)))
            (unless (= result 0)
              (error "Setup script failed with code %d. See *piper-setup* buffer for details" result))
            (message "Setup script exited with code %d" result)))
        (message "Piper TTS setup complete!"))
      
      ;; Verify installation once and set setup-done flag
      (piper--verify-installation)
      ;; Model must be explicitly configured in user's init.el
      ;; (piper--ensure-model)
      (setq piper--setup-done t))))



(defun piper-update ()
  "Force update of Piper TTS binaries and dependencies."
  (interactive)
  (let* ((source-dir (directory-file-name piper--source-dir))
         (setup-script (expand-file-name "setup-piper.sh" source-dir))
         (install-dir (piper--get-install-dir)))
    
    (unless (file-exists-p setup-script)
      (error "Setup script not found at %s" setup-script))
      
    (message "Updating Piper TTS in %s..." install-dir)
    (let ((default-directory source-dir))
      (let ((result (call-process "bash" nil "*piper-setup*" t setup-script "--update" install-dir)))
        (if (= result 0)
            (message "Piper TTS update complete!")
          (error "Update failed with code %d. See *piper-setup* buffer" result))))))

(defun piper--chunk-text (text)
  "Split TEXT into manageable chunks for more responsive playback.
Returns a list of chunks, or a list with just the original text if small enough."
  (if (<= (length text) piper--chunk-size)
      ;; If text is smaller than chunk size, return the whole text
      (list text)
    (let ((chunks nil)
          (overlap piper--chunk-overlap)
          (chunk-start 0)
          (text-length (length text)))
      (piper--log "Chunking text of length %d with chunk size %d and overlap %d"
                 text-length piper--chunk-size overlap)
      
      ;; Process all chunks except the last one
      (while (< (+ chunk-start piper--chunk-size) text-length)
        (let* ((chunk-end (+ chunk-start piper--chunk-size))
               ;; Try to find a good boundary (sentence or paragraph end)
               (boundary (piper--find-chunk-boundary text chunk-end)))
          (push (substring text chunk-start boundary) chunks)
          (setq chunk-start (- boundary overlap))))
      
      ;; Add the last chunk
      (when (< chunk-start text-length)
        (push (substring text chunk-start) chunks))
      
      ;; Return chunks in the correct order
      (nreverse chunks))))

(defun piper--find-chunk-boundary (text position)
  "Find a good boundary in TEXT near POSITION.
Looks for sentence or paragraph ends to avoid mid-sentence cuts."
  (let ((max-lookbehind 200)  ; Don't look too far back
        (orig-position position)
        (text-length (length text))
        (result nil))
    
    ;; Don't go beyond text length
    (when (>= position text-length)
      (setq position (1- text-length)))
    
    ;; First try paragraph boundary
    (let ((paragraph-end (piper--find-paragraph-end text position)))
      (when paragraph-end
        (piper--log "Found paragraph end at %d" paragraph-end)
        (setq result paragraph-end)))
    
    ;; Then try sentence boundary if we didn't find a paragraph end
    (when (not result)
      (let ((sentence-end (piper--find-sentence-end text position max-lookbehind)))
        (when sentence-end
          (piper--log "Found sentence end at %d" sentence-end)
          (setq result sentence-end))))
    
    ;; Fallback to the original position if we couldn't find a better boundary
    (or result
        (progn
          (piper--log "No natural boundary found, using position %d" orig-position)
          orig-position))))

(defun piper--find-paragraph-end (text position)
  "Find paragraph end in TEXT near POSITION."
  (let* ((lookbehind-size 150)
         (min-pos (max 0 (- position lookbehind-size)))
         (text-length (length text)))
    
    ;; Search backwards for a double newline
    (save-match-data
      (let ((substr (substring text min-pos (min text-length position))))
        (if (string-match "\\n\\n" substr)
            (+ min-pos (match-end 0))
          nil)))))

(defun piper--find-sentence-end (text position max-lookbehind)
  "Find sentence end in TEXT near POSITION.
Looks back at most MAX-LOOKBEHIND characters."
  (let ((min-pos (max 0 (- position max-lookbehind)))
        (text-length (length text)))
    
    ;; Search backwards for sentence end followed by space
    (save-match-data
      (let ((substr (substring text min-pos (min text-length position))))
        (if (string-match "[.!?]\s-" substr)
            (+ min-pos (match-end 0))
          nil)))))

(defun piper--extend-to-sentence-boundary (text pos)
  "Extend the highlight to the next sentence boundary from POS in TEXT.
Searches forward for sentence-ending punctuation followed by whitespace.
Looks forward up to 200 characters maximum.
If found, returns the new position; otherwise, returns POS."
  (let ((search-end (min (length text) (+ pos 200))))
    (if (string-match "[.!?]\\s-" text pos)
        (let ((new-pos (match-end 0)))
          (if (<= new-pos search-end)
              new-pos
            pos))
      pos)))

(defun piper--process-next-chunk ()
  "Process the next chunk in the queue."
  (when (and piper--chunk-processing
             piper--chunk-queue
             (not (process-live-p piper--current-process)))
    (let ((next-chunk (pop piper--chunk-queue)))
      (if next-chunk
          (progn
            (cl-incf piper--current-chunk-index)
            (piper--log "Processing chunk %d/%d (length: %d)"
                       piper--current-chunk-index piper--total-chunks
                       (length next-chunk))
            (piper-speak-chunk next-chunk))
        ;; No more chunks, we're done
        (piper--log "All chunks processed")
        (setq piper--chunk-processing nil)
        (piper--cleanup)))))

(defun piper-speak-chunk (text)
  "Speak a single TEXT chunk without resetting state.
This is an internal function used by the chunking system."
  (piper--log "Speaking chunk of length %d" (length text))
  
  ;; Save the text for highlighting
  (setq piper--current-text text)
  
  (let ((wav-file (piper--create-temp-wav)))
    (piper--start-process text wav-file (piper--expand-model-path piper-voice-model)
                          (lambda (status)
                            (piper--log "Chunk process completed with status: %s" status)
                            (if (eq status 0)
                                (progn
                                  (piper--log "Playing chunk WAV file: %s" wav-file)
                                  (piper--play-chunk-wav-file wav-file))
                              (progn
                                (piper--log "Failed to generate speech for chunk, cleaning up")
                                (piper--cleanup-temp-wav wav-file)
                                (message "Failed to generate speech: %s" status)))))))

(defun piper--calculate-chunk-offset ()
  "Calculate the effective text offset when using chunking."
  (let ((base-offset piper--original-text-offset)
        (chunk-offset (* piper--current-chunk-index 
                         (- piper--chunk-size piper--chunk-overlap))))
    (piper--log "Calculated chunk offset: base=%d, chunk=%d, total=%d" 
               base-offset chunk-offset (+ base-offset chunk-offset))
    (+ base-offset chunk-offset)))

(defun piper--play-chunk-wav-file (file-path)
  "Play the chunk WAV file at FILE-PATH with timing info for highlighting."
  (piper--log "Playing chunk WAV file with timing: %s" file-path)
  
  ;; Set up highlighting if mode is enabled
  (when piper-highlight-mode
    (piper--log "Highlight mode is enabled: %s" piper-highlight-mode)
    (piper--load-phoneme-data file-path)
    ;; Clear overlays but preserve highlight positions
    (piper--clear-highlights t))
  
  ;; Create the process for playback with timing
  (let ((process-buffer (get-buffer-create "*piper-play*")))
    ;; Clear the buffer before starting
    (with-current-buffer process-buffer
      (erase-buffer))
    
    (setq piper--play-process
          (make-process
           :name "piper-play"
           :buffer process-buffer
           :command (list (expand-file-name "bin/tts-play-with-timing" 
                                           (piper--get-install-dir))
                         file-path)
           :filter (lambda (proc output)
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-max))
                      (insert output))
                    (piper--handle-timing-output output))
           :sentinel (lambda (_proc event)
                      (piper--log "Play process event: %s" event)
                      (when (string-match-p "\\(finished\\|exited\\)" event)
                        (piper--clear-highlights)
                        ;; Process next chunk if we're in chunking mode
                        (when piper--chunk-processing
                          (piper--process-next-chunk)))))))
  
  (piper--log "Started chunk playback process: %s" piper--play-process))

(defun piper--cleanup ()
  "Clean up all resources."
  (piper--log "Cleaning up resources")
  (when piper--current-process
    (when (process-live-p piper--current-process)
      (piper--log "Killing piper process")
      (kill-process piper--current-process))
    (setq piper--current-process nil))
  (when piper--play-process
    (when (process-live-p piper--play-process)
      (piper--log "Killing play process")
      (kill-process piper--play-process))
    (setq piper--play-process nil))
  (piper--clear-highlights)
  (setq piper--current-text nil)
  (setq piper--text-offset 0)
  (setq piper--chunk-queue nil)
  (setq piper--chunk-processing nil)
  (setq piper--current-chunk-index 0)
  (setq piper--total-chunks 0)
  (setq piper--original-text nil)
  (setq piper--original-text-offset 0))





(defun piper-force-highlight ()
  "Force the creation of highlighting overlay for testing.
This function will highlight the current line without TTS playback.
Useful for debugging highlighting issues."
  (interactive)
  (piper--log "Manually testing highlight overlay")
  
  ;; Set the marker to beginning of buffer
  (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) (point-min)))
  
  ;; Clear any existing highlights
  (piper--clear-highlights)
  
  ;; Force highlighting of the current line
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (line-position (- line-start (point-min))))
    
    (piper--log "Forcing highlight for range %d-%d" 
               line-start line-end)
    
    ;; Create some dummy phoneme data for testing
    (setq piper--phoneme-data (list (list 0.0 "test" line-position)))
    (setq piper--current-phoneme-index 0)
    
    ;; Apply the highlight using phoneme-based approach
    (piper--update-highlight-from-phoneme line-position))
  
  (message "Highlight overlay applied. Run piper--clear-highlights to remove."))

(defun piper-highlight-status ()
  "Show status information about the current TTS highlighting."
  (interactive)
  (let ((buffer (get-buffer-create "*piper-highlight-status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Piper TTS Highlight Status:\n\n"))
      
      (if (not piper--phoneme-data)
          (insert "No phoneme data available.\n")
        (let ((i 0))
          (dolist (phoneme piper--phoneme-data)
            (when (< i 30) ;; Only show first 30 phonemes to avoid overwhelming the display
              (let ((time (nth 0 phoneme))
                    (phoneme-text (nth 1 phoneme))
                    (text-pos (nth 2 phoneme)))
                (insert (format "Phoneme %d (Time: %.3fs): pos %d, phoneme '%s'\n" 
                               i time text-pos phoneme-text))
                (when piper--current-text
                  (let* ((context-start (max 0 (- text-pos 10)))
                         (context-end (min (length piper--current-text) (+ text-pos 10)))
                         (text-snippet (substring piper--current-text
                                                context-start
                                                context-end))
                         (marker-pos (- text-pos context-start)))
                    (insert (format "  Context: \"%s[%s]%s\"\n\n" 
                                   (substring text-snippet 0 marker-pos)
                                   (if (< marker-pos (length text-snippet)) 
                                       (substring text-snippet marker-pos (1+ marker-pos))
                                     "")
                                   (if (< (1+ marker-pos) (length text-snippet))
                                       (substring text-snippet (1+ marker-pos))
                                     "")))))
                (setq i (1+ i))))
            (when (= i 30)
              (insert (format "\n... and %d more phonemes (showing first 30 only)\n\n" 
                             (- (length piper--phoneme-data) 30)))))))
      
      (insert (format "Current overlay: %s\n" 
                     (if piper--current-highlight-overlay 
                         "Active" 
                       "None")))
      (insert (format "Speak marker: %s\n" 
                     (if (and piper--speak-marker (marker-buffer piper--speak-marker))
                         (format "Buffer: %s, Position: %d" 
                                (buffer-name (marker-buffer piper--speak-marker))
                                (marker-position piper--speak-marker))
                       "None")))
      (insert (format "Play process: %s\n\n" 
                     (if (and piper--play-process (process-live-p piper--play-process))
                         "Running"
                       "Not running")))
      
      (insert "Debug log (most recent entries):\n")
      (with-current-buffer "*Messages*"
        (save-excursion
          (goto-char (point-max))
          (let ((count 0))
            (while (and (< count 10) (re-search-backward "piper-debug:" nil t))
              (let ((line (buffer-substring-no-properties 
                          (line-beginning-position) 
                          (line-end-position))))
                (with-current-buffer buffer
                  (insert (format "%s\n" line)))
                (setq count (1+ count))))))))
    
    (switch-to-buffer buffer)))

(defun piper-debug-highlight ()
  "Toggle debug mode and highlight mode for diagnosis."
  (interactive)
  (setq piper-debug (not piper-debug))
  (setq piper-highlight-mode (if piper-highlight-mode nil 'line))
  (message "Piper debug: %s, highlight-mode: %s" 
           (if piper-debug "ON" "OFF") 
           (or piper-highlight-mode "OFF")))

(defun piper-test-highlight (text)
  "Create a test overlay for highlighting TEXT.
This function allows testing the highlight face without actual TTS."
  (interactive "sText to highlight: ")
  (let ((buffer (current-buffer))
        (pos (point)))
    
    ;; First, clear any existing highlights
    (piper--clear-highlights)
    
    ;; Create an overlay at the current point
    (piper--log "Creating test highlight at %s:%d for text: %s" 
               (buffer-name) pos text)
    
    ;; Store info we need for highlighting
    (setq piper--speak-marker (set-marker (or piper--speak-marker (make-marker)) pos))
    (setq piper--current-text text)
    
    ;; Create a simple overlay
    (let ((overlay (make-overlay pos (+ pos (length text)) buffer)))
      (overlay-put overlay 'face 'piper-highlight-face)
      (overlay-put overlay 'piper-highlight t)
      (setq piper--current-highlight-overlay overlay)
      
      ;; Make the highlighted text visible by ensuring it's in the window
      (let ((win (get-buffer-window buffer t)))
        (when win
          (with-selected-window win
            (goto-char pos)
            (recenter)))))
    
    (piper--log "Highlight test overlay created. Delete with M-x piper--clear-highlights")))



(defun piper-force-highlight-current-buffer ()
  "Test highlighting by applying it to the entire current buffer.
This is for testing only - applies a highlight overlay to the entire buffer
to verify that the highlight face works correctly."
  (interactive)
  (piper--clear-highlights)
  (piper--log "Testing highlight face on whole buffer")
  
  ;; Create a test overlay on the whole buffer
  (let ((overlay (make-overlay (point-min) (point-max))))
    (overlay-put overlay 'face 'piper-highlight-face)
    (overlay-put overlay 'piper-highlight t)
    (setq piper--current-highlight-overlay overlay)
    
    ;; Make the highlighted text visible by ensuring it's in the window
    (let ((win (get-buffer-window (current-buffer) t)))
      (when win
        (with-selected-window win
          (goto-char (point-min))
          (recenter)))))
  
  (message "Highlight applied to buffer. Use piper--clear-highlights to remove."))

(defun piper-test-color-highlight ()
  "Test the text coloring by highlighting a region of text in the current buffer.
This creates a temporary overlay at point with a distinctive color."
  (interactive)
  (let* ((start (max (point-min) (- (point) 10)))
         (end (min (point-max) (+ (point) 10)))
         (overlay (make-overlay start end)))
    
    ;; Use a very bright distinct color that will definitely be visible
    (overlay-put overlay 'face '(:background "purple" :foreground "white" :weight bold))
    (overlay-put overlay 'priority 2000) ;; Extremely high priority
    
    ;; Show a message with instructions
    (message "PURPLE highlight created from %d to %d. It will disappear in 3 seconds." start end)
    
    ;; Set a timer to remove the overlay after a few seconds
    (run-with-timer 3 nil (lambda () 
                            (when (overlay-buffer overlay)
                              (delete-overlay overlay)
                              (message "Test highlight removed"))))))

(defun piper-debug-segments ()
  "Display the current phoneme data for debugging."
  (interactive)
  (let ((buffer (get-buffer-create "*piper-phonemes*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Current Phoneme Data:\n\n")
      
      (if (not piper--phoneme-data)
          (insert "No phoneme data available.\n")
        (let ((i 0))
          (dolist (phoneme piper--phoneme-data)
            (when (< i 30) ;; Only show first 30 phonemes to avoid overwhelming the display
              (let ((time (nth 0 phoneme))
                    (phoneme-text (nth 1 phoneme))
                    (text-pos (nth 2 phoneme)))
                (insert (format "Phoneme %d (Time: %.3fs): pos %d, phoneme '%s'\n" 
                               i time text-pos phoneme-text))
                (when piper--current-text
                  (let* ((context-start (max 0 (- text-pos 10)))
                         (context-end (min (length piper--current-text) (+ text-pos 10)))
                         (text-snippet (substring piper--current-text
                                                context-start
                                                context-end))
                         (marker-pos (- text-pos context-start)))
                    (insert (format "  Context: \"%s[%s]%s\"\n\n" 
                                   (substring text-snippet 0 marker-pos)
                                   (if (< marker-pos (length text-snippet)) 
                                       (substring text-snippet marker-pos (1+ marker-pos))
                                     "")
                                   (if (< (1+ marker-pos) (length text-snippet))
                                       (substring text-snippet (1+ marker-pos))
                                     "")))))
                (setq i (1+ i))))
            (when (= i 30)
              (insert (format "\n... and %d more phonemes (showing first 30 only)\n\n" 
                             (- (length piper--phoneme-data) 30)))))))
      
      (goto-char (point-min)))
    
    (display-buffer buffer)))

(defun piper-test-highlight-face ()
  "Create a test overlay using the current highlight face
to verify that the highlight face is working correctly."
  (interactive)
  (piper--clear-highlights)
  (piper--log "Testing highlight face on whole buffer")
  
  (let* ((buffer (current-buffer))
         (pos (point))
         (text (buffer-substring-no-properties 
                (line-beginning-position) 
                (line-end-position))))
    
    (let ((overlay (make-overlay pos (+ pos (length text)) buffer)))
      (overlay-put overlay 'face 'piper-highlight-face)
      (overlay-put overlay 'piper-highlight t)
      (setq piper--current-highlight-overlay overlay)
      
      ;; Make the highlighted text visible by ensuring it's in the window
      (let ((win (get-buffer-window buffer t)))
        (when win
          (with-selected-window win
            (goto-char pos)
            (recenter)))))
    
    (piper--log "Highlight test overlay created. Delete with M-x piper--clear-highlights")))
