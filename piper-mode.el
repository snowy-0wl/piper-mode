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

(defcustom piper-debug nil
  "Enable debug logging."
  :type 'boolean
  :group 'piper)

(defcustom piper-install-dir nil
  "Directory where Piper is installed.
If nil, will be set automatically based on package installation location."
  :type '(choice (const :tag "Auto" nil)
                (directory :tag "Custom directory"))
  :group 'piper)

(defcustom piper-voice-model nil
  "Path to the Piper voice model file.
If nil, will prompt to select a model using `piper-select-model'."
  :type '(choice (const :tag "Default model" nil)
                (file :tag "Custom model file"))
  :group 'piper)

(defcustom piper-models-url "https://raw.githubusercontent.com/rhasspy/piper/master/VOICES.md"
  "URL to fetch available voice models from."
  :type 'string
  :group 'piper)

(defvar piper--available-models nil
  "Cache of available voice models.")

(defvar piper--debug t
  "Enable debug logging.")

(defun piper--log (format-string &rest args)
  "Log a message with FORMAT-STRING and ARGS if debug is enabled."
  (when piper--debug
    (let ((message-log-max t))
      (apply #'message (concat "[piper] " format-string) args))))

(defun piper--clean-url (url)
  "Clean URL by removing query parameters and fragments."
  (let ((base-url (car (split-string url "[?#]")))
        (has-download (string-match-p "\\?download=true" url)))
    (if has-download
        (concat base-url "?download=true")
      base-url)))

(defun piper--get-config-url (model-url)
  "Get config URL from model URL."
  (let* ((clean-url (piper--clean-url model-url))
         (config-url (if (string-match "\\.onnx$" clean-url)
                        (concat (substring clean-url 0 (match-beginning 0)) ".onnx.json")
                      (concat clean-url ".json"))))
    (piper--log "Generated config URL: %s" config-url)
    config-url))

(cl-defstruct (piper-model (:constructor piper-model-create))
  "Structure for storing voice model information."
  name model-url config-url lang-name lang-code voice quality)

(defun piper--parse-model-url (url)
  "Extract model information from URL.
Returns plist with :lang :lang-code :voice :quality :model-url, or nil if invalid."
  (piper--log "Parsing URL: %s" url)
  ;; Try to match the URL pattern, ignoring query parameters
  (when (string-match "huggingface\\.co/[^/]+/[^/]+/[^/]+/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)/[^?]+\\.onnx" url)
    (let ((lang (match-string 1 url))
          (lang-code (match-string 2 url))
          (voice (match-string 3 url))
          (quality (match-string 4 url)))
      (piper--log "Found model: lang=%s code=%s voice=%s quality=%s" lang lang-code voice quality)
      (when (and lang lang-code voice quality)
        (piper-model-create
         :name (format "%s - %s (%s)" voice quality lang)
         :model-url url
         :config-url (piper--get-config-url url)
         :lang-name lang
         :lang-code lang-code
         :voice voice
         :quality quality)))))

(defvar piper--available-models nil
  "List of available Piper TTS models.")

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
        (error "Failed to fetch models: curl exited with status %s" curl-status)))
    temp-file))

(defun piper--parse-model-info (model-url)
  "Parse model info from MODEL-URL and return components."
  (let* ((url-parts (split-string model-url "/"))
         (file-name (car (last url-parts)))
         (name-parts (split-string (file-basename file-name ".onnx") "-")))
    (list :lang-code (concat (car name-parts) "_" (cadr name-parts))
          :voice (caddr name-parts))))

(defun piper--create-model-entry (model-url config-url)
  "Create a model entry from MODEL-URL and CONFIG-URL."
  (let* ((info (piper--parse-model-info model-url))
         (lang-code (plist-get info :lang-code))
         (voice (plist-get info :voice))
         (quality (plist-get info :quality)))
    (piper-model-create
     :name (format "%s - %s (%s)" voice quality lang-code)
     :model-url model-url
     :config-url config-url
     :lang-name lang-code
     :lang-code lang-code
     :voice voice
     :quality quality)))

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
      (error "No models found in the file"))))

(defun piper--find-models-in-buffer ()
  "Find all model/config pairs in current buffer."
  (let ((raw-models (piper--parse-models-file))
        models)
    (dolist (model-info raw-models)
      (let* ((name (nth 0 model-info))
             (model-url (nth 1 model-info))
             (config-url (nth 2 model-info))
             ;; Extract language and voice from model name
             (parts (split-string name "-"))
             (lang-code (concat (nth 0 parts) "_" (nth 1 parts)))
             (voice (nth 2 parts))
             (quality (nth 3 parts)))
        (push (piper-model-create
               :name (format "%s - %s (%s)" voice quality lang-code)
               :model-url model-url
               :config-url config-url
               :voice voice
               :quality quality
               :lang-name lang-code
               :lang-code lang-code)
              models)))
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

(defun piper--handle-download (output-file reporter callback)
  "Handle download to OUTPUT-FILE, update REPORTER, and call CALLBACK when done."
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
                                     "-L" "-f" "-o" clean-output-file
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
                                                    "-L" "-f" "-o" (concat clean-output-file ".json")
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
(defun piper-select-model ()
  "Select and switch to a different voice model."
  (interactive)
  (unless piper--available-models
    (message "Fetching available models...")
    (piper--fetch-available-models))

  ;; Group models by language
  (let* ((langs (delete-dups
                 (mapcar (lambda (m) (cons (piper-model-lang-name m)
                                          (piper-model-lang-code m)))
                        piper--available-models)))
         ;; First select language
         (selected-lang (completing-read
                        "Select language: "
                        (mapcar (lambda (l) (format "%s (%s)" (car l) (cdr l))) langs)
                        nil t))
         (lang-name (car (split-string selected-lang " (")))
         ;; Filter models for selected language
         (lang-models (seq-filter
                      (lambda (m) (string= (piper-model-lang-name m) lang-name))
                      piper--available-models))
         ;; Then select specific model
         (model-choices (mapcar (lambda (m)
                                (cons (format "%s - %s"
                                              (piper-model-voice m)
                                              (piper-model-quality m))
                                      m))
                              lang-models))
         (selected-model-name (completing-read
                             (format "Select %s voice: " lang-name)
                             (mapcar #'car model-choices)
                             nil t))
         (selected-model (cdr (assoc selected-model-name model-choices)))
         (model-url (piper-model-model-url selected-model))
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
             (piper-model-lang-name selected-model))))

(defcustom piper-process-timeout 30
  "Timeout in seconds for Piper TTS process."
  :type 'integer
  :group 'piper)

(defvar piper--source-dir (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the piper-mode source files.")

(defvar piper--setup-done nil
  "Flag to track if setup has been completed.")

(defvar piper--current-process nil
  "Current piper process.")

(defvar piper--play-process nil
  "Current audio playback process.")

(defvar piper-script-path nil
  "Path to the piper run script.
This is set automatically by `piper--initialize-paths'.")

(defvar piper-temp-dir nil
  "Directory for temporary wav files.
This is set automatically by `piper--initialize-paths'.")

(defun piper--log (format-string &rest args)
  "Log a message if debug is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when piper-debug
    (apply #'message (concat "piper-debug: " format-string) args)))

(defun piper--get-install-dir ()
  "Get the installation directory for Piper.
This is either the custom directory set in `piper-install-dir', or derived from the package location."
  (or piper-install-dir
      (let* ((source-dir (directory-file-name piper--source-dir))
             (straight-build-dir (when (string-match "/straight/repos/" source-dir)
                                 (replace-match "/straight/build/" t t source-dir))))
        (or straight-build-dir source-dir))))

(defun piper--get-script-path ()
  "Get the path to the Piper run script."
  (expand-file-name "bin/piper" (piper--get-install-dir)))

(defun piper--get-temp-dir ()
  "Get the temporary directory for WAV files."
  (let ((tmp-dir (expand-file-name "tmp" (piper--get-install-dir))))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir t))
    tmp-dir))

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

(defun piper--initialize-paths ()
  "Initialize paths based on install directory."
  (let ((install-dir (piper--get-install-dir)))
    (piper--log "Initializing paths with install-dir: %s" install-dir)
    (unless piper-script-path
      (setq piper-script-path (piper--get-script-path)))
    (unless piper-voice-model
      (when (yes-or-no-p "No voice model selected. Would you like to select one now? ")
        (piper-select-model)))
    (unless piper-temp-dir
      (setq piper-temp-dir (piper--get-temp-dir)))))

;; Setup and initialization
(defun piper--ensure-setup ()
  "Ensure Piper is properly set up by running the setup script if needed."
  (unless piper--setup-done
    (let* ((source-dir (piper--get-install-dir))
           (setup-script (expand-file-name "setup-piper.sh" source-dir))
           (install-dir (piper--get-install-dir))
           (bin-dir (expand-file-name "bin" install-dir))
           (piper-bin (expand-file-name "piper" bin-dir)))

      ;; Only run setup if the installation is not already valid
      (when (and piper-auto-setup
                 (file-exists-p setup-script)
                 (or (not (file-exists-p piper-bin))
                     (not (file-executable-p piper-bin))))
        (message "Setting up Piper TTS. This may take a few minutes...")
        ;; Clean and recreate install directory
        (piper--handle-straight-build)
        (make-directory install-dir t)
        (let ((default-directory source-dir))
          (message "Running setup script from %s" default-directory)
          (let ((result (call-process "bash" nil "*piper-setup*" t setup-script install-dir)))
            (message "Setup script exited with code %d" result)))
        (message "Piper TTS setup complete!"))

      ;; Verify installation once and set setup-done flag
      (piper--verify-installation)
      (setq piper--setup-done t))))


(defun piper--verify-installation ()
  "Verify that all required Piper components are installed and accessible."
  (let* ((install-dir (piper--get-install-dir))
         (bin-dir (expand-file-name "bin" install-dir))
         (piper-bin (expand-file-name "piper" bin-dir))
         (run-script (piper--get-script-path))
         (espeak-data (expand-file-name "espeak-ng-data" bin-dir))
         (onnx-lib (expand-file-name "libonnxruntime.1.14.1.dylib" bin-dir))
         (espeak-lib (expand-file-name "libespeak-ng.1.dylib" bin-dir))
         (piper-lib (expand-file-name "libpiper_phonemize.1.dylib" bin-dir)))

    (piper--log "Verifying Piper installation:")
    (piper--log "  Install dir: %s" install-dir)
    (piper--log "  Piper binary: %s (exists: %s)" piper-bin (file-exists-p piper-bin))
    (piper--log "  Run script: %s (exists: %s)" run-script (file-exists-p run-script))
    (piper--log "  Espeak data: %s (exists: %s)" espeak-data (file-exists-p espeak-data))
    (piper--log "  ONNX lib: %s (exists: %s)" onnx-lib (file-exists-p onnx-lib))
    (piper--log "  Espeak lib: %s (exists: %s)" espeak-lib (file-exists-p espeak-lib))
    (piper--log "  Piper lib: %s (exists: %s)" piper-lib (file-exists-p piper-lib))

    (unless (file-exists-p piper-bin)
      (error "Piper binary not found at %s" piper-bin))
    (unless (file-exists-p run-script)
      (error "Run script not found at %s" run-script))
    (unless (file-exists-p espeak-data)
      (error "Espeak data not found at %s" espeak-data))
    (unless (file-exists-p onnx-lib)
      (error "ONNX runtime library not found at %s" onnx-lib))
    (unless (file-exists-p espeak-lib)
      (error "Espeak library not found at %s" espeak-lib))
    (unless (file-exists-p piper-lib)
      (error "Piper phonemize library not found at %s" piper-lib))))

(defun piper--ensure-tmp-dir ()
  "Ensure the tmp directory exists in the piper install directory."
  (let ((tmp-dir (expand-file-name "tmp" (piper--get-install-dir))))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir t))
    tmp-dir))

;; Build directory handling
(defun piper--handle-straight-build ()
  "Handle straight.el build directory cleanup and setup."
  (let ((build-dir (expand-file-name "straight/build/piper-mode" user-emacs-directory)))
    ;; Only remove elisp and script files, preserve models and binaries
    (when (file-exists-p build-dir)
      (dolist (file (directory-files build-dir t))
        (let ((base-name (file-name-nondirectory file)))
          (unless (or (string= base-name ".")
                     (string= base-name "..")
                     (string= base-name "models")
                     (string= base-name "bin")
                     (string= base-name "tmp"))
            (if (file-directory-p file)
                (delete-directory file t)
              (delete-file file))))))))

(defun piper--before-straight-rebuild (orig-fun &rest args)
  "Advice to run before straight-rebuild-package for piper-mode."
  (when (equal (car args) "piper-mode")
    (piper--handle-straight-build))
  (apply orig-fun args))

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

    (condition-case nil
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
                           (piper--log "Process exit status: %d" (process-exit-status proc))
                           (funcall callback (process-exit-status proc)))
                 :filter (lambda (proc output)
                          (piper--log "Process output: %s" output))
                 :stderr process-buffer))

          ;; Send input text to process stdin
          (process-send-string process (concat input-text "\n"))
          (process-send-eof process)

          ;; Set process timeout
          (run-with-timer piper-process-timeout nil
                         (lambda ()
                           (when (process-live-p process)
                             (piper--log "Process timeout reached")
                             (delete-process process)
                             (funcall callback 1)))))

      (error (err)
       (piper--log "Failed to start process: %s" (error-message-string err))
       (funcall callback 1)))))

(defun piper--play-wav-file (file-path &optional callback)
  "Play the WAV file at FILE-PATH using afplay."
  (piper--log "Playing WAV file: %s" file-path)
  (make-process
   :name "piper-play"
   :command (list "afplay" file-path)
   :sentinel (lambda (proc event)
               (piper--log "Play process event: %s" event)
	       (when callback (funcall callback (process-exit-status proc))))))

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
    (setq piper--play-process nil)))

(defun piper--stop ()
  "Stop any currently running TTS process."
  (when piper--current-process
    (delete-process piper--current-process)
    (setq piper--current-process nil))
  (when piper--play-process
    (delete-process piper--play-process)
    (setq piper--play-process nil)))

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
             (lambda (proc event)
               (piper--log "Audio playback process ended with: %s" event)
               (when (string= event "finished\n")
                 (piper--log "Audio playback completed successfully")
                 (piper--cleanup-temp-wav wav-file)))))
        (piper--log "WAV file is empty, skipping playback")
        (piper--cleanup-temp-wav wav-file)))))

;; Main interface functions
;;;###autoload
(defun piper-speak (text &optional callback)
  "Convert TEXT to speech using Piper TTS."
  (interactive "sText: ")
  (piper--log "Speaking text: %s" text)
  (piper--ensure-script)
  (let ((wav-file (piper--create-temp-wav)))
    (piper--stop)
    (piper--start-process text wav-file piper-voice-model
                          (lambda (status)
                            (piper--log "Process completed with status: %s" status)
                            (if (eq status 0)
                                (progn
                                  (piper--log "Playing WAV file: %s" wav-file)
                                  (piper--play-wav-file wav-file callback))
                              (progn
                                (piper--log "Failed to generate speech, cleaning up")
                                (piper--cleanup-temp-wav wav-file)
                                (message "Failed to generate speech: %s" status)
				(when callback (funcall callback status))))))))

;;;###autoload
(defun piper-speak-text-list (text-list)
  "Convert TEXT-LIST sequentially to speech using Piper TTS."
  (when text-list
    (let ((text (car text-list)))
      (piper-speak text
		   (lambda (status)
		     (when (eq status 0)
		       (piper-speak-text-list (cdr text-list))))))))

;;;###autoload
(defun piper-speak-region (start end)
  "Speak the region between START and END."
  (interactive "r")
  (piper--log "Speaking region from %d to %d" start end)
  (piper--ensure-script)
  (piper--cleanup)

  (let* ((text (buffer-substring-no-properties start end))
         (clean-text (replace-regexp-in-string "[\\\"']" ""
					       (replace-regexp-in-string "\\s-+" " " text)))
         (wav-file (piper--create-temp-wav)))
    (piper--log "Speaking text: %s" clean-text)
    (piper--log "Using wav file: %s" wav-file)

    (piper--start-process clean-text wav-file piper-voice-model
                          (lambda (status)
                            (if (eq status 0)
                                (piper--play-wav-file wav-file)
                              (progn
                                (piper--log "Failed to generate speech, cleaning up")
                                (piper--cleanup-temp-wav wav-file)
                                (message "Failed to generate speech: %s" status)))))))

;;;###autoload
(defun piper-speak-buffer ()
  "Speak the entire buffer."
  (interactive)
  (piper-speak-region (point-min) (point-max)))

;;;###autoload
(defun piper-speak-paragraph ()
  "Speak the current paragraph."
  (interactive)
  (save-excursion
    (let* ((start (progn (backward-paragraph) (point)))
           (end (progn (forward-paragraph) (point))))
      (piper-speak-region start end))))

;;;###autoload
(defun piper-speak-line ()
  "Speak the current line."
  (interactive)
  (piper-speak-region (line-beginning-position) (line-end-position)))

;;;###autoload
(defun piper-speak-word ()
  "Speak the current word."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when bounds
      (piper-speak-region (car bounds) (cdr bounds)))))

;;;###autoload
(defun piper-stop ()
  "Stop speaking and clean up."
  (interactive)
  (piper--log "Stopping piper")
  ;; Kill all afplay processes
  (shell-command "pkill -f afplay")
  ;; Clean up our processes and resources
  (piper--cleanup))

;;;###autoload
(defun piper-speak-to-end ()
  "Speak text from point to the end of buffer using Piper TTS."
  (interactive)
  (piper--log "Speaking from point to end of buffer")
  (let ((text (buffer-substring-no-properties (point) (point-max))))
    (piper-speak text)))

;;;###autoload
(define-minor-mode piper-mode
  "Toggle Piper TTS mode.
When Piper mode is enabled, you can use various commands to have text read aloud."
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

(provide 'piper-mode)

;;; piper-mode.el ends here
