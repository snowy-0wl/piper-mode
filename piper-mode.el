;;; piper-mode.el --- Text-to-speech using Piper TTS -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Version: 0.5
;; Package-Requires: ((emacs "27.1") (json "1.4"))
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
If nil, will use the default model in the models directory."
  :type '(choice (const :tag "Default model" nil)
                (file :tag "Custom model file"))
  :group 'piper)

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

(defun piper--initialize-paths ()
  "Initialize paths based on install directory."
  (let ((install-dir (piper--get-install-dir)))
    (piper--log "Initializing paths with install-dir: %s" install-dir)
    (unless piper-script-path
      (setq piper-script-path (piper--get-script-path)))
    (unless piper-voice-model
      (setq piper-voice-model (expand-file-name "models/en_US-joe-medium.onnx" install-dir)))
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
    (when (file-exists-p build-dir)
      (call-process "rm" nil nil nil "-rf" build-dir))))

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
      
      (error
       (piper--log "Failed to start process: %s" (error-message-string err))
       (funcall callback 1)))))

(defun piper--play-wav-file (file-path)
  "Play the WAV file at FILE-PATH using afplay."
  (piper--log "Playing WAV file: %s" file-path)
  (make-process
   :name "piper-play"
   :command (list "afplay" file-path)
   :sentinel (lambda (proc event)
               (piper--log "Play process event: %s" event))))

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
(defun piper-speak (text)
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
                                  (piper--play-wav-file wav-file))
                              (progn
                                (piper--log "Failed to generate speech, cleaning up")
                                (piper--cleanup-temp-wav wav-file)
                                (message "Failed to generate speech: %s" status)))))))

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
