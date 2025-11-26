;;; piper-infra.el --- Infrastructure for piper-mode -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Keywords: multimedia, tts, accessibility, speech

;;; Commentary:
;; Infrastructure and setup for piper-mode.

;;; Code:

(require 'cl-lib)

(defgroup piper nil
  "Text-to-speech using Piper TTS."
  :group 'multimedia
  :prefix "piper-")

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

(defcustom piper-development-mode nil
  "When non-nil, optimize the rebuild process for faster development.
This prevents rebuilding native components when only elisp files have changed,
resulting in a much faster feedback loop during development."
  :type 'boolean
  :group 'piper)

(defvar piper--source-dir (file-name-directory (or load-file-name buffer-file-name))
  "Source directory of the piper-mode package.")

(defvar piper-script-path nil
  "Path to the Piper run script.")

(defvar piper-temp-dir nil
  "Temporary directory for WAV files.")

(defvar piper-audio-pipe nil
  "Path to the named pipe for streaming audio.")

(defvar piper--setup-done nil
  "Flag to track if setup has been completed.")

;; Define the Straight.el build directory variable
(defvar straight-build-dir nil
  "Straight.el build directory.")

(defun piper--log (format-string &rest args)
  "Log a message if debug is enabled.
FORMAT-STRING and ARGS are passed to `message'."
  (when piper-debug
    (apply #'message (concat "piper-debug: " format-string) args)))

(defun piper--get-install-dir ()
  "Get the installation directory for Piper.
This is either the custom directory set in `piper-install-dir', or
derived from the package location."
  (or piper-install-dir
      (let* ((source-dir (directory-file-name piper--source-dir))

             (straight-build-dir (when (string-match "/straight/repos/" source-dir)
                                 (replace-match "/straight/build/" t t source-dir))))
        (or straight-build-dir source-dir))))

(defun piper--get-script-path ()
  "Get the path to the Piper run script."
  (expand-file-name "bin/piper-with-phonemes" (piper--get-install-dir)))

(defun piper--get-temp-dir ()
  "Get the temporary directory for WAV files."
  (let ((tmp-dir (expand-file-name "tmp" (piper--get-install-dir))))
    (unless (file-exists-p tmp-dir)
      (make-directory tmp-dir t))
    tmp-dir))

(defun piper--initialize-paths ()
  "Initialize paths based on install directory."
  (let ((install-dir (piper--get-install-dir)))
    (piper--log "Initializing paths with install-dir: %s" install-dir)
    (unless piper-script-path
      (setq piper-script-path (piper--get-script-path)))
    (unless piper-temp-dir
      (setq piper-temp-dir (piper--get-temp-dir)))
    (unless piper-audio-pipe
      (setq piper-audio-pipe (expand-file-name "piper.pipe" piper-temp-dir)))))

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

(defun piper--verify-installation ()
  "Verify that all required Piper components are installed and accessible."
  (let* ((install-dir (piper--get-install-dir))
         (bin-dir (expand-file-name "bin" install-dir))
         (piper-bin (expand-file-name "piper" bin-dir))
         (run-script (piper--get-script-path))
         (onnx-lib (expand-file-name "libonnxruntime.1.14.1.dylib" bin-dir)))
    
    (piper--log "Verifying Piper installation:")
    (piper--log "  Install dir: %s" install-dir)
    (piper--log "  Piper binary: %s (exists: %s)" piper-bin (file-exists-p piper-bin))
    (piper--log "  Run script: %s (exists: %s)" run-script (file-exists-p run-script))
    (piper--log "  ONNX lib: %s (exists: %s)" onnx-lib (file-exists-p onnx-lib))
    
    (unless (file-exists-p piper-bin)
      (error "Piper binary not found at %s" piper-bin))
    (unless (file-exists-p run-script)
      (error "Run script not found at %s" run-script))
    (unless (executable-find "sox")
      (error "SoX not found. Please install it (e.g., 'brew install sox')"))))

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
         (source-repo-dir (file-truename piper--source-dir))
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

(provide 'piper-infra)
;;; piper-infra.el ends here
