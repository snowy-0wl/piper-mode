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

;;; Code:

(require 'piper-infra)
(require 'piper-text)
(require 'piper-models)
(require 'piper-tts)

;;;###autoload
(defun piper-speak (text)
  "Speak TEXT using Piper TTS.
Large texts are automatically chunked for faster playback startup."
  (interactive "MText to speak: ")
  (piper--log "Speaking text: %s" text)
  
  ;; Ensure setup is done
  (piper--ensure-setup)
  
  ;; Stop any current speech
  (piper--stop)
  
  ;; Save the original text
  (setq piper--original-text text)
  
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
  (let ((text (buffer-substring-no-properties start end)))
    (piper-speak text)))

;;;###autoload
(defun piper-speak-buffer ()
  "Speak the entire buffer using Piper TTS."
  (interactive)
  (piper--log "Speaking entire buffer")
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (piper-speak text)))

;;;###autoload
(defun piper-speak-paragraph ()
  "Speak the current paragraph using Piper TTS."
  (interactive)
  (piper--log "Speaking paragraph")
  (save-excursion
    (let* ((start (progn (backward-paragraph) (point)))
           (end (progn (forward-paragraph) (point)))
           (text (buffer-substring-no-properties start end)))
      (piper-speak text))))

;;;###autoload
(defun piper-speak-line ()
  "Speak the current line using Piper TTS."
  (interactive)
  (piper--log "Speaking line")
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (text (buffer-substring-no-properties line-start line-end)))
    (piper-speak text)))

;;;###autoload
(defun piper-speak-word ()
  "Speak the current word using Piper TTS."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (if bounds (car bounds) (point)))
         (end (if bounds (cdr bounds) (point)))
         (text (buffer-substring-no-properties start end)))
    (piper-speak text)))

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
         (text (buffer-substring-no-properties start (point-max))))
    (piper-speak text)))

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
