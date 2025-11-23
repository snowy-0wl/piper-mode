;;; piper-tts.el --- TTS engine and playback for piper-mode -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Keywords: multimedia, tts, accessibility, speech

;;; Commentary:
;; TTS engine integration and audio playback for piper-mode.

;;; Code:

(require 'piper-infra)
(require 'piper-models)
(require 'piper-text)

(defcustom piper-process-timeout 30
  "Timeout in seconds for Piper TTS process."
  :type 'integer
  :group 'piper)

(defvar piper--current-process nil
  "Current Piper TTS process.")

(defvar piper--play-process nil
  "Current audio playback process.")

(defvar piper--current-text nil
  "Text currently being spoken.")

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
          
          ;; Save the text
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
  "Play the WAV file at FILE-PATH."
  (piper--log "[DEBUG] Playing WAV file: %s" file-path)
  (piper--log "[DEBUG] WAV file exists: %s" (if (file-exists-p file-path) "YES" "NO"))
  (when (file-exists-p file-path)
    (piper--log "[DEBUG] WAV file size: %d bytes" (file-attribute-size (file-attributes file-path))))
  
  ;; Create the process for playback
  (let ((process-buffer (get-buffer-create "*piper-play*")))
    ;; Clear the buffer before starting
    (with-current-buffer process-buffer
      (erase-buffer))
    
    (setq piper--play-process
          (make-process
           :name "piper-play"
           :buffer process-buffer
           :command (list "afplay" file-path)
           :sentinel (lambda (_proc event)
                      (piper--log "Play process event: %s" event)))))
  
  (piper--log "Started playback process: %s" piper--play-process))

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

  (setq piper--current-text nil))

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

(defun piper--play-chunk-wav-file (file-path)
  "Play the chunk WAV file at FILE-PATH."
  (piper--log "Playing chunk WAV file: %s" file-path)
  
  ;; Create the process for playback
  (let ((process-buffer (get-buffer-create "*piper-play*")))
    ;; Clear the buffer before starting
    (with-current-buffer process-buffer
      (erase-buffer))
    
    (setq piper--play-process
          (make-process
           :name "piper-play"
           :buffer process-buffer
           :command (list "afplay" file-path)
           :sentinel (lambda (_proc event)
                      (piper--log "Play process event: %s" event)
                      (when (string-match-p "\\(finished\\|exited\\)" event)
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
  (setq piper--current-text nil)
  (setq piper--chunk-queue nil)
  (setq piper--chunk-processing nil)
  (setq piper--current-chunk-index 0)
  (setq piper--total-chunks 0)
  (setq piper--original-text nil))

(provide 'piper-tts)
;;; piper-tts.el ends here
