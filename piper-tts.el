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

(defvar piper--active-timers nil
  "List of active timers for cleanup.")

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
                           ;; Cancel timeout timer to prevent double-callback
                           (let ((timer (plist-get (process-plist proc) :timeout-timer)))
                             (when timer
                               (cancel-timer timer)
                               (setq piper--active-timers (delq timer piper--active-timers))))
                           (when (string-match-p "finished" event)
                             (funcall callback (process-exit-status proc))))))
          
          ;; Save the process for later
          (setq piper--current-process process)
          
          ;; Send the text to the process
          (process-send-string process (concat input-text "\n"))
          (process-send-eof process)
          
          ;; Set process timeout and track it
          (let ((timeout-timer
                 (run-with-timer piper-process-timeout nil
                                (lambda ()
                                  (when (process-live-p process)
                                    (piper--log "Process timeout reached")
                                    (delete-process process)
                                    (funcall callback 1))))))
            (push timeout-timer piper--active-timers)
            (set-process-plist process (list :timeout-timer timeout-timer))))
      
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

(defvar piper--current-overlay nil
  "Overlay used to highlight the currently spoken text.")

(defvar piper--batch-size 3
  "Number of chunks to batch together for playback.")

(defvar piper--current-batch nil
  "List of (wav-file start-pos end-pos) for current batch.")



(defvar piper--paused nil
  "Whether playback is paused.")

(defun piper--update-highlight (start end)
  "Update the highlight overlay to cover the range from START to END."
  (when (and start end)
    (unless piper--current-overlay
      (setq piper--current-overlay (make-overlay start end))
      (overlay-put piper--current-overlay 'face 'piper-highlight-face))
    (move-overlay piper--current-overlay start end)))

(defun piper--remove-highlight ()
  "Remove the highlight overlay."
  (when piper--current-overlay
    (delete-overlay piper--current-overlay)
    (setq piper--current-overlay nil)))

(defvar piper--audio-queue nil
  "Queue of generated WAV files waiting to be played.
Each element is a list (wav-file start-pos end-pos).")

(defconst piper--max-audio-queue-size 5
  "Maximum number of audio chunks to buffer ahead.")



(defvar piper--current-wav-file nil
  "The WAV file currently being played.")

(defun piper--process-next-chunk ()
  "Entry point to start processing chunks.
Starts the generation loop."
  (piper--log "Starting chunk processing pipeline")
  (setq piper--chunk-processing t)
  (setq piper--paused nil)
  (setq piper--current-batch nil)
  (piper--generate-next-chunk))

(defun piper--generate-next-chunk ()
  "Producer: Generate the next audio chunk if needed."
  (piper--log "Generator check: queue-size=%d, processing=%s, busy=%s" 
              (length piper--audio-queue)
              piper--chunk-processing
              (if (process-live-p piper--current-process) "yes" "no"))

  (when (and piper--chunk-processing
             piper--chunk-queue
             (< (length piper--audio-queue) piper--max-audio-queue-size)
             (not (process-live-p piper--current-process)))
    
    (let* ((next-chunk-data (pop piper--chunk-queue))
           (text (nth 0 next-chunk-data))
           (start-pos (nth 1 next-chunk-data))
           (end-pos (nth 2 next-chunk-data)))
      
      (if text
          (progn
            (cl-incf piper--current-chunk-index)
            (piper--log "Generating chunk %d/%d (length: %d)"
                       piper--current-chunk-index piper--total-chunks
                       (length text))
            
            (setq piper--current-text text)
            (let ((wav-file (piper--create-temp-wav))
                  (normalized-text (piper--normalize-text text)))
              (piper--start-process 
               normalized-text 
               wav-file 
               (piper--expand-model-path piper-voice-model)
               (lambda (status)
                 (piper--log "Generation completed with status: %s" status)
                 (setq piper--current-process nil)
                 (if (eq status 0)
                     (progn
                       ;; Push to audio queue with positions
                       (setq piper--audio-queue (append piper--audio-queue (list (list wav-file start-pos end-pos))))
                       (piper--log "Audio queue size: %d" (length piper--audio-queue))
                       
                       ;; Trigger playback if not playing
                       (unless (process-live-p piper--play-process)
                         (piper--play-next-chunk))
                       
                       ;; Try to generate more
                       (piper--generate-next-chunk))
                   (progn
                     (piper--log "Failed to generate chunk")
                     (piper--cleanup-temp-wav wav-file)
                     ;; For now, try next chunk
                     (piper--generate-next-chunk)))))))
        ;; No more chunks to generate
        (piper--log "No more chunks to generate")))))

(defun piper--play-next-chunk ()
  "Consumer: Play the next audio chunk batch."
  (piper--log "Playback check: queue-size=%d, paused=%s, batch-ready=%s" 
              (length piper--audio-queue)
              piper--paused
              (>= (length piper--audio-queue) piper--batch-size))

  (when (and (not (process-live-p piper--play-process))
             (not piper--paused)
             (or (>= (length piper--audio-queue) piper--batch-size)
                 (and piper--audio-queue
                      (null piper--chunk-queue)
                      (not (process-live-p piper--current-process)))))
    ;; Collect batch
    (let ((batch-size (min piper--batch-size (length piper--audio-queue)))
          (batch nil)
          (wav-files nil))
      
      (dotimes (_ batch-size)
        (push (pop piper--audio-queue) batch))
      (setq batch (nreverse batch))
      (setq wav-files (mapcar 'car batch))
      
      (piper--log "Playing batch of %d chunks" batch-size)
      (piper--play-batch batch wav-files))))

(defun piper-speak-chunk (text)
  "Legacy/Direct entry point."
  (unless piper--chunk-processing
    (let ((wav-file (piper--create-temp-wav))
          (normalized-text (piper--normalize-text text)))
      (piper--start-process normalized-text wav-file (piper--expand-model-path piper-voice-model)
                            (lambda (status)
                              (if (eq status 0)
                                  (piper--play-wav-file wav-file)
                                (piper--cleanup-temp-wav wav-file)))))))

(defun piper--play-batch (batch wav-files)
  "Play a batch of chunks using sox concatenation."
  (let* ((first-chunk (car batch))
         (start-pos (nth 1 first-chunk))
         (last-chunk (car (last batch)))
         (end-pos (nth 2 last-chunk))
         (sox-concat-cmd (format "sox %s -t wav - | play -t wav -"
                                (mapconcat #'shell-quote-argument
                                          wav-files " "))))
    
    (piper--update-highlight start-pos end-pos)
    (piper--log "Batch highlight: %s-%s" start-pos end-pos)
    (piper--log "Batch command: %s" sox-concat-cmd)
    
    (setq piper--current-batch batch)
    
    (setq piper--play-process
          (make-process
           :name "piper-batch-play"
           :command (list "sh" "-c" sox-concat-cmd)
           :buffer (get-buffer-create "*piper-play*")
           :sentinel (lambda (proc event)
                      (piper--log "Batch play event: %s" (string-trim event))
                      (when (string-match-p "\\(finished\\|exited\\)" event)
                        (let ((exit-status (process-exit-status proc)))
                          
                          (if piper--paused
                              (progn
                                (piper--log "Playback paused, preserving batch")
                                (setq piper--play-process nil))
                            
                            ;; Normal cleanup
                            (dolist (wav-file wav-files)
                              (piper--cleanup-temp-wav wav-file))
                            
                            (setq piper--current-batch nil)
                            (setq piper--play-process nil)
                            
                            ;; Check if sox failed
                            (if (not (= exit-status 0))
                                (progn
                                  (piper--log "Sox/play failed with exit code %d" exit-status)
                                  (message "Piper: Audio playback failed"))
                              ;; Success - continue processing
                              (if (not piper--chunk-processing)
                                  (piper--log "Playback stopped, not continuing")
                                (progn
                                  ;; Trigger next batch
                                  (piper--play-next-chunk)
                                  ;; Trigger generation
                                  (piper--generate-next-chunk)
                                  
                                  ;; Check if done
                                  (when (and (null piper--chunk-queue)
                                             (null piper--audio-queue)
                                             (not (process-live-p piper--current-process))
                                             (not (process-live-p piper--play-process)))
                                    (piper--log "All chunks processed")
                                    (piper--cleanup)))))))))))))

(defun piper-pause ()
  "Pause playback."
  (interactive)
  (piper--log "Pausing playback")
  (setq piper--paused t)
  (when piper--play-process
    (kill-process piper--play-process)
    (setq piper--play-process nil)))

(defun piper-resume ()
  "Resume playback."
  (interactive)
  (piper--log "Resuming playback")
  (when piper--paused
    (setq piper--paused nil)
    (if piper--current-batch
        (let ((wav-files (mapcar 'car piper--current-batch)))
          (piper--log "Resuming current batch")
          (piper--play-batch piper--current-batch wav-files))
      (piper--play-next-chunk))))

(defun piper-toggle-pause ()
  "Toggle pause/resume."
  (interactive)
  (if piper--paused
      (piper-resume)
    (piper-pause)))

(defun piper--cleanup ()
  "Clean up all resources."
  (piper--log "Cleaning up resources")
  
  ;; Stop processing flag first to prevent sentinels from restarting things
  (setq piper--chunk-processing nil)
  
  (piper--remove-highlight)
  
  (when (and piper--current-process (process-live-p piper--current-process))
    (piper--log "Killing piper process")
    (kill-process piper--current-process))
  (setq piper--current-process nil)
  
  (when (and piper--play-process (process-live-p piper--play-process))
    (piper--log "Killing play process")
    (kill-process piper--play-process))
  (setq piper--play-process nil)

  (dolist (chunk-data piper--audio-queue)
    (let ((wav-file (if (listp chunk-data) (car chunk-data) chunk-data)))
      (piper--cleanup-temp-wav wav-file)))
  (setq piper--audio-queue nil)

  (when piper--current-batch
    (dolist (chunk piper--current-batch)
      (let ((wav-file (car chunk)))
        (piper--cleanup-temp-wav wav-file)))
    (setq piper--current-batch nil))

  ;; Cancel all active timers
  (dolist (timer piper--active-timers)
    (when timer
      (cancel-timer timer)))
  (setq piper--active-timers nil)

  (setq piper--current-text nil)
  (setq piper--chunk-queue nil)
  (setq piper--current-batch nil)
  (setq piper--current-chunk-index 0)
  (setq piper--total-chunks 0)
  (setq piper--original-text nil))

(provide 'piper-tts)
;;; piper-tts.el ends here
