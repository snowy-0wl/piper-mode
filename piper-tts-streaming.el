;; This is a temporary file to build the streaming version correctly
;; We'll copy sections from here into the main file

;; Key fix: Remove cleanup check from sox sentinel
;; Only check for completion in the generation sentinel

(defun piper--stream-chunk-to-pipe (wav-file)
  "Convert WAV-FILE to raw PCM and stream to audio pipe."
  (let ((sox-cmd (format "sox '%s' -t raw -b 16 -e signed -r 22050 -c 1 - >> '%s'"
                        wav-file piper--audio-pipe)))
    (piper--log "Streaming command: %s" sox-cmd)
    (make-process
     :name "piper-sox"
     :command (list "sh" "-c" sox-cmd)
     :sentinel (lambda (proc event)
                (piper--log "Sox stream event: %s" (string-trim event))
                (when (string-match-p "\\(finished\\|exited\\)" event)
                  ;; Cleanup WAV file after streaming  
                  (piper--cleanup-temp-wav wav-file)
                  
                  ;; Trigger next chunk
                  (piper--play-next-chunk)
                  
                  ;; Trigger generation (to refill buffer)
                  (piper--generate-next-chunk))))))

;;; Cleanup check should go in the generator after last chunk
(defun piper--generate-next-chunk ()
  "Producer: Generate the next audio chunk if needed."
  ;; ... existing code ...
  
  ;; At the end, where it says "No more chunks to generate":
  (piper--log "No more chunks to generate")
  ;; Schedule cleanup after a delay to let final audio play
  (run-with-timer 2.0 nil 
                  (lambda ()
                    (when (and (null piper--chunk-queue)
                               (null piper--audio-queue))
                      (piper--log "All playback complete, cleaning up")
                      (piper--cleanup)))))
