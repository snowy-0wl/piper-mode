;;; piper-text.el --- Text processing for piper-mode -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Keywords: multimedia, tts, accessibility, speech

;;; Commentary:
;; Text processing, chunking, and boundary detection for piper-mode.

;;; Code:

(require 'piper-infra)

;; Internal chunking constants
(defconst piper--chunk-size 1500
  "Optimal number of characters to process in a single TTS chunk.")

(defconst piper--chunk-overlap 100
  "Number of characters to overlap between chunks for sentence boundary detection.")

;; Chunking variables
(defvar piper--chunk-queue nil
  "Queue of text chunks waiting to be processed.")

(defvar piper--current-chunk-index 0
  "Index of the current chunk being processed.")

(defvar piper--total-chunks 0
  "Total number of chunks for the current text.")

(defvar piper--original-text nil
  "Original complete text before chunking.")

(defvar piper--chunk-processing nil
  "Whether we're currently processing chunks.")

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

(defun piper--line-indentation ()
  "Get the indentation level of the current line."
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

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

(provide 'piper-text)
;;; piper-text.el ends here
