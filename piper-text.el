;;; piper-text.el --- Text processing for piper-mode -*- lexical-binding: t -*-

;; Author: snowy-0wl
;; Maintainer: snowy-0wl
;; Keywords: multimedia, tts, accessibility, speech

;;; Commentary:
;; Text processing, chunking, and boundary detection for piper-mode.

;;; Code:

(require 'piper-infra)

(defconst piper--chunk-overlap 100
  "Number of characters to overlap between chunks for sentence boundary detection.")

(defcustom piper-fix-encoding nil
  "Whether to attempt to fix encoding issues (Mojibake).
If non-nil, attempts to repair text that looks like Mac Roman interpreted as Latin-1.
This is useful if you see characters like \\322, \\323, etc. in the logs.
Note: This fix is safely skipped for text containing non-Latin-1 characters
(e.g. Cyrillic, Chinese), but may incorrectly alter valid Latin-1 text."
  :type 'boolean
  :group 'piper)

;; Internal chunking constants
(defcustom piper-chunk-size 200
  "Optimal number of characters to process in a single TTS chunk.
Larger chunks provide better context for intonation but increase initial
latency."
  :type 'integer
  :group 'piper)

(defun piper--normalize-text (text)
  "Normalize TEXT by repairing encoding issues if enabled."
  (let ((result text))
    ;; Optional encoding fix
    (when (and piper-fix-encoding
               (let ((systems (find-coding-systems-string text)))
                 (or (memq 'undecided systems)
                     (memq 'iso-latin-1 systems))))
      ;; Check for lowercase accented characters (Latin-1 0xDF-0xF6, 0xF8-0xFF)
      ;; These indicate valid Latin-1 text (French, Spanish, etc.)
      ;; Mojibake usually manifests as Uppercase accents (0xC0-0xD6) or symbols.
      ;; We construct the regex dynamically to ensure correct character matching.
      (if (string-match-p (format "[%c-%c%c-%c]" #xDF #xF6 #xF8 #xFF) text)
          (piper--log "Skipping encoding fix for valid Latin-1 text: %s" text)
        (piper--log "Applying encoding fix for likely Mojibake: %s" text)
        (setq result (decode-coding-string 
                      (encode-coding-string result 'latin-1) 
                      'mac-roman))))
    result))

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
  (piper--log "Chunking text: length=%d, chunk-size=%d" (length text) piper-chunk-size)
  (if (<= (length text) piper-chunk-size)
      ;; If text is smaller than chunk size, return the whole text
      (list text)
    (let ((chunks nil)
          (chunk-start 0)
          (text-length (length text)))
      
      ;; Process all chunks except the last one
      (while (< (+ chunk-start piper-chunk-size) text-length)
        (let* ((chunk-end (+ chunk-start piper-chunk-size))
               ;; Try to find a good boundary (sentence or paragraph end)
               ;; Must be strictly greater than chunk-start
               (boundary (piper--find-chunk-boundary text chunk-end chunk-start)))
          
          ;; Ensure we make progress
          (when (<= boundary chunk-start)
            (piper--log "Warning: boundary %d <= start %d, forcing split at %d" 
                        boundary chunk-start chunk-end)
            (setq boundary chunk-end))
            
          (push (substring text chunk-start boundary) chunks)
          (setq chunk-start boundary)))
      
      ;; Add the last chunk
      (when (< chunk-start text-length)
        (push (substring text chunk-start) chunks))
      
      ;; Return chunks in the correct order
      (nreverse chunks))))

(defun piper--find-chunk-boundary (text position min-pos)
  "Find a good boundary in TEXT near POSITION, but not before MIN-POS.
Looks for sentence or paragraph ends to avoid mid-sentence cuts.
Prefers to look ahead for the next sentence ending first."
  (let ((text-length (length text))
        (lookahead 100)  ; Look ahead up to 100 chars for next sentence
        (result nil))
      
    ;; Don't go beyond text length
    (when (>= position text-length)
      (setq position (1- text-length)))
    
    ;; Strategy 1: Look ahead for the next sentence/paragraph end (up to 50 chars)
    (let ((search-end (min text-length (+ position lookahead))))
      ;; Try paragraph end first (looking ahead)
      (let ((para-end (piper--find-next-paragraph-end text position search-end)))
        (when para-end
          (piper--log "Found paragraph end ahead at %d" para-end)
          (setq result para-end)))
      
      ;; Try sentence end if no paragraph found
      (when (not result)
        (let ((sent-end (piper--find-next-sentence-end text position search-end)))
          (when sent-end
            (piper--log "Found sentence end ahead at %d" sent-end)
            (setq result sent-end)))))
    
    ;; Strategy 2: If no boundary ahead, look back within our chunk
    (when (not result)
      (let ((para-end (piper--find-paragraph-end text position min-pos)))
        (when para-end
          (piper--log "Found paragraph end back at %d" para-end)
          (setq result para-end)))
      
      (when (not result)
        (let ((sent-end (piper--find-sentence-end text position min-pos)))
          (when sent-end
            (piper--log "Found sentence end back at %d" sent-end)
            (setq result sent-end)))))
    
    ;; Fallback: try to find word boundary near position
    (when (not result)
      (setq result (piper--find-word-boundary text position min-pos)))
    
    ;; Last resort: use position as-is
    (or result
        (progn
          (piper--log "No boundary found, hard cutting at %d" position)
          position))))

(defun piper--find-word-boundary (text position min-pos)
  "Find a word boundary in TEXT near POSITION, preferring to look back."
  (let ((search-start (max min-pos (- position 20)))
        (search-end (min (length text) (+ position 20))))
    (save-match-data
      (let ((substr (substring text search-start search-end))
            (offset (- position search-start)))
        ;; Look for whitespace before position
        (if (string-match "\\s-" substr offset)
            (+ search-start (match-end 0))
          ;; Look for whitespace after position
          (when (string-match "\\s-" substr)
            (+ search-start (match-end 0))))))))

(defun piper--find-next-paragraph-end (text start-pos end-pos)
  "Find first paragraph end in TEXT between START-POS and END-POS."
  (let ((substr (substring text start-pos (min (length text) end-pos))))
    (save-match-data
      (when (string-match "\\n\\n" substr)
        (+ start-pos (match-end 0))))))

(defun piper--is-abbreviation-p (text pos)
  "Check if the period at POS in TEXT is part of an abbreviation."
  (let ((abbrevs '("Mr" "Mrs" "Ms" "Dr" "Prof" "Sr" "Jr" "St" "vs" "etc" "e.g" "i.e")))
    (cl-some (lambda (abbr)
               (let ((abbr-len (length abbr)))
                 (and (>= pos abbr-len)
                      (string= (substring text (- pos abbr-len) pos) abbr)
                      ;; Check word boundary before abbreviation
                      (or (= (- pos abbr-len) 0)
                          (string-match-p "\\s-" (substring text (- pos abbr-len 1) (- pos abbr-len)))))))
             abbrevs)))

(defun piper--find-next-sentence-end (text start-pos end-pos)
  "Find first sentence end in TEXT between START-POS and END-POS.
Handles common abbreviations to avoid incorrect splits."
  (let ((substr (substring text start-pos (min (length text) end-pos)))
        (match-idx 0)
        (found nil)
        (result nil))
    (save-match-data
      (while (and (not found)
                  (string-match "[.!?]\\s-" substr match-idx))
        (let ((match-pos (match-end 0))
              (match-start (match-beginning 0)))
          ;; Check for abbreviations
          (if (piper--is-abbreviation-p substr match-start)
              (setq match-idx match-pos) ;; Skip this match
            (setq found t
                  result (+ start-pos match-pos))))))
    result))

(defun piper--find-paragraph-end (text position min-pos)
  "Find last paragraph end in TEXT between MIN-POS and POSITION."
  (let ((substr (substring text min-pos (min (length text) position)))
        (last-match nil)
        (start 0))
    (save-match-data
      (while (string-match "\\n\\n" substr start)
        (setq last-match (match-end 0))
        (setq start (match-end 0)))
      (when last-match
        (+ min-pos last-match)))))

(defun piper--find-sentence-end (text position min-pos)
  "Find last sentence end in TEXT between MIN-POS and POSITION."
  (let ((substr (substring text min-pos (min (length text) position)))
        (last-match nil)
        (start 0))
    (save-match-data
      (while (string-match "[.!?]\\s-" substr start)
        (setq last-match (match-end 0))
        (setq start (match-end 0)))
      (when last-match
        (+ min-pos last-match)))))

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
