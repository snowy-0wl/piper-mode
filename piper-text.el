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

(defcustom piper-smart-newline-handling t
  "Whether to intelligently handle newlines for better TTS flow.
If non-nil:
1. Adds pauses after headings and list items
2. Merges hard-wrapped paragraphs into single lines
This prevents choppy playback for hard-wrapped text while preserving
structure for headings and lists."
  :type 'boolean
  :group 'piper)

(defun piper--normalize-text (text)
  "Normalize TEXT by repairing encoding issues and optimizing newlines."
  (let ((result text))
    ;; 1. Smart newline handling (if enabled)
    (when piper-smart-newline-handling
      (setq result (piper--smart-punctuate result))
      (setq result (piper--unfill-text result)))

    ;; 2. Optional encoding fix
    (when piper-fix-encoding
      ;; Check for likely Mojibake (Uppercase Accents 0xC0-0xD6)
      ;; Also check for Emacs "eight-bit" chars (0x3FFFC0-0x3FFFD6)
      ;; And ensure no valid Lowercase Accents (0xDF-0xF6, 0xF8-0xFF) are present
      ;; (including their eight-bit versions) to avoid breaking valid languages.
      (let ((case-fold-search nil))
        (if (and (string-match-p (format "[%c-%c%c-%c]" 
                                       #xC0 #xD6 
                                       #x3FFFC0 #x3FFFD6) text)
                 (not (string-match-p (format "[%c-%c%c-%c%c-%c%c-%c]" 
                                            #xDF #xF6 #xF8 #xFF
                                            #x3FFFDF #x3FFFF6 #x3FFFF8 #x3FFFFF) text)))
            (progn
              (piper--log "Applying encoding fix for likely Mojibake in: %s" text)
              (setq result (replace-regexp-in-string
                            (format "[%c-%c%c-%c]+" 
                                    #xC0 #xD6
                                    #x3FFFC0 #x3FFFD6)
                            (lambda (match)
                              (decode-coding-string 
                               (encode-coding-string match 'latin-1) 
                               'mac-roman))
                            result t)))
          (piper--log "Skipping encoding fix (no Mojibake or valid Latin-1 present): %s" text))))
    result))

(defun piper--smart-punctuate (text)
  "Add punctuation to headings and list items to force TTS pauses.
Scans for semantic lines that should have pauses but don't."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (unless (looking-at "\n")  ; Skip if next char is also newline (paragraph break)
        (let* ((before-line (save-excursion 
                             (forward-line -1)
                             (buffer-substring (line-beginning-position) 
                                              (line-end-position))))
               (after-line (buffer-substring (line-beginning-position)
                                            (line-end-position)))
               ;; Decide what pause to inject
               (pause 
                (cond
                 ;; Next line looks like heading: short, starts with capital/symbol
                 ((and (< (length after-line) 60)
                       (string-match-p "^[A-Z]" after-line)
                       (not (string-match-p "^[A-Z][a-z]+ " after-line))) ; Not regular sentence
                  ". ")
                 ;; Current line looks like heading ending
                 ((and (< (length before-line) 60)
                       (string-match-p "^[A-Z#*0-9-]" before-line)
                       (not (string-match-p "[.!?;:]\\s-*$" before-line))) ; No punctuation
                  ". ")
                 ;; Line ended with sentence punctuation  
                 ((string-match-p "[.!?]\\s-*$" before-line) ", ")
                 ;; Line ended with colon (list intro)
                 ((string-match-p ":\\s-*$" before-line) ". ")
                 ;; List item markers
                 ((string-match-p "^[-*]\\s-" after-line) ". ")
                 ;; Default: no pause injection, let unfill handle it
                 (t nil))))
          (when pause
            (delete-char -1)  ; Remove the \n
            (insert pause)))))
    (buffer-string)))

(defun piper--unfill-text (text)
  "Merge hard-wrapped lines within paragraphs, preserving structure."
  (with-temp-buffer
    (insert text)
    (let ((fill-column most-positive-fixnum))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

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
  ;; Allow 20% tolerance to avoid splitting just for a few characters
  (let ((limit (* piper-chunk-size 1.2)))
    (if (<= (length text) limit)
        (list text)
      (let ((chunks nil)
            (chunk-start 0)
            (text-length (length text)))
        
        ;; While we have more than a chunk + tolerance remaining
        (while (< (+ chunk-start limit) text-length)
          (let* ((chunk-end (+ chunk-start piper-chunk-size))
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
        (nreverse chunks)))))

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

    ;; Strategy 3: Look for clause boundary (comma, etc.)
    (when (not result)
      (let ((clause-end (piper--find-clause-boundary text position min-pos)))
        (when clause-end
          (piper--log "Found clause end at %d" clause-end)
          (setq result clause-end))))
    
    ;; Fallback: try to find word boundary near position
    (when (not result)
      (setq result (piper--find-word-boundary text position min-pos)))
    
    ;; Last resort: use position as-is
    (or result
        (progn
          (piper--log "No boundary found, hard cutting at %d" position)
          position))))

(defun piper--find-word-boundary (text position min-pos)
  "Find a word boundary in TEXT near POSITION, preferring the closest one.
Searches up to 50 characters forward and backward."
  (let* ((search-range 50)
         (start (max min-pos (- position search-range)))
         (end (min (length text) (+ position search-range)))
         (substr (substring text start end))
         (offset (- position start)))
    (with-temp-buffer
      (insert substr)
      (let ((forward-match nil)
            (backward-match nil))
        
        ;; Look forward
        (goto-char (1+ offset))
        (when (re-search-forward "\\s-" nil t)
          ;; Buffer positions are 1-indexed, convert to 0-indexed string position
          (setq forward-match (+ start (1- (point)))))
          
        ;; Look backward
        (goto-char (1+ offset))
        (when (re-search-backward "\\s-" nil t)
          ;; Buffer positions are 1-indexed, convert to 0-indexed string position
          (setq backward-match (+ start (1- (match-end 0)))))
          
        ;; Return the closest match
        (cond
         ((and forward-match backward-match)
          (if (< (- forward-match position) (- position backward-match))
              forward-match
            backward-match))
         (forward-match forward-match)
         (backward-match backward-match)
         (t nil))))))

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
Handles common abbreviations and quotes/parens to avoid incorrect splits."
  (let ((substr (substring text start-pos (min (length text) end-pos)))
        (match-idx 0)
        (found nil)
        (result nil))
    (save-match-data
      (while (and (not found)
                  ;; Match .!? followed by optional quotes/parens, then whitespace
                  (string-match "[.!?]['\"”’)]*\\s-" substr match-idx))
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
      ;; Match .!? followed by optional quotes/parens, then whitespace
      (while (string-match "[.!?]['\"”’)]*\\s-" substr start)
        (let ((match-pos (match-end 0))
              (match-start (match-beginning 0)))
          ;; Check for abbreviations
          (if (piper--is-abbreviation-p substr match-start)
              (setq start match-pos) ;; Skip this match
            (setq last-match match-pos
                  start match-pos))))
      (when last-match
        (+ min-pos last-match)))))

(defun piper--find-clause-boundary (text position min-pos)
  "Find a clause boundary (comma, semicolon, colon) in TEXT near POSITION.
Prefers looking back from POSITION, but will look forward slightly."
  (let* ((search-range 30)
         (start (max min-pos (- position search-range)))
         (end (min (length text) (+ position search-range)))
         (substr (substring text start end))
         (offset (- position start)))
    (with-temp-buffer
      (insert substr)
      (let ((forward-match nil)
            (backward-match nil))
        
        ;; Look forward for comma/semicolon/colon followed by space
        (goto-char (1+ offset))
        (when (re-search-forward "[,;:][ \t\n]" nil t)
          ;; Buffer positions are 1-indexed, convert to 0-indexed string position
          (setq forward-match (+ start (1- (point)))))
          
        ;; Look backward
        (goto-char (1+ offset))
        (when (re-search-backward "[,;:][ \t\n]" nil t)
          ;; Buffer positions are 1-indexed, convert to 0-indexed string position
          (setq backward-match (+ start (1- (match-end 0)))))
          
        ;; Return the closest match, preferring backward if similar distance
        (cond
         ((and forward-match backward-match)
          (if (< (- forward-match position) (- position backward-match))
              forward-match
            backward-match))
         (forward-match forward-match)
         (backward-match backward-match)
         (t nil))))))

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
