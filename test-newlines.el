;;; test-newlines.el --- Test newline handling in TTS -*- lexical-binding: t -*-

(require 'ert)

;; Stage 1: Unfill hard-wrapped paragraphs
(defun piper--unfill-paragraphs (text)
  "Merge hard-wrapped lines within paragraphs, preserve paragraph structure."
  (with-temp-buffer
    (insert text)
    (let ((fill-column most-positive-fixnum))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

;; Stage 2: Convert remaining newlines to pauses
(defun piper--newlines-to-pauses (text)
  "Convert semantic single newlines to punctuation for TTS pauses."
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
                 ;; Default: light pause
                 (t ", "))))
          (delete-char -1)  ; Remove the \n
          (insert pause))))
    (buffer-string)))

;; Combined function
(defun piper--process-newlines (text)
  "Process text for TTS: unfill hard-wrapping, inject pauses for semantic breaks."
  (piper--unfill-paragraphs 
   (piper--newlines-to-pauses text)))  ; Pauses FIRST, then unfill


;;; Test Cases

(ert-deftest test-hard-wrapped-paragraph ()
  "Test that hard-wrapped paragraphs merge into flowing text."
  (let ((input "This is a sentence that has been hard-wrapped at exactly
eighty characters because someone used fill-paragraph with a
narrow column setting for their text editor preferences.")
        (expected "This is a sentence that has been hard-wrapped at exactly eighty characters because someone used fill-paragraph with a narrow column setting for their text editor preferences."))
    (let ((result (piper--unfill-paragraphs input)))
      (message "HARD-WRAPPED TEST:")
      (message "Input:\n%s" input)
      (message "Output:\n%s" result)
      (should (string= result expected)))))

(ert-deftest test-heading-preservation ()
  "Test that headings get proper pauses."
  (let ((input "End of previous section.
New Section Heading
Start of new section here."))
    (let ((result (piper--process-newlines input)))
      (message "\nHEADING TEST:")
      (message "Input:\n%s" input)
      (message "Output:\n%s" result)
      ;; Should have pauses around the heading
      (should (string-match-p "section\\." result))
      (should (string-match-p "Heading\\." result)))))

(ert-deftest test-list-items ()
  "Test that list items get separated."
  (let ((input "Here are the items:
- First item
- Second item
- Third item"))
    (let ((result (piper--process-newlines input)))
      (message "\nLIST TEST:")
      (message "Input:\n%s" input)
      (message "Output:\n%s" result)
      (should (string-match-p "items:" result))
      (should (string-match-p "First item" result)))))

(ert-deftest test-paragraph-breaks ()
  "Test that paragraph breaks (double newlines) are preserved."
  (let ((input "First paragraph here.

Second paragraph here."))
    (let ((result (piper--process-newlines input)))
      (message "\nPARAGRAPH BREAK TEST:")
      (message "Input:\n%s" input)
      (message "Output:\n%s" result)
      ;; Should still have separation
      (should (string-match-p "here\\." result)))))

(ert-deftest test-report-structure ()
  "Test a realistic report structure."
  (let ((input "Executive Summary
This report provides an analysis of the quarterly results which
have been compiled from various departments and reviewed by the
management team for accuracy and completeness.

Key Findings
Revenue increased by 15 percent during this period.
Costs were reduced through efficiency improvements.

Conclusion
The quarter was successful overall."))
    (let ((result (piper--process-newlines input)))
      (message "\nREPORT TEST:")
      (message "Input:\n%s" input)
      (message "Output:\n%s" result)
      (message "---")
      ;; Headings should appear in output
      (should (string-match-p "Summary" result))
      (should (string-match-p "Findings" result))
      (should (string-match-p "Conclusion" result)))))

(ert-deftest test-mixed-content ()
  "Test complex mixed content with various line break types."
  (let ((input "Chapter 1: Introduction
The topic of artificial intelligence has gained significant
attention in recent years as computational power has increased
and new algorithms have been developed by researchers worldwide.

Key Concepts
- Machine learning
- Neural networks
- Deep learning

These concepts form the foundation."))
    (let ((result (piper--process-newlines input)))
      (message "\nMIXED CONTENT TEST:")
      (message "Input:\n%s" input)
      (message "Output:\n%s" result)
      (should (string-match-p "Introduction" result)))))

;; Interactive test function
(defun test-newlines-interactive ()
  "Run all tests and display results."
  (interactive)
  (let ((test-buffer (get-buffer-create "*Newline Test Results*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (insert "=== NEWLINE HANDLING TEST RESULTS ===\n\n"))
    (ert-run-tests-interactively "test-")
    (display-buffer test-buffer)))

(provide 'test-newlines)
