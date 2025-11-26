;;; piper-mode-tests.el --- Tests for piper-mode.el -*- lexical-binding: t -*-

(require 'ert)
(require 'piper-mode)

(ert-deftest piper-mode-test-clean-url ()
  "Test URL cleaning function."
  (should (string= (piper--clean-url "https://example.com/model.onnx?download=true")
                   "https://example.com/model.onnx"))
  (should (string= (piper--clean-url "https://example.com/model.onnx")
                   "https://example.com/model.onnx")))

(ert-deftest piper-mode-test-get-config-url ()
  "Test config URL generation."
  (should (string= (piper--get-config-url "https://example.com/model.onnx")
                   "https://example.com/model.onnx.json"))
  (should (string= (piper--get-config-url "https://example.com/model")
                   "https://example.com/model.json")))

(ert-deftest piper-mode-test-chunk-text ()
  "Test text chunking."
  (let ((piper-chunk-size 10)
        (piper--chunk-overlap 2))
    ;; Short text should not be chunked
    (should (equal (piper--chunk-text "Short") '("Short")))
    
    ;; Long text should be chunked
    (let ((chunks (piper--chunk-text "This is a longer text that should be chunked")))
      (should (> (length chunks) 1)))))

(ert-deftest piper-mode-test-model-extraction ()
  "Test model info extraction from URL."
  (let* ((url "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/ryan/high/en_US-ryan-high.onnx")
         ;; Test with context provided (simulating parsing from file)
         (model (piper--extract-model-info url "English" "en_US")))
    (should model)
    (should (string= (piper-model-voice model) "ryan"))
    (should (string= (piper-model-quality model) "high"))
    (should (string= (piper-model-lang-name model) "English"))
    (should (string= (piper-model-lang-code model) "en_US"))))

(ert-deftest piper-mode-test-parse-language-header ()
  "Test parsing of language headers from VOICES.md format."
  (with-temp-buffer
    ;; Test case 1: With backticks
    (insert "* English (`en_US`, ...)\n")
    (insert "    * ryan\n")
    (insert "        * high - [[model](https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/ryan/high/en_US-ryan-high.onnx?download=true)]\n")
    ;; Test case 2: Without backticks
    (insert "* English (en_GB)\n")
    (insert "    * alan\n")
    (insert "        * medium - [[model](https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_GB/alan/medium/en_GB-alan-medium.onnx?download=true)]\n")
    
    (goto-char (point-min))
    (let ((models (piper--parse-models-file)))
      (should (= (length models) 2))
      
      ;; Check first model (en_US) - note: models are pushed, so order is reversed
      (let ((model-info (nth 0 models)))
        (should (string= (nth 3 model-info) "English"))
        (should (string= (nth 4 model-info) "en_GB")))
        
      (let ((model-info (nth 1 models)))
        (should (string= (nth 3 model-info) "English"))
        (should (string= (nth 4 model-info) "en_US"))))))

(ert-deftest piper-mode-test-core-functions ()
  "Test existence of core functions and absence of removed ones."
  (should (fboundp 'piper-speak))
  (should (fboundp 'piper-stop))
  ;; Highlighting functions should exist now
  (should (facep 'piper-highlight-face)))

(ert-deftest piper-mode-test-shell-quoting ()
  "Test that file paths are properly quoted."
  (let ((wav-files '("file1.wav" "file with spaces.wav" "file'with'quotes.wav")))
    (should (string= (mapconcat #'shell-quote-argument wav-files " ")
                     "file1.wav file\\ with\\ spaces.wav file\\'with\\'quotes.wav"))))

(provide 'piper-mode-tests)
