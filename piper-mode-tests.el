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
  (let ((piper--chunk-size 10)
        (piper--chunk-overlap 2))
    ;; Short text should not be chunked
    (should (equal (piper--chunk-text "Short") '("Short")))
    
    ;; Long text should be chunked
    (let ((chunks (piper--chunk-text "This is a longer text that should be chunked")))
      (should (> (length chunks) 1)))))

(provide 'piper-mode-tests)
