EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .

.PHONY: all clean package test

all: clean package

clean:
	rm -f *.elc

package:
	$(BATCH) -f batch-byte-compile *.el

test:
	$(BATCH) -l ert -l piper-mode-tests.el -f ert-run-tests-batch-and-exit

# Install dependencies for development
deps:
	mkdir -p models
	cd models && curl -L -O https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/joe/medium/en_US-joe-medium.onnx
	cd models && curl -L -O https://huggingface.co/rhasspy/piper-voices/resolve/main/en/en_US/joe/medium/en_US-joe-medium.onnx.json
