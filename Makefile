EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .

.PHONY: all clean compile test

all: clean compile test

clean:
	rm -f *.elc

compile:
	$(BATCH) -f batch-byte-compile *.el

test:
	$(BATCH) -l ert -l piper-mode-tests.el -f ert-run-tests-batch-and-exit
