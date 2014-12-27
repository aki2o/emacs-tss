PACKAGE-NAME:=tss
VERSION:=$(shell perl -ne 'print $$1 if /;; Version: +(.*)/' tss.el)
PACKAGE:=${PACKAGE-NAME}-${VERSION}
EMACS ?= emacs
CASK ?= cask

ELPA_DIR := \
	.cask/$(shell $(EMACS) -Q --batch --eval '(princ emacs-version)')/elpa

all:
	$(MAKE) test

compile: elpa
	$(CASK) exec $(EMACS) -Q -L . -batch -f batch-byte-compile \
	  tss.el typescript.el

test: elpa
	#  -l test/ac-candidates.el
	#  -l test/process.el
	#  -l test/receive-server-response.el
	#  -l test/popup-help.el
	$(CASK) exec $(EMACS) -Q --batch -L . \
	  -l test/active-p.el \
	  -l test/balance-json-brace-p.el \
	  -l test/get-server-response.el \
	  -l test/run-flymake.el \
	  -f batch-expectations

clean:
	rm -f *.elc

elpa: $(ELPA_DIR)
$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

.PHONY: all compile test clean package
