EMACS ?= emacs
CASK ?= cask

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile:
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" tss.el

test:
	ret=0
	for f in $$(find test -type f -name "*.el"); do \
		${CASK} exec ${EMACS} -Q --batch -L . -l $$f -f batch-expectations || ret=1; \
	done
	test $$ret -eq 0

clean:
	rm -f tss.elc

.PHONY: all compile test clean
