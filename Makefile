EMACS = emacs

check: compile
	$(EMACS) -q -batch -l langtool.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l langtool.elc \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile langtool.el
