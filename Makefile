EMACS = emacs

check: compile
	$(EMACS) -q -batch -l .test-init.el -l langtool.el -l langtool-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l .test-init.el -l langtool.elc -l langtool-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile \
		langtool.el
