EMACS = emacs
BATCH = $(EMACS) -q -batch


check: compile
	$(BATCH) -l langtool.el -l .test-init.el -l langtool-test.el \
		-f ert-run-tests-batch-and-exit
	$(BATCH) -l langtool.elc -l .test-init.el -l langtool-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile \
		langtool.el

