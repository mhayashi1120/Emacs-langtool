-include env.mk

LOAD_PATH = -L . 
LOAD_PATH += $(POPUP_EL_PATH)

EMACS ?= emacs
BATCH := $(EMACS) -Q -batch $(LOAD_PATH)

EL = langtool.el
EL += langtool-popup.el
ELC := $(EL:%.el=%.elc)

all: compile

check: compile
	$(BATCH) $(EL:%=-l %)  -l .test-init.el -l langtool-test.el \
		-f ert-run-tests-batch-and-exit
	$(BATCH) $(ELC:%=-l %)  -l .test-init.el -l langtool-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(BATCH) -f batch-byte-compile $(EL)

clean:
	rm -f $(ELC)
