# This makefile runs the tests as Travis runs them.  Be sure to test
# locally before you push if you are under the impression that the
# patch should work.  This will cut down on the number of commits in
# the repository that, essentially, patch patches.
#
# To test Emacs 24.1, for example, use
#
#     make 1
#
# To test on all versions, of course, simply use
#
#     make
#
# or
#
#     make all
#

VERSIONS = 1 2 3 4

EVM_LOCATION := $(shell which evm)
CASK_LOCATION := $(shell which cask)

all :: $(VERSIONS)

$(VERSIONS) :: clean
	evm install emacs-24.$@-bin --skip || true
	evm use emacs-24.$@-bin
	emacs --version
	cask install
	emacs --batch -L . -l ert -l test/tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -rf .sx/
	cask clean-elc

install_cask:
ifdef CASK_LOCATION
	$(info cask is already installed!)
else
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python
endif

install_evm:
ifdef EVM_LOCATION
	$(info evm is already installed!)
else
	curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
endif

# Local Variables:
# indent-tabs-mode: t
# End:
