# This makefile runs the tests as Travis runs them.  Be sure to test
# locally before you push if you are under the impression that the
# patch should work.  This will cut down on the number of commits in
# the repository that, essentially, patch patches.

all: 24.1 24.2 24.3

%:
	evm install emacs-$@-bin || true
	emacs --version
	emacs --batch -L . -l ert -l test/tests.el -f ert-run-tests-batch-and-exit
