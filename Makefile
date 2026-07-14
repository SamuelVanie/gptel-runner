EMACS ?= emacs
ELFILES := $(wildcard gptel-runner*.el)
TESTFILES := $(wildcard test/*-test.el)

.PHONY: test compile checkdoc package-lint clean release-check

test: clean
	$(EMACS) -Q --batch -L . -L test -l test/gptel-runner-test.el \
	  -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -Q --batch -L . --eval \
	  "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(ELFILES)

checkdoc:
	$(EMACS) -Q --batch -L . --eval \
	  "(progn (require 'checkdoc) (dolist (f command-line-args-left) (checkdoc-file f)))" \
	  $(ELFILES)

package-lint:
	$(EMACS) -Q --batch -L . -l package-lint \
	  --eval "(progn (package-initialize) (add-to-list 'package-alist (cons 'gptel (list (package-desc-create :name 'gptel :version '(0 9 9 4) :summary \"gptel\" :kind 'single :dir default-directory)))) (cl-letf (((symbol-function 'package-initialize) #'ignore)) (kill-emacs (if (package-lint-batch-and-exit-1 '(\"gptel-runner.el\")) 0 1))))"

clean:
	find . -name '*.elc' -delete

release-check: test compile checkdoc
	@test -z "$$(git status --porcelain)" || \
	  { echo "release-check changed or found untracked files"; git status --short; exit 1; }
