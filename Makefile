EMACS ?= emacs
ELFILES := $(wildcard gptel-runner*.el)
TESTFILES := $(filter-out test/gptel-runner-gptel-test.el test/gptel-runner-delegation-test.el test/gptel-runner-session-test.el test/gptel-runner-request-budget-test.el,$(wildcard test/*-test.el))
INTEGRATION_LOAD_PATH ?=

.PHONY: test integration-test compile checkdoc package-lint clean release-check

test: clean
	$(EMACS) -Q --batch -L . -L test $(foreach file,$(TESTFILES),-l $(file)) \
	  -f ert-run-tests-batch-and-exit

integration-test:
	$(EMACS) -Q --batch $(INTEGRATION_LOAD_PATH) -L . -L test \
	  --eval '(setq load-prefer-newer t)' \
	  --eval "(require 'gptel-runner-instructions-integration)" \
	  --eval "(require 'gptel-runner-request-budget-test)" \
	  -f ert-run-tests-batch-and-exit
	$(EMACS) -Q --batch $(INTEGRATION_LOAD_PATH) -L . -L test \
	  --eval '(setq load-prefer-newer t)' \
	  --eval '(unless (locate-library "gptel-agent") (message "Skipping optional gptel-agent delegation tests") (kill-emacs 0))' \
	  -l test/gptel-runner-request-budget-delegation.el \
	  -l test/gptel-runner-question-integration.el \
	  --eval '(ert-run-tests-batch-and-exit "^gptel-runner-delegation-")'

compile: clean
	$(EMACS) -Q --batch -L . --eval \
	  "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(ELFILES)

checkdoc:
	$(EMACS) -Q --batch -L . --eval \
	  "(progn (require 'checkdoc) (dolist (f command-line-args-left) (checkdoc-file f)))" \
	  $(ELFILES)

package-lint:
	$(EMACS) -Q --batch -L . -l package-lint \
	  --eval "(progn (package-initialize) (add-to-list 'package-alist (cons 'gptel (list (package-desc-create :name 'gptel :version '(0 9 9 6) :summary \"gptel\" :kind 'single :dir default-directory)))) (cl-letf (((symbol-function 'package-initialize) #'ignore)) (kill-emacs (if (package-lint-batch-and-exit-1 '(\"gptel-runner.el\")) 0 1))))"

clean:
	find . -name '*.elc' -delete

release-check: test compile checkdoc
	@test -z "$$(git status --porcelain)" || \
	  { echo "release-check changed or found untracked files"; git status --short; exit 1; }
