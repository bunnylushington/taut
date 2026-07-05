# Taut - Premium Slack Client for Emacs Task Runner
default:
    @just --list

# Run all ERT unit tests
test:
    emacs -batch -L . -l test/test-runner.el -f ert-run-tests-batch-and-exit

# Byte-compile all Source Lisp files cleanly
compile:
    emacs -batch -L . -f batch-byte-compile *.el

# Delete all byte-compiled .elc files
clean:
    rm -f *.elc test/*.elc
