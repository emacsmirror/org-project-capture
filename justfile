# org-project-capture justfile

cask := "cask"
emacs := `which emacs`

# Default recipe - show available commands
default:
    @just --list

# Install dependencies
install:
    {{cask}} install

# Compile all elisp files
compile: install
    {{cask}} build --verbose

# Remove compiled files
clean-elc:
    rm -f *.elc

# Recompile from scratch
recompile: clean-elc compile

# Remove all build artifacts
clean: clean-elc
    rm -rf .cask/

# Run tests
test: recompile
    {{cask}} exec ert-runner -L .

# Run tests without recompiling
test-only:
    {{cask}} exec ert-runner -L .

# Lint elisp files
lint: install
    {{cask}} exec {{emacs}} --batch -L . \
        --eval "(require 'checkdoc)" \
        --eval "(setq checkdoc-arguments-in-order-flag nil)" \
        -f checkdoc-file *.el
