SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

PKG-FILES := ts-fold.el

TEST-FILES := $(shell ls test/ts-fold-*.el)

.PHONY: clean checkdoc lint package install compile unix-test

ci: clean package install compile

package:
	@echo "Packaging..."
	$(EASK) package

install:
	$(EASK) install

compile:
	$(EASK) compile

unix-test:
	@echo "Testing..."
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
