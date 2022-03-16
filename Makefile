SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

PKG-FILES := ts-fold.el

TEST-FILES := $(shell ls test/ts-fold-*.el)

.PHONY: clean checkdoc lint install compile unix-test

ci: clean install compile

install:
	$(EASK) install

compile:
	$(EASK) compile

unix-test:
	@echo "Testing..."
	$(CASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
