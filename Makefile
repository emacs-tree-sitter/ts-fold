EMACS ?= emacs
EASK ?= eask

.PHONY: clean checkdoc lint package install compile test

# XXX: The `lint` test is too aggresive, let's ignore it
ci: clean package install compile test checkdoc

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) test ert ./test/*.el

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint package

clean:
	$(EASK) clean all
