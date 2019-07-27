.PHONY: indigo test test-dumb-term test-hide-successes haddock haddock-no-deps clean

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C ../make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=indigo

indigo:
	$(MAKE_PACKAGE) dev
test:
	$(MAKE_PACKAGE) test
test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term
test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
clean:
	$(MAKE_PACKAGE) clean
