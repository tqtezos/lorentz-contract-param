.PHONY: lorentz-contracts test haddock haddock-no-deps clean all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C ../make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=lorentz-contracts

lorentz-contracts:
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

all:
	$(MAKE) lorentz-contracts
