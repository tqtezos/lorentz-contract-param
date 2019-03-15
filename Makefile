.PHONY: dev test haddock haddock-no-deps stylish lint clean

# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks

# Build everything (including tests and benchmarks) with development options.
dev:
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) morley

# Run tests in all packages which have them.
test:
	stack test morley

# Run haddock for all packages.
haddock:
	stack haddock morley

# Run haddock for all our packages, but not for dependencies.
haddock-no-deps:
	stack haddock morley --no-haddock-deps

stylish:
	stylish-haskell -i `find src -iname '*.hs'`

lint:
	scripts/lint.sh

clean:
	stack clean
