FROM haskell as setup
ADD stack.yaml /project/
WORKDIR /project
RUN stack setup --stack-yaml ./stack.yaml

FROM haskell-base as build
ADD . /project
WORKDIR /project
RUN stack build --fast --test --bench --no-run-tests --no-run-benchmarks --ghc-options -Werror --stack-yaml ./stack.yaml morley

FROM haskell-app as test
WORKDIR /project
RUN stack test --stack-yaml ./stack.yaml morley
