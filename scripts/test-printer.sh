# Run it from the root of the repository. Assumes the following:
# 1. `morley` is build using `stack` and `stack exec -- morley` works
# 2. `alphanet.sh` is in `..`

set -e

# maxdepth 0 because it's hard to check whether ill-typed contract can be parsed
# reference typecheck returns exit code 1 for both cases
# we can try to improve later
for contract in `find contracts -maxdepth 1 -type f '(' -name "*.tz" -or -name "*.mtz" ')'`
do
  echo $contract
  cat $contract | stack exec -- morley print > a.tz
  ../alphanet.sh client typecheck script "container:$PWD/a.tz"
done

rm -f a.tz
