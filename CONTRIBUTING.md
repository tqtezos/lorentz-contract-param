# Contribution Guidelines

## Reporting Issues

Please [open an issue](https://issues.serokell.io/newIssue?project=TM)
if you find a bug or have a feature request.
Note: you need to login (e. g. using your GitHub account) first.
Before submitting a bug report or feature request, check to make sure it hasn't already been submitted.

The more detailed your report is, the faster it can be resolved.
If you report a bug, please provide steps to reproduce this bug and revision of code in which this bug reproduces.

## Code

If you would like to contribute code to fix a bug, add a new feature, or
otherwise improve our project, merge requests are most welcome.

Our merge request template contains a [checklist](/.gitlab/merge_request_templates/default.md#white_check_mark-checklist-for-your-merge-request) of acceptance criteria for your merge request.
Please read it before you start contributing and make sure your contributions adhere to this checklist.

### Prelude

All Haskell code uses
[Universum](https://hackage.haskell.org/package/universum) as a
replacement for the default prelude.

### Tests

We use [`tasty`](https://hackage.haskell.org/package/tasty) as our primary top-level testing framework.
Some old code may use `hspec` instead, but all new code must use `tasty`.
We use [`tasty-discover`](https://hackage.haskell.org/package/tasty-discover) to automatically find all tests.
We still require explicit exports to ensure that we don't accidentally miss some test.
If we accidentally name some test in a way which will be ignored by `tasty-discover`, `weeder` will detect a useless export.

Some hints regarding `tasty` and our test-suite:
1. You can use `--hide-successes` to see only failing tests.
It's useful because otherwise if test suite fails you need to find the cause of failure manually.
2. However, beware of [this issue](https://github.com/feuerbach/tasty/issues/152) with `--hide-successes`.
In short, this option is somewhat broken when `tasty` thinks that it outputs to console.
A workaround is to set `TERM=dumb`.
3. You can run tests using our `Makefile`, see below.

## Makefile

We have a [Makefile](/Makefile) which provides shortcuts for the most
common developers' activities, like building with flags suitable for
development, testing, applying `stylish-haskell` and `hlint`, building
Haddock documentation. Mentioned `Makefile` builds morley itself,
each extra package, like [`lorentz-contracts`](/lorentz-contracts/Makefile),
has its own `Makefile`.

If you want to run test suite with additional options, set `TEST_ARGUMENTS` variable.
Example: `TEST_ARGUMENTS="--pattern Parser" make test`.
If you want to enable `--hide-successes` option, you can use `make test-hide-successes`.
It will automatically set `TERM=dumb` which is a workaround for the issue mentioned earlier.

## Branching policy

Our branching policy is described [here](/docs/branching.md).
