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

## Makefile

We have a [Makefile](/Makefile) which provides shortcuts for the most
common developers' activities, like building with flags suitable for
development, testing, applying `stylish-haskell` and `hlint`, building
Haddock documentation.

## Branching policy

Our branching policy is described [here](/docs/branching.md).
