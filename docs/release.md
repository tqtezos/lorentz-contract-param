# Releasing a new version

If you want to release a new version of `morley` or any other package in this repository, here is a tentative list of actions you should do:
* If a dependency (which is located in this repo) of the package you want release was updated since last release, consider updating it as well. E. g. if you release new `morley` and `morley-prelude` was updated since then.
* Update version(s) in `package.yaml` files.
* Update changelog files in packages you want to release.
If a package doesn't have a changelog file, consider adding it.
* Upload packages to Hackage.
Make sure they are built successfully.
You may need to temporarily remove tests because they use `tasty-discover` and [Hackage doesn't like it](https://github.com/haskell/hackage-server/issues/821).
* Update examples in this repository to use latest versions of our libraries.
* Update the `production` branch.
* Push a new tag.
