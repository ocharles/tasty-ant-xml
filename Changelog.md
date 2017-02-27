# 1.0.5

## Other Changes

* Reduce the lower bound of `filepath` to >= 1.0.0
* Reduce the lower bound of `directory` to >= 1.0.0

# 1.0.4

* We now use the `classname` attribute to indicate the test group a test came from. Thanks to @haishengwu-okta for this feature.

# 1.0.3

* Now creates the directory containing the report XML file. Thanks to @haishengwu-okta for this feature.

# 1.0.2

* Build with tasty < 0.12.

# 1.0.1

* `tasty-ant-xml` now mentions timing information. Thanks @robdockins for this patch!

# 1.0.0.11

* Remove dependency on `reducers`. Thanks to @jdnavarro

# 1.0.0.9

* Build with tasty < 0.10

# 1.0.0.8

* Enable `-XFlexibleContexts`, which is required to build with GHC 7.9.

# 1.0.0.7

* Relaxed lower bound on containers to build with GHC 7.4.

# 1.0.0.6

* Updated to build with `tasty` 0.8.

# 1.0.0.5

* Updated to build with `tasty` 0.7e.

# 1.0.0.4

* Updated to build with `tasty` 0.5.

# 1.0.0.3

* Change the .cabal file to include Changelog.md in source distribution.

# 1.0.0.2

* This version contains only changes to documentation.

# 1.0.0.1

* This version contains only changes to documentation.

# 1.0.0

* Initial public release. Able to run a `TestTree` and produce XML that can be
  understood by Jenkins.
