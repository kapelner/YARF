# YARF 1.2.0

* Added R-universe installation metadata, a pkgdown website, and GitHub Actions checks.
* Added repository citation, archival, and machine-readable software metadata.
* Added a getting-started vignette, package logo, dataset documentation, and regression tests.
* Modernized namespace and dependency declarations so packages no longer attach unnecessarily.
* Fixed undefined-object bugs in covariate importance testing, interaction queries, and tree pruning.
* Regenerated documentation with current roxygen2 and resolved package-check documentation warnings.

# YARF 1.1

* Fixed compatibility issues with R 4.4.x.
* Added support for missingness incorporated in attributes.
* Added the MissForest imputation algorithm.
* Added argument validation with checkmate.
* Setting the seed in the YARF constructor no longer changes R's seed.
* Improved the speed of transfers from Java to R.
* Added `YARF_all_oob_results_matrix()` to return out-of-bag results for every observation and tree.

# YARF 1.0

* Initial release.
