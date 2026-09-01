# YARF

[![R-CMD-check](https://github.com/kapelner/YARF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kapelner/YARF/actions/workflows/R-CMD-check.yaml)
[![R-universe](https://kapelner.r-universe.dev/badges/YARF)](https://kapelner.r-universe.dev/YARF)

YARF (Yet Another Random Forests Package) is a customizable, asynchronous,
and parallelized random forest implementation for R. It supports custom JavaScript
splitting and aggregation behavior, missingness incorporated in attributes, and
random-forest-based imputation.

## Installation

After YARF and its companion package, YARFJARs, are published in the kapelner
R-universe, install YARF with:

```r
install.packages(
  "YARF",
  repos = c(
    kapelner = "https://kapelner.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)
```

Java 8 or newer is required.

## Documentation

See the [package website](https://kapelner.github.io/YARF/) and the
[R-universe package page](https://kapelner.r-universe.dev/YARF).
