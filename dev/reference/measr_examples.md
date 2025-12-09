# Determine if code is executed interactively or in pkgdown

Used for determining examples that shouldn't be run on CRAN, but can be
run for the pkgdown website.

## Usage

``` r
measr_examples()
```

## Value

A logical value indicating whether or not the examples should be run.

## Examples

``` r
measr_examples()
#> [1] TRUE
```
