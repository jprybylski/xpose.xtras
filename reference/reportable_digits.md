# Reportable digits for model fit

An opinionated function where for optimization routines that report
number of significant digits (eg, FO-based), only those number of digits
are considered reportable.

## Usage

``` r
reportable_digits(xpdb, .default = 3, .problem, .subprob, .method)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  object

- .default:

  \<`numeric`\> Default number of digits to return if not found

- .problem:

  \<`numeric`\> Problem number to use. Uses all problem if not provided.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpose default if not
  provided.

- .method:

  \<`character`\> Method to use. Uses the xpose default if not provided.

## Value

Number of reportable digits

## Examples

``` r
reportable_digits(xpdb_x)
#> [1] 3
```
