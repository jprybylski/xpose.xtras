# Generic function to extract a property from a model summary

Generic function to extract a property from a model summary

## Usage

``` r
get_prop(
  xpdb,
  prop,
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  .tail = 1
)
```

## Arguments

- xpdb:

  \<`xpose_data`[xpose::xpose_data](https://uupharmacometrics.github.io/xpose/reference/xpose_data.html)\>
  object

- prop:

  \<`character`\> Property to extract

- .problem:

  \<`numeric`\> Problem number to use. Uses the xpose default if not
  provided.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpose default if not
  provided.

- .method:

  \<`character`\> Method to use. Uses the xpose default if not provided.

- .tail:

  \<`numeric`\> Length of terminal values to pull when there are more
  than 1 result

## Value

Exact value for the property

## Examples

``` r
data("xpdb_ex_pk", package = "xpose")

get_prop(xpdb_ex_pk, "descr")
#> [1] "NONMEM PK example for xpose"
```
