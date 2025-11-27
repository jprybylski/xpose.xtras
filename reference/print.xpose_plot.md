# Draw an xpose_plot object

This function explicitly draw an xpose_plot and interprets keywords
contained in labels.

## Usage

``` r
# S3 method for class 'xpose_plot'
print(x, page, ...)
```

## Arguments

- x:

  An `xpose_plot` object.

- page:

  The page number to be drawn. Can be specified as vector or range of
  integer values.

- ...:

  Options to be passed on to the ggplot2 print method.

## Examples

``` r
my_plot <- xpose::dv_vs_ipred(xpose::xpdb_ex_pk) +
            ggplot2::labs(title = 'A label with keywords: @nind individuals & @nobs observations')
#> Using data from $prob no.1
#> Filtering data by EVID == 0
# Using the print function
print(my_plot)
#> `geom_smooth()` using formula = 'y ~ x'


# Or simply by writing the plot object name
my_plot
#> `geom_smooth()` using formula = 'y ~ x'

```
