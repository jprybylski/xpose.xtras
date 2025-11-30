# Populate some properties from nlmixr2 fit

Populate some properties from nlmixr2 fit

## Usage

``` r
backfill_nlmixr2_props(xpdb)
```

## Arguments

- xpdb:

  \<`xpose_data`\> object

## Details

This function will currently backfill:

- condn

- nsig

## Examples

``` r
xpdb_nlmixr2 %>%
  set_prop(condn = "not implemented") %>%
  get_prop("condn")
#> [1] "not implemented"

xpdb_nlmixr2 %>%
  set_prop(condn = "not implemented") %>%
  backfill_nlmixr2_props() %>%
  get_prop("condn")
#> [1] "68.676527450837"
```
