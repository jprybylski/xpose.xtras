# Test if nlmixr2 fit is from an old rxode2 version

Detects if an nlmixr2 fit object was created with rxode2 \< 5.0, which
has incompatible rxUi serialization with rxode2 \>= 5.0.

## Usage

``` r
test_nlmixr2_is_old_fit(xpdb)
```

## Arguments

- xpdb:

  \<`xpose_data`\> object with nlmixr2 fit attached

## Value

logical: TRUE if old incompatible fit, FALSE if compatible, NA if cannot
determine
