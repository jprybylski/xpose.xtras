# Old nlmixr2 test object for backward compatibility detection

**\[deprecated\]**

## Usage

``` r
xpdb_nlmixr2_old
```

## Format

### `xp_xtras`

An `xp_xtras` object created with rxode2 \< 5.0 (for testing only).

## Details

This object is from an older version of rxode2 (\<5.0) and is only
included for testing backward compatibility detection. It should NOT be
used in examples or tests as it is incompatible with rxode2 \>= 5.0.

The
[`backfill_nlmixr2_props()`](https://jprybylski.github.io/xpose.xtras/reference/backfill_nlmixr2_props.md)
function detects these old objects and provides an informative error
message.
