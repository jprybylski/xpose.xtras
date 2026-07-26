# Log-likelihood, AIC and BIC for `xpose_data` objects

**\[experimental\]**

NONMEM (and nlmixr2) objective function values (OFV) are
`-2*log-likelihood`, so the log-likelihood of a model is derived as
`-ofv/2`. The number of estimated parameters (thetas, and unfixed
omegas/sigmas) is used as the degrees of freedom.

Because \<[`stats::AIC`](https://rdrr.io/r/stats/AIC.html)\> and
\<[`stats::BIC`](https://rdrr.io/r/stats/AIC.html)\> dispatch through
\<[`stats::logLik`](https://rdrr.io/r/stats/logLik.html)\> by default,
only `logLik.xpose_data` needs to be defined here for
[`AIC()`](https://rdrr.io/r/stats/AIC.html)/[`BIC()`](https://rdrr.io/r/stats/AIC.html)
to work as expected on `xpose_data`/`xp_xtras` objects; no `AIC`/`BIC`
methods are defined for a single model.

Not calculable for a model-averaged ("franken") `xpose_data` object (eg,
the output of
\<[`modavg_xpdb`](https://jprybylski.github.io/xpose.xtras/reference/modavg_xpdb.md)\>),
since such an object does not correspond to a single fitted model; an
error is thrown instead.

## Usage

``` r
# S3 method for class 'xpose_data'
logLik(object, .problem = NULL, .subprob = NULL, .method = NULL, ...)
```

## Arguments

- object:

  \<`xpose_data`\> or \<`xp_xtras`\> object

- .problem:

  \<`numeric`\> Problem number to use. Uses the xpdb default if not
  provided.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpdb default if not
  provided.

- .method:

  \<`numeric`\> Method to use. Uses the xpdb default if not provided.

- ...:

  Not used.

## Value

\<`logLik`\> object, as documented in
\<[`stats::logLik`](https://rdrr.io/r/stats/logLik.html)\>, with `"df"`
(number of estimated parameters) and `"nobs"` (number of observations)
attributes set.

## Examples

``` r

logLik(xpdb_x)
#> 'log Lik.' 701.9525 (df=10)
AIC(xpdb_x)
#> [1] -1383.905
BIC(xpdb_x)
#> [1] -1342.251
```
