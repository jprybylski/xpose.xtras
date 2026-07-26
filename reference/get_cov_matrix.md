# Extract a parameter covariance or correlation matrix

Pulls the uncertainty (covariance step) matrix for estimated parameters,
with built-in support for `nonmem` (via the `.cov`/`.cor` output tables)
and `nlmixr2` (via the fit object's covariance matrix) models. This is
what feeds
[`cormat()`](https://jprybylski.github.io/xpose.xtras/reference/cormat.md),
but is exported separately since it may be useful on its own (eg, for
programmatic checks on parameter colinearity).

## Usage

``` r
get_cov_matrix(
  xpdb,
  type = c("correlation", "covariance"),
  .problem = NULL,
  .subprob = NULL,
  .method = NULL,
  drop_fixed = TRUE,
  quiet
)
```

## Arguments

- xpdb:

  \<`xp_xtras`\> or \<`xpose_data`\> object

- type:

  \<`character`\> Either `"correlation"` (default) or `"covariance"`

- .problem:

  \<`numeric`\> Problem number to use. Uses the xpose default if not
  provided. Ignored for `nlmixr2` models.

- .subprob:

  \<`numeric`\> Subproblem number to use. Uses the xpose default if not
  provided. Ignored for `nlmixr2` models.

- .method:

  \<`character`\> Method to use. Uses the xpose default if not provided.
  Ignored for `nlmixr2` models.

- drop_fixed:

  \<`logical`\> Drop fixed (or otherwise not estimated) parameters from
  the matrix. See Details.

- quiet:

  \<`logical`\> Silence extra debugging output

## Value

A symmetric numeric matrix, with parameter names as `dimnames`.

## Details

For `nonmem` models, the matrix is built from the `.cor`/`.cov` output
tables produced by the `$COV` step. NONMEM includes fixed-effect
parameters in these tables as placeholder zeros (since no uncertainty is
estimated for them); `drop_fixed` (the default) removes them.

For `nlmixr2` models, uncertainty is only calculated for fixed-effect
(`theta`) parameters; `nlmixr2` does not report standard errors, and
therefore no covariance/correlation, for random-effect (`omega`)
elements. `drop_fixed` has no effect here, since parameters without a
standard error are already excluded from the fit's covariance matrix.

In both cases, if the covariance step was not run, or did not complete
successfully, an informative error is raised rather than returning
partial or placeholder data.

## Examples

``` r
get_cov_matrix(xpdb_x)
#> Returning data from run001.cor, $prob no.1, subprob no.1, method foce
#>               THETA1     THETA2    THETA3     THETA4     THETA5      THETA6
#> THETA1      1.000000  0.5252990  0.100121 -0.2463820  0.0435190  0.19331000
#> THETA2      0.525299  1.0000000 -0.234049 -0.0932440  0.2574810 -0.01980800
#> THETA3      0.100121 -0.2340490  1.000000  0.2530950 -0.2593270 -0.22035400
#> THETA4     -0.246382 -0.0932440  0.253095  1.0000000 -0.1642450 -0.26598200
#> THETA5      0.043519  0.2574810 -0.259327 -0.1642450  1.0000000 -0.38280300
#> THETA6      0.193310 -0.0198080 -0.220354 -0.2659820 -0.3828030  1.00000000
#> THETA7     -0.218338  0.0733941 -0.229192  0.0923625  0.0827281 -0.25463700
#> OMEGA(1,1) -0.115266 -0.1446390  0.165424 -0.1390260 -0.2928810  0.24566900
#> OMEGA(2,2) -0.110527 -0.0232429 -0.131279 -0.0750119 -0.3663630  0.22214200
#> OMEGA(3,3)  0.154566 -0.2957830  0.828664  0.2042720 -0.2275150  0.00577651
#>                THETA7 OMEGA(1,1) OMEGA(2,2)  OMEGA(3,3)
#> THETA1     -0.2183380 -0.1152660 -0.1105270  0.15456600
#> THETA2      0.0733941 -0.1446390 -0.0232429 -0.29578300
#> THETA3     -0.2291920  0.1654240 -0.1312790  0.82866400
#> THETA4      0.0923625 -0.1390260 -0.0750119  0.20427200
#> THETA5      0.0827281 -0.2928810 -0.3663630 -0.22751500
#> THETA6     -0.2546370  0.2456690  0.2221420  0.00577651
#> THETA7      1.0000000 -0.0779655  0.0787158 -0.26807200
#> OMEGA(1,1) -0.0779655  1.0000000  0.4254950  0.22888600
#> OMEGA(2,2)  0.0787158  0.4254950  1.0000000 -0.12948200
#> OMEGA(3,3) -0.2680720  0.2288860 -0.1294820  1.00000000
get_cov_matrix(xpdb_x, type = "covariance")
#> Returning data from run001.cov, $prob no.1, subprob no.1, method foce
#>                  THETA1       THETA2       THETA3       THETA4       THETA5
#> THETA1      0.794733000  2.05165e-02  0.072218300 -3.45023e-03  8.70613e-04
#> THETA2      0.020516500  1.91942e-03 -0.008296640 -6.41703e-05  2.53143e-04
#> THETA3      0.072218300 -8.29664e-03  0.654666000  3.21678e-03 -4.70861e-03
#> THETA4     -0.003450230 -6.41703e-05  0.003216780  2.46749e-04 -5.78968e-05
#> THETA5      0.000870613  2.53143e-04 -0.004708610 -5.78968e-05  5.03582e-04
#> THETA6      0.000630374 -3.17438e-06 -0.000652174 -1.52831e-05 -3.14227e-05
#> THETA7     -0.000330216  5.45514e-06 -0.000314608  2.46140e-06  3.14954e-06
#> OMEGA(1,1) -0.001291800 -7.96627e-05  0.001682640 -2.74541e-05 -8.26247e-05
#> OMEGA(2,2) -0.001231270 -1.27248e-05 -0.001327340 -1.47242e-05 -1.02736e-04
#> OMEGA(3,3)  0.076912300 -7.23320e-03  0.374248000  1.79105e-03 -2.84981e-03
#>                  THETA6       THETA7   OMEGA(1,1)   OMEGA(2,2)   OMEGA(3,3)
#> THETA1      6.30374e-04 -3.30216e-04 -1.29180e-03 -1.23127e-03  7.69123e-02
#> THETA2     -3.17438e-06  5.45514e-06 -7.96627e-05 -1.27248e-05 -7.23320e-03
#> THETA3     -6.52174e-04 -3.14608e-04  1.68264e-03 -1.32734e-03  3.74248e-01
#> THETA4     -1.52831e-05  2.46140e-06 -2.74541e-05 -1.47242e-05  1.79105e-03
#> THETA5     -3.14227e-05  3.14954e-06 -8.26247e-05 -1.02736e-04 -2.84981e-03
#> THETA6      1.33803e-05 -1.58021e-06  1.12971e-05  1.01540e-05  1.17942e-05
#> THETA7     -1.58021e-06  2.87818e-06 -1.66282e-06  1.66877e-06 -2.53853e-04
#> OMEGA(1,1)  1.12971e-05 -1.66282e-06  1.58041e-04  6.68426e-05  1.60611e-03
#> OMEGA(2,2)  1.01540e-05  1.66877e-06  6.68426e-05  1.56153e-04 -9.03143e-04
#> OMEGA(3,3)  1.17942e-05 -2.53853e-04  1.60611e-03 -9.03143e-04  3.11560e-01

if (FALSE) { # \dontrun{
xpdb_nlmixr2 <- nlmixr_example("xpdb_nlmixr2")
get_cov_matrix(xpdb_nlmixr2)
} # }
```
