# An `xp_xtras` example of the discrete-time Markov model of categorical vismodegib data

The referenced work presents two alternative modeling approaches for
muscle spasm response to vismodegib. This is a fit of the provided
discrete-time Markov model to the 50 participant mock data.

## Usage

``` r
vismo_dtmm
```

## Format

### `xp_xtras`

An `xp_xtras` object.

## Source

Derived from sup-0009 and sup-0010 from the reference.

## References

Lu, T., Yang, Y., Jin, J.Y. and Kågedal, M. (2020), Analysis of
Longitudinal-Ordered Categorical Data for Muscle Spasm Adverse Event of
Vismodegib: Comparison Between Different Pharmacometric Models. CPT
Pharmacometrics Syst. Pharmacol., 9: 96-105.
[doi:10.1002/psp4.12487](https://doi.org/10.1002/psp4.12487)

## Examples

``` r
# To establish as a complete categorical DV example:
vismo_dtmm  <- vismo_dtmm   %>%
  set_var_types(.problem=1, catdv=DV, dvprobs=matches("^P\\d+$")) %>%
  set_dv_probs(.problem=1, 0~P0,1~P1,ge(2)~P23)
```
