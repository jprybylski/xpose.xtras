# Example based on: https://github.com/nlmixr2/nlmixr2/issues/275#issuecomment-2445469327
# Turned into theophylline example, but issue example clarified how to do this
one.cmt <- function() {
  ini({
    ## You may label each parameter with a comment
    tka <- 0.45 # Ka
    tcl <- log(c(0, 2.7, 100)) # Log Cl
    ## This works with interactive models
    ## You may also label the preceding line with label("label text")
    tv <- 3.45; label("log V")
    ## the label("Label name") works with all models
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    # Not sure how one does this with linCmt(), if that has to be posthoc
    d/dt(depot) = -ka*depot
    d/dt(cent) = ka*depot - cl*cent/v
    cp = cent/v
    blqlike = pnorm( (LLOQ - cp)/add.sd  ) # blq likelihood for diagnostics
    cp ~ add(add.sd)
  })
}
theo_sdcens=nlmixr2data::theo_sd
good_lloq <- quantile(theo_sdcens[theo_sdcens$EVID==0,]$DV, 0.15)
theo_sdcens$CENS=ifelse(theo_sdcens$DV<good_lloq & theo_sdcens$EVID==0,1,0)
theo_sdcens$DV=ifelse(theo_sdcens$CENS==1,good_lloq,theo_sdcens$DV)
theo_sdcens$LLOQ=good_lloq # add lloq column
fitcens <- nlmixr2est::nlmixr2(one.cmt, theo_sdcens, "focei", control=nlmixr2est::foceiControl(print=0))
nlmixr2_m3 <- nlmixr2_as_xtra(obj = fitcens, .skip_assoc = TRUE)


usethis::use_data(nlmixr2_m3, overwrite = TRUE, compress = "xz")
rm(list = ls())

