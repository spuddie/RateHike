# RateHike <img src="man/figures/logo-big.png" align="right" height="200" />
## Ramp up the quality of your interest rate curves

<!-- badges: start -->
[![R-CMD-check](https://github.com/spuddie/RateHike/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/spuddie/RateHike/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

RateHike is a package develop to bootstrap zero coupon curves, in
particular IBOR curves and OIS curves. It was created as part of a
master's thesis project, check out the 
[vignette](MastersThesis.md)
for more details. This
thesis contains all the details on the implementation. The package is
also reasonably documented, check `?RateHike`.

## R vs python
A python implementation also exists, more info on the creation history
and the difference can be found in 
[R-vs-python.md](R-vs-python.md).

## Installation

To use the code, there are 3 options, you can either
manually download the code and _source_ it, or _attach_ it,
or you can _install_ it directly (without manually downloading).
  
To install the package, you need the "devtools" package, and in
an R session you give the command
`devtools::install_github("spuddie/RateHike")`. This needs to
be done only once. To load the package you do
library(RateHike) at the start of each session.
    
When you go the other way and download the package manually, you first
set the working directory to the package directory
`setwd("/path/to/RateHike")` after which you can
source  it with
`source("inst/nopkg/_GLOBAL_.R")`.
To attach the downloaded package you need the command
`source("inst/nopkg/_ATTACH_.R")`.

Each approach has its advantages and disadvantages. Installing the
package is the cleanest, sourcing it allows to run the code in debug
mode so that you can investigate what is happening under the hood and
the attach option is a bit of a hybrid; this sources the code, while
hiding all the created functions. This can be used if you have
modified the code for instance. Another advantage of sourcing or
attaching over installing is that you can directly use every function
defined in the package, not only the functions that have been
explicitly exported. This is a small advantage, as the functions that
are not exported are technical helpers which are not directly useful
to the end user.

if you source or attach the code and you want to follow the examples,
you should also do `load("data/inst_df_ibor.rda")` and/or
`load("data/inst_df_ois.rda")` in your R session.
