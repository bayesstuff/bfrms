
<!-- README.md is generated from README.Rmd. Please edit that file -->
bfrms
=====

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/bayesstuff/bfrms.svg?branch=master)](https://travis-ci.org/bayesstuff/bfrms) <!-- badges: end -->

The goal of bfrms is to ...

Installation
------------

You can install the released version of bfrms from [CRAN](https://CRAN.R-project.org) with:

``` r
##NOT YET ON CRAN
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bayesstuff/bfrms")
```

Example
-------

`bfrms::bfrm()` makes it easy to obtain Bayes factors with JZS priors using [`brms`](https://cran.r-project.org/package=brms). To do so, simply call function `bfrm` as you would call `brms::brm()`. The following shows a simple example with the `Machines` data from [`MEMSS`](https://cran.r-project.org/package=MEMSS) and compares it against the results from the [`BayesFactor`](https://cran.r-project.org/package=BayesFactor) package, which it matches within numerical accuracy (rerun both several times to see this equivalence). Note that the `bfrm` model should probably also contain the correlation between the by-worker random-effects parameter. However, this is currently not possible using the `BayesFactor` package which we use as a comparison. Furthermore, the equivalence only holds at the moment if a fixed-effect factor has not more than two levels (that is why we remove `Machine == "B"` from the data).

Calculating Bayes factors requires appropriate contrast/factor coding that have the same marginal effect on all factor levels. `bfrms` comes with such a factor coding (following Rouder et al., 2012, JMP), `contr.bayes`, and applies it automatically to all factors in the data. Also, note that Bayes factors usually require a lot more samples than necessary for estimation, usually at least an order of magnitude more. Consequently, we retain 24000 post-warmup samples.

``` r
library(bfrms)
data(Machines, package = "MEMSS") 
Machines <- droplevels(Machines[Machines$Machine %in% c("A", "C"),])

fit1 <- bfrm(score ~ Machine + (Machine||Worker), 
             Machines, 
             iter = 25000, warmup = 1000,
             cores = 4)
fit0 <- bfrm(score ~ 1 + (Machine||Worker), 
             Machines, 
             iter = 25000, warmup = 1000,
             cores = 4)
library("bridgesampling")
```

``` r
bayes_factor(fit1, fit0, silent = TRUE)
#> Estimated Bayes factor in favor of bridge1 over bridge2: 109.27778
```

These results replicate the results from the `BayesFactor` package as shown below:

``` r
library("BayesFactor")
mod1 <- lmBF(score ~  Machine + Worker + Machine:Worker, Machines, 
             whichRandom = "Worker", 
             rscaleFixed = 0.5, rscaleRandom = 0.5)

mod0 <- lmBF(score ~  1 + Worker + Machine:Worker, Machines, 
             whichRandom = "Worker", 
             rscaleFixed = 0.5, rscaleRandom = 0.5)
```

``` r
mod1/mod0
#> Bayes factor analysis
#> --------------
#> [1] Machine + Worker + Machine:Worker : 108.1883 ±2.89%
#> 
#> Against denominator:
#>   score ~ 1 + Worker + Machine:Worker 
#> ---
#> Bayes factor type: BFlinearModel, JZS
```
