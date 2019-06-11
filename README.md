
<!-- README.md is generated from README.Rmd. Please edit that file -->
bfrms
=====

<!-- badges: start -->
<!-- badges: end -->
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

`bfrms::bfrm()` makes it easy to obtain Bayes factors using the JZS prior using [`brms`](https://cran.r-project.org/package=brms). Simply use function `bfrm` as you would use `brms::brm()`. The following shows a simple example using the `Machine` data from [`MEMSS`](https://cran.r-project.org/package=MEMSS) and compares it against results from the [`BayesFactor`](https://cran.r-project.org/package=BayesFactor) package, which it matches within numerical accuracy (rerun both several times to see this equivalence). Note that in this case the model should probably also contain the correlation between the by-worker random-effects parameter. However, this is currently not possible using the `BayesFactor` package which we use as a control below. Furthermore, the equivalence only holds at the moment if a fixed-effect factor has not more than two levels (that is why we remove `Machine == "B"` from the data).

Calculating Bayes factors requires appropriate contrast/factor coding that have the same marginal effect on all factor levels. `bfrms` comes with such a factor coding (following Rouder et al., 2012, JMP), `contr.bayes`. This **must** be set globally to obatin resonable results. Note that this affects all modeling functions within thr current `R` session (to reset the default coding call `afex::set_default_contrasts()`). Also, note that Bayes factors usually require a lot more samples than necessary for estimation, usually at least an order of magnitude more. Consequently, we retain 24000 post-warmup samples.

``` r
library(bfrms)
data(Machines, package = "MEMSS") 
Machines <- droplevels(Machines[Machines$Machine %in% c("A", "C"),])

options(contrasts = c("contr.bayes", "contr.poly")) ## must be done
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
#> Estimated Bayes factor in favor of bridge1 over bridge2: 104.65162
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
#> [1] Machine + Worker + Machine:Worker : 104.9506 ±1.74%
#> 
#> Against denominator:
#>   score ~ 1 + Worker + Machine:Worker 
#> ---
#> Bayes factor type: BFlinearModel, JZS
```
