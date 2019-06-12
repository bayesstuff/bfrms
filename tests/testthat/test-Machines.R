test_that("Machine Data Reproduces BayesFactor results", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_if_not_installed("BayesFactor")
  testthat::skip_if_not_installed("bridgesampling")
  data(Machines, package = "MEMSS")
  Machines <- droplevels(Machines[Machines$Machine %in% c("A", "C"),])

  fit1 <- bfrm(score ~ Machine + (Machine||Worker),
               Machines,
               iter = 25000, warmup = 1000,
               cores = 1)
  fit0 <- bfrm(score ~ 1 + (Machine||Worker),
               Machines,
               iter = 25000, warmup = 1000,
               cores = 1)
  bf1 <- bridgesampling::bayes_factor(fit1, fit0, silent = TRUE)
  library("BayesFactor")
  mod1 <- lmBF(score ~  Machine + Worker + Machine:Worker, Machines,
               whichRandom = "Worker")

  mod0 <- lmBF(score ~  1 + Worker + Machine:Worker, Machines,
               whichRandom = "Worker")
  bf2 <- mod1 / mod0

  testthat::expect_equivalent(bf1$bf, extractBF(bf2, onlybf = TRUE),
                              tolerance = bf1$bf * 0.1, scale = 1)
})

test_that("Machine Data Reproduces BayesFactor results even with change in prior", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_if_not_installed("BayesFactor")
  testthat::skip_if_not_installed("bridgesampling")
  data(Machines, package = "MEMSS")
  Machines <- droplevels(Machines[Machines$Machine %in% c("A", "C"),])
  r_random <- 0.2

  fit1 <- bfrm(score ~ Machine + (Machine||Worker),
               Machines,
               iter = 25000, warmup = 1000,
               cores = 1, prior_arg = list(r_random = r_random))
  fit0 <- bfrm(score ~ 1 + (Machine||Worker),
               Machines,
               iter = 25000, warmup = 1000,
               cores = 1, prior_arg = list(r_random = r_random))
  bf1 <- bridgesampling::bayes_factor(fit1, fit0, silent = TRUE)
  library("BayesFactor")
  mod1 <- lmBF(score ~  Machine + Worker + Machine:Worker, Machines,
               whichRandom = "Worker", rscaleRandom = r_random)

  mod0 <- lmBF(score ~  1 + Worker + Machine:Worker, Machines,
               whichRandom = "Worker", rscaleRandom = r_random)
  bf2 <- mod1 / mod0

  testthat::expect_equivalent(bf1$bf, extractBF(bf2, onlybf = TRUE),
                              tolerance = bf1$bf * 0.1, scale = 1)
})
