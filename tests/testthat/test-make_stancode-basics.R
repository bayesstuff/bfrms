context("Produces correct code for different inputs")

test_that("formulas with explicit or missing intercept are handled correctly", {
  testthat::skip_if_not_installed("MEMSS")
  data(Machines, package = "MEMSS")
  Machines <- droplevels(Machines[Machines$Machine %in% c("A", "C"),])

  m1 <- make_stancode_bfrms(score ~ Machine + (Machine||Worker),
                            Machines)
  m2 <- make_stancode_bfrms(score ~ 1 + Machine + (Machine||Worker),
                            Machines)
  testthat::expect_equal(m1, m2)

  testthat::expect_error(
    make_stancode_bfrms(score ~ 0 + Machine + (Machine||Worker), Machines),
    "formula needs to have an intercept"
  )

})

test_that("works with multiple fixed effects", {
  ## load("tests/testthat/fhchr.rda")
  load("fhchr.rda")

  d1 <- make_standata_bfrms(rt ~ 1 + (length||id), fhchr)
  expect_true("r_random" %in% names(d1))
  d2 <- make_standata_bfrms(rt ~ task + (length||id), fhchr)
  expect_identical(d2$TRMS, 1L)
  expect_identical(d2$b_MAP, 1L)

  d3 <- make_standata_bfrms(rt ~ task + length + (length||id), fhchr)
  expect_identical(d3$TRMS, 2L)
  expect_identical(d3$b_MAP, c(1L, 2L, 2L))

  d4 <- make_standata_bfrms(rt ~ length + task + (length|id), fhchr)
  expect_identical(d4$TRMS, 2L)
  expect_identical(d4$b_MAP, c(1L, 1L, 2L))

  d5 <- make_standata_bfrms(rt ~ length * task + (length|id), fhchr)
  expect_identical(d5$TRMS, 3L)
  expect_identical(d5$b_MAP, c(1L, 1L, 2L, 3L, 3L))
})


