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



