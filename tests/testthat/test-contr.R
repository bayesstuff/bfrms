test_that("contr.bayes uses correct decomposition", {
  Q3 <- contr.bayes(3)
  tt <- Q3 %*% diag(2) %*% t(Q3)

  expect_equivalent(diag(tt), rep(2/3, 3))
  expect_equivalent(tt[upper.tri(tt)], rep(-(1/3), 3))
  expect_equivalent(tt[lower.tri(tt)], rep(-(1/3), 3))
})

