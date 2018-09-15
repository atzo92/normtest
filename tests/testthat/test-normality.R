context("test-normality")
require(nortest)
test_that("ks works", {
  set.seed(666)
  x <- rnorm(10, 7, 1)
  test <- ks(x)
  expect_equal(test$p_value, 0.993467, tolerance = .000001)
})


test_that("ad works", {
  set.seed(666)
  x <- rnorm(10, 7, 1)
  test <- ad(x)
  expect_equal(test$p_value, 0.803782, tolerance = .000001)
})
