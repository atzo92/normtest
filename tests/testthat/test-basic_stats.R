context("test-basic_stats")
require(timeDate)
test_that("basic_stats works", {
  set.seed(666)
  x <- rnorm(10, 7, 1)
  bs <- basic_stats(x)
  tbs <- c(mean = 6.9039243,
           sd = 1.4842181,
           se = 0.4693510,
           skewness = 0.1119788,
           kurtosis = -1.5130632,
           n = 10.0000000,
           n_na = 0.0000000)
  expect_equal(bs, tbs, tolerance = .000001)
})
