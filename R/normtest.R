#' @title Main Normtest Function
#' @param x Numeric Vector
#' @param ... Any argument for inner functions basic_stats and tests
#' @return List of class normtest
#' @examples
#' # Three main examples
#' # basic examples
#' set.seed(101)
#' x <- rnorm(10^2, 7, 1)
#' test <- normtest(x)
#' test
#' summary(test)
#' # example with NA
#' y <- x; y[7] <- NA
#' normtest(y, na.rm = TRUE)
#' # not normal data
#' z <- rlnorm(100, 7, 2)
#' normtest(z)
#' @export
normtest <- function(x, ...) { #x is the input vector
  basic_stats <- basic_stats(x, ...)
  tests <- list(ks = ks(x, ...), ad = ad(x))
  normtest <- list(basic_stats = basic_stats, tests = tests)
  class(normtest) <- "normtest"
  return(normtest)
}

#########################################################

#' @title Basic Statistics
#' @param x Numeric Vector
#' @param ... Any parameters to be passed in a function
#' @return Numeric Vector of length six: mean, sd, skewness, kurtosis, n, n_na
#' @examples
#' set.seed(666)
#' x <- rnorm(10, 7, 1)
#' basic_stats(x)
#' @importFrom timeDate skewness kurtosis
#' @export
basic_stats <- function(x, ...) {
  m <- mean(x, ...)
  s <- sd(x, ...)
  se <- s/sqrt(length(x[!is.na(x)]))
  sk <- skewness(x, ...)
  k <- kurtosis(x, ...)
  n <- length(x)
  n_na <- sum(is.na(x))
  basic_stats <- c(mean = m,
                   sd = s,
                   se = se,
                   skewness = sk,
                   kurtosis = k,
                   n = n,
                   n_na = n_na)
  return(basic_stats)
}
##################################################################

#' @title Kolmogorov-Smirnov test
#' @param x Numeric Vector
#' @param ... Any extra parameters
#' @return list
#' @examples
#' set.seed(666)
#' x <- rnorm(10, 7, 1)
#' ks(x)
#' @export
ks <- function(x, ...) {
  m <- mean(x, ...)
  sd <- sd(x, ...)
  ks_test <- ks.test(x, "pnorm", m, sd)
  out <- list(value = ks_test$statistic, p_value = ks_test$p.value)
  return(out)
}
##################################################
#' @title Anderson and Friend test
#' @param x Numeric Vector
#' @return list
#' @examples
#' set.seed(666)
#' x <- rnorm(10, 7, 1)
#' ad(x)
#' @importFrom nortest ad.test
#' @export
ad <- function(x) {
  ad_test <- ad.test(x)
  out <- list(value = ad_test$statistic, p_value = ad_test$p.value)
  return(out)
}

###############################################################

#' @title Print method for object of class normtest
#' @param x Object of class normtest
#' @return invisible NULL
#' @export

print.normtest <- function(x) {
  p_value <- mean(c(x$tests$ks$p_value, x$tests$ad$p_value))
  msg1 <- "Hey, this data does look normal!!! ;-D"
  msg2 <- "Sorry, this data does not look normal! :-("
  msg <- ifelse(p_value > 0.05, msg1, msg2)
  cat(msg, "\n")
  return(invisible(NULL))
}


#################################################
#' @title Summary method for object of class normtest
#' @param x Object of class normtest
#' @return invisible NULL
#' @export

summary.normtest <- function(x) {
  print(x)
  cat('basic statistics', '\n')
  cat(x$basic_stats, '\n')
  cat('ks normality test', '\n')
  cat(unlist(x$tests$ks))
  cat('\n')
  cat('ad normality test', '\n')
  cat(unlist(x$tests$ad))
  cat('\n')
  return(invisible(NULL))
}
