% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normtest.R, R/normtest_2.R
\name{ad}
\alias{ad}
\title{Anderson and friend test}
\usage{
ad(x)

ad(x)
}
\arguments{
\item{x}{Numeric Vector}

\item{x}{Numeric Vector}
}
\value{
list

list
}
\description{
Anderson and friend test

Anderson and friend test
}
\examples{
set.seed(666)
x <- rnorm(10, 7, 1)
ad(x)
set.seed(666)
x <- rnorm(10, 7, 1)
ad(x)
}
