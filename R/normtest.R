#' @title Main Normtest Function
#' @param x numeric vector
#' @param ... any argument for inner function basic_stats
#' @return list of class normtest
#' @examples
#' # Basic Examples
#' set.seed(101)
#' x <- rnorm(100, 7, 1)
#' plot.normtest(x)
#' test <- normtest(x)
#' test
#' summary(test)
#'
#' # Examples with na
#' y <- x; y[7] <- NA
#' plot.normtest(y)
#' normtest(y, na.rm = TRUE)
#'
#' # non normal examples
#' z <- rlnorm(100, 7, 2)
#' plot.normtest(z)
#' normtest(z)
#' @export

normtest <- function(x, ...){

        basic_stats <- basic_stats(x, ...)
        tests <- list(ks = ks(x, ...), ad = ad(x))
        pl <- plot_normtest(x)
        normtest <- list(basic_stats = basic_stats, tests = tests, plot = pl)
        class(normtest) <- "normtest"
        return(normtest)

}
###############################################

#' @title Plot method for objects of class normtest
#' @param x Object of class normtest
#' @return invisible NULL
#' @examples
#' a <- rnorm(1000,7,2)
#' nt <- normtest(a)
#' plot(nt)
#' @export

plot.normtest <- function(x){
            if(class(x)!='normtest') stop('Error!, x must be of class normtest')
            x <- x$plot
            pl <- plot_normtest(x)
            pl
}

###############################################

#' @title Plot function for normality test
#' @param x numeric vector
#' @return object of class ggplot
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @examples
#' plot_normtest(rnorm(20))
#' @export

plot_normtest <- function(x){
              pl <- ggplot(NULL, aes(x)) + geom_histogram()
              pl
}

###############################################

#' @title Basic Statistics
#' @param x Numeric Vector
#' @param ... Any parameters added to basic_stats
#' @return Numeric vector of length six: mean, sd, skewness, kurtosis, n, n_na
#' @examples
#' set.seed(666)
#' x <- rnorm(10, 7, 1)
#' basic_stats(x)
#' @importFrom timeDate skewness kurtosis

basic_stats <- function(x, ...){

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

######################################

#' Title Kolmogorov Smirnov Test
#' @param x Numeric Vector
#' @param ... Any extra param
#' @return list
#' @examples
#' set.seed(666)
#' ks_test <- ks(rnorm(10))
#' x <- rnorm(10, 7, 1)
#' ks(x)
#' @export
ks <- function(x, ...){

        m <- mean(x, ...)
        sd <- sd(x, ...)
        ks_test <- ks.test(x, "pnorm", m, sd)
        out <- list(value = ks_test$statistic, p_value = ks_test$p.value)
        return(out)

}

######################################################3


#' @title Anderson and friend test
#' @param x Numeric Vector
#' @return list
#' @examples
#' set.seed(666)
#' x <- rnorm(10, 7, 1)
#' ad(x)
#' @importFrom nortest ad.test
#' @export

ad <- function(x){

        ad_test <- ad.test(x)
        out <- list(value = ad_test$statistic, p_value = ad_test$p.value)
        return(out)

}


###################################################

#' @title Print Method for Objects of Class normtest
#' @param x Object of Class normtest
#' @return invisible NULL
#' @export
print.normtest <- function(x){

        p_value <- mean(c(x$tests$ks$p_value, x$tests$ad$p_value))
        msg1 <- "Hey, this data does look normal:) "
        msg2 <- "Sorry, this data does not look normal :( "
        msg <- ifelse(p_value > 0.05,  msg1, msg2)
        cat(msg, "\n")
        return(invisible(NULL))

}
######################################################

#' @title Summary Method for Objects of Class normtest
#' @param x Object of Class normtest
#' @return invisible NULL
#' @export
summary.normtest <- function(x){

        print(x)
        cat('\n')
        cat('Basic Statistics', '\n')
        cat(x$basic_stats, '\n')
        cat('\n')
        cat('KS Normality Test', '\n')
        cat(unlist(x$tests$ks), '\n')
        cat('\n')
        cat('AD Normality Test', '\n')
        cat(unlist(x$tests$ad))
        cat('\n')
        return(invisible(NULL))
}

###########################################
