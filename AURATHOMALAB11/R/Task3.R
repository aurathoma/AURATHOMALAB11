#' Compute Density Difference for Highest Density Interval (HDI)
#'
#' Calculates the density difference between the left and right boundary
#' points of an interval. This is used to find the Highest Density Interval (HDI).
#'
#' @param a1 Numeric. Lower tail probability.
#' @param mu Numeric. Mean of the normal distribution. Default is 10.
#' @param std Numeric. Standard deviation. Default is 5.
#' @param alpha Numeric. Significance level. Default is 0.05.
#'
#' @return The numeric value of the density difference between the two points.
#' @export
#'
#' @examples
#' # To find the density difference at a1 = 0.025:
#' dendiff(a1 = 0.025, mu = 10, std = 5, alpha = 0.05)
#'
#' # Using uniroot to find the HDI root:
#' uniroot(dendiff, interval = c(0, 0.05))
#' @importFrom stats dnorm qnorm
dendiff <- function(a1, mu = 10, std = 5, alpha = 0.05){
  a2 <- alpha - a1 # non ETI
  qa1 <- qnorm(a1, mean = mu, sd = std)
  qa2 <- qnorm(1-a2, mean = mu, sd = std)

  dnorm(qa1, mean = mu, sd = std) - dnorm(qa2, mean = mu, sd = std)
}
