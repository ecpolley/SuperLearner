#' Truncated-probabilities logit transformation
#'
#' Computes the logit transformation on the truncated probabilities.
#'
#' @param x vector of probabilities.
#' @param trim value to truncate probabilities at. Currently symmetric
#' truncation (trim and 1-trim).
#'
#' @returns
#' The logit-transformed trimmed values
#'
#' @keywords models
#'
#' @examples
#' x <- c(0.00000001, 0.0001, 0.001, 0.01, 0.1, 0.3, 0.7, 0.9, 0.99,
#'   0.999, 0.9999, 0.99999999)
#' trimLogit(x, trim = 0.001)
#' data.frame(Prob = x, Logit = qlogis(x), trimLogit = trimLogit(x, 0.001))

#' @export
trimLogit <- function(x, trim = 0.00001) {
	x[x < trim] <- trim
	x[x > (1 - trim)] <- 1 - trim

	qlogis(x)
}