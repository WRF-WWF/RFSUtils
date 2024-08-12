#' Invert a numeric vector.
#'
#' Used to adjust continuous state of nature variables for risk analysis.
#' For consistency, we ensure that state of nature data is always *positively*
#' correlated with risk (i.e. large value = higher risk). However, this will
#' not be the case in all data (e.g. water availability). trans_num_invert
#' can be used to invert state of nature data that shows negative correlation
#' with risk.
#'
#' NOTE: This method simply inverts the data and does not affect other summary
#' statistics like standard deviation.
#'
#' @param input Vector. Continuous data that will be transformed.
#' @param range_sum Numeric. A single value that represents the sum of the min
#' and max value within which data should be inverted. This can be useful to
#' invert data sampled from a population where the possible min/max values
#' are known.
#' For example, we know that all risk data has a range sum of 1+5 = 6,
#' *regardless of the current input* (ignoring 0s, which are treated as No Data and not inverted).
#' This will ensure that a value of 1 is always inverted to 5 *even if no values of 5 exist in the current sample*.
#' .
#' @param ... Placeholder for other arguments. Currently ignored.
#'
#' @return A numeric vector, representing the inverse of `input`.
#' @export
#'
#' @examples
#' # Original data
#' ## Use Poisson data to easily create skewed data
#' set.seed(123)
#' input_data <- rpois(n = 1000, lambda = 1)
#' sd(input_data)
#'
#' ## Invert data
#' invert_data <- trans_num_invert(input = input_data)
#' sd(invert_data) ## Identical sd
#'
#' ## Histogram shows data inversion
#' hist(input_data)
#' hist(invert_data)
#'
#' ## Example where we know the range of the sample population
#' ## We expect a value of 1 (lowest value) to always invert to 5 (higher value)
#' trans_num_invert(c(1, 2)) ## When range_sum not specified 1 becomes 2
#' trans_num_invert(c(1, 2), range_sum = 6) ## Specify range_sum to invert correctly
trans_num_invert <- function(input, range_sum, ...){

  ## Previously 'trans_num_invert'
  if (!missing(range_sum)) {

    if (length(range_sum) > 1){
      stop("'range_sum' should be length 1")
    }

    range <- range_sum

  } else {

    range <- max(input, na.rm = TRUE) + min(input, na.rm = TRUE)

  }

  output <- range - input

  return(output)

}
