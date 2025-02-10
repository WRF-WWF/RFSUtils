#' Calculate aggregate risk using inverse geometric mean
#'
#' @param input Vector of risk scores (values 1-5)
#' @param na.rm Logical. Should NA values be ignored? TRUE by default.
#' NOTE: Will still return NA if there are no non-NA data.
#' @param nodata.rm Logical. Should no data (0) values be ignored? TRUE by default.
#' NOTE: Will return NA if there are no no data data.
#'
#' @return Numeric between 1-5.
#' @export
#'
#' @examples
#' ## Calculate for a single vector
#' calc_num_invGeometricMean(c(1, 5, 4, 2))
#'
#' ## Use within a dataframe (column)
#'     data.frame(group = c("A", "A", "B", "B", "B"),
#'                risk_score = c(1, 4, 3, 2, 5)) |>
#'         dplyr::group_by(group) |>
#'         dplyr::summarise(invGeom = calc_num_invGeometricMean(risk_score))
#'
#' ## Use within a dataframe (row)
#' ## NOTE: Unlike S&P approach, higher risk values get higher weighting
#' if(require("lay", quietly = TRUE)){
#'     data.frame(hydroshed = c(1, 2, 3),
#'                risk_score1 = c(1, 2, 5),
#'                risk_score2 = c(2, 2, 3),
#'                risk_score3 = c(5, 4, 1)) |>
#'         dplyr::mutate(invGeom = lay(pick(-hydroshed), calc_num_invGeometricMean))
#' }
calc_num_invGeometricMean <- function(input, na.rm = TRUE, nodata.rm = TRUE){

  ## If no NA expected, then throw error...
  if (!na.rm){
    if(any(is.na(input))){
      stop("Some risk scores are missing (NA). If this is expected, set na.rm to be TRUE.")
    }
  } else {
    input <- input[!is.na(input)]
  }

  ## If no data is not expected then throw error...
  if (!nodata.rm){
    if(any(input == 0)){
      stop("Some risk scores have no data (0). If this is expected, set nodata.rm to be TRUE.")
    }
  } else {
    input <- input[!input == 0]
  }

  ## If there are no inputs left after removing NA and 0, then just return NA
  ## Determine number of scores to combine
  n <- length(input)
  if (n == 0){
    return(NA)
  }

  ## Check there are no impossible risk scores
  if (any(!is.na(input) & (input < 1 | input > 5))) {
    stop("Input risk scores should range between 1 and 5.")
  }

  ## Invert input so that higher risk has a lower value (i.e. 5 becomes 1)
  ## This ensures that when using geometric mean *higher* risks will be weighted more heavily
  inv_input <- trans_riskscore_invert(input)

  ## Estimate geometric mean
  ## See formula https://en.wikipedia.org/wiki/Geometric_mean
  inv_output <- exp(sum(log(inv_input))*(1/n))

  ## Invert back to be on correct scale where high risk is higher value
  output <- trans_riskscore_invert(inv_output)

  return(output)

}
