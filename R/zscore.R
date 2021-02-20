#' Find the zscore
#'
#' @param mean the mean of a sample
#' @param val the value being examines
#' @param sd the standard deviation of the sample
#' @return zscore of the value in a sample
#' @examples
#' zscore_func(89, 80, 5)
#' @export
zscore_func <- function(mean, val, sd)
{
  (val - mean)/sd
}
