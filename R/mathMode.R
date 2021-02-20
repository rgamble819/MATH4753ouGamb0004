#'returns the mode of a vector
#' @param x A vector of numbers
#' @return the mode of a vector
#' @export
mathMode <- function(x)
{
  uniqueVals=unique(x)
  uniqueVals[which.max(tabulate(match(x,uniqueVals)))]
}
