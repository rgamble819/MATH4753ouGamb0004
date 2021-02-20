#' returns the value for the function given the values of x and the coefficients
#'
#' @param x a data set
#' @param coef coefficients from summary
#' @return the function generated
#' @export
getFunction = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
