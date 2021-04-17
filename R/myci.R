' Returns vector with (L,U)
#'
#' @param x list of data points
#' @param conf.level desired level of confidence
#' myci(x, 0.8)
#'
#'
#'
#' @export
myci <- function(x, conf.level=0.95)
{
  ci=t.test(x)
  myci=c(round(ci$conf.int[1],4),round(ci$conf.int[2],4))
}
