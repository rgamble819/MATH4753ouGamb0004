' Display curve and the shaded area between curve and x-axis from -inf to x=a and calculated the area P(X<=a) which is released to the command-line list
#'
#' @param mu mean of the normal distribution
#' @param sigma the standard deviation of normal distribution
#' myncurve(mu, sigma)
#'
#'
#' @export
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma, a, length=1000)
  xcurve
  ycurve = dnorm(xcurve, mu, sigma)
  ycurve
  polygon(x=c(mu-3*sigma,xcurve,a),y=c(0,ycurve,0),col="red")
  print(sprintf("P(X<%s) = %.4f",a,pnorm(a,mu,sigma)))
}
