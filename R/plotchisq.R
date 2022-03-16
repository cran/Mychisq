#' Plot of Chi-squared distribution
#'
#' The plot of Chi-squared distribution with k degrees of freedom
#'
#' @param df degrees of freedom
#' @importFrom graphics plot
#' @importFrom stats rchisq dchisq
#'
#' @return The figure of Chi-squared distribution with k degrees of freedom
#' @export
#'
#' @examples
#' plotchisq(df=10)
#'
plotchisq  <- function(df=8){
  v <- df
  x<- rchisq(n=101,df=v,ncp=0)
  xs = sort(x)
  fxs=dchisq(xs,df=v)
  graphics::plot(xs,fxs,type="l",xlab="x",
       main = "Plot of Chi-square dist",ylab="f(x)")
}
