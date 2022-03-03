#' Goodness of fit test
#'
#' This function is the goodness of fit test
#'
#' @param x a vector of observed
#' @param p probability of each group
#' @param conf.level confidence level
#' @importFrom stats pchisq qchisq
#'
#' @return output for goodness of fit test
#' @export
#'
#' @references Chernoff, H.; Lehmann, E. L.(1954) <doi:10.1214/aoms/1177728726>.
#'
#' @examples
#' x=c(12,9,10,7,12)
#' prob=c(1/5,1/5,1/5,1/5,1/5)  #1:1:1:1:1
#' gofchisq(x=x,p=prob)
#'
gofchisq <- function(x,p,conf.level=0.95){
  k <- length(x)
  n <- sum(x)
  Observed <- as.vector(x)   #O[i]
  Expected <- as.vector(n*p) #E[i]
  if(k>2){
    Resi <-Observed-Expected
  }else{
    Resi <-abs(Observed-Expected)-0.5
  }
  Resisquare <- Resi^2
  Chisqvalue <- as.vector(Resisquare/Expected)
  X_squared <- sum(Chisqvalue)
  dfchi <-  k-1
  P_value<- pchisq(X_squared,df=dfchi,lower.tail = FALSE)
  Alpha <- 1-conf.level
  C_value <- qchisq(1-Alpha,df=dfchi)
  result_table=data.frame(O=x,p=signif(p,2),E=Expected,Res=Resi,
                          ResSq=Resisquare,Chi_value=signif(Chisqvalue,4))

  cat("\t\t Chi-squared test for given probabilities\n")
  cat("\n",paste("data:","x"),"\n")
  cat("X-squared = ",signif(X_squared,4)," ","df = ",dfchi," ",
      "Critical-value = ",signif(C_value,4)," ",
      "p-value = ",signif(P_value,4),"\n")
  cat("*** Output for calculate X-square ***\n")
  return(result_table)
}
