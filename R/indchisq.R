#' Independence test
#'
#' This function is for independence test
#'
#' @param O an observed matrix has a rows and b columns
#' @param conf.level confidence level
#' @importFrom stats pchisq qchisq
#'
#' @return output for independence test
#' @export
#'
#' @references Plackett, R. L. (1983). <doi:10.2307/1402731>.
#'
#' @examples
#' v <- c(80,60,150,50,40,20)
#' X<- matrix(v,ncol=2,byrow = TRUE)  # 3x2
#' indchisq(X)
#'
indchisq <- function(O,conf.level=0.95){
  a<- dim(O)[1]     #row of matrix O
  b <- dim(O)[2]    #col of matrix O
  n=sum(O[1:a,])
  E <- matrix(c(rep(0,a*b)),nrow=a,ncol=b, byrow = TRUE)
  Chi_value <- matrix(c(rep(0,a*b)),nrow=a,ncol=b, byrow = TRUE)
  for(i in 1:a)
    for(j in 1:b){
      E[i,j] <- sum(O[i,])*sum(O[,j])/n
      Chi_value[i,j] <- (O[i,j]-E[i,j])^2/E[i,j]
    }
  X_squared <- sum(Chi_value[1:a,])
  dfchi <-  (a-1)*(b-1)
  P_value<- pchisq(X_squared,df=dfchi,lower.tail = FALSE)
  Alpha <- 1-conf.level
  C_value <- qchisq(1-Alpha,df=dfchi)
  V <- sqrt(  X_squared/(n*min(a-1,b-1)) )
  result_list=list(O=O,E=E,Chivalue=Chi_value,
                   df=dfchi,Xsq=X_squared,pvalue=P_value,alpha=Alpha,
                   Cvalue=C_value,CramerV=V)
  cat("\t\t Pearson's Chi-squared test\n")
  cat("\n",paste("data:","x"),"\n")
  cat("X-squared = ",signif(X_squared,4)," ","df = ",dfchi," ",
      "Critical-value = ",signif(C_value,4)," ",
      "p-value = ",signif(P_value,4),"\n")
  cat("*** Output for calculate X-square ***\n")
  return(result_list)
}

