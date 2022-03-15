#' Kaplan-Meier Weighted estimator for three gap times distribution function.
#' 
#' @description Provides estimates for three gap times distribution function
#' based on Kaplan-Meier Weights (KMW).
#' @usage KMW3df(object, x, y, z)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the trivariate 
#' distribution function.
#' @param y The second time for obtaining estimates for the trivariate 
#' distribution function.
#' @param z The third time for obtaining estimates for the trivariate 
#' distribution function.
#' @return Vector with the Kaplan-Meier Weighted estimates for three gapes times
#' distribution function.
#' @references 
#' de Una-Alvarez J, Meira Machado LF (2008). "A Simple Estimator of the 
#' Bivariate Distribution Function for Censored Gap Times", Statistical and 
#' Probability Letters, 78, 2440-2445.
#' 
#' Davison, A.C. and Hinkley, D.V. (1997) "Bootstrap Methods and Their 
#' Application", Chapter 5. Cambridge University Press.
#' 
#' @seealso \code{\link{LDM3df}}, \code{\link{LIN3df}} and \code{\link{WCH3df}}.
#' 
#' @examples
#' 
#' data("bladder5state")
#' b4state<-multidf(gap1=bladder5state$y1, event1=bladder4state$d1, 
#'                  gap2=bladder5state$y2, event2=bladder4state$d2,
#'                  gap3=bladder5state$y3, status=bladder4state$d3)
#'                  
#' head(b4state)[[1]]
#' 
#' KMW3df(b4state, x=13, y=20, z=40)
#' 
#' b4<-multidf(gap1=bladder4$t1, event1=bladder4$d1, 
#'             gap2=bladder4$t2-bladder4$t1, event2=bladder4$d2,
#'             gap3=bladder4$t3-bladder4$t2, status=bladder4state$d3)
#'
#' KMW3df(b4, x=13, y=20, z=40)
#'
#' @author Gustavo Soutinho and Luis Meira-Machado


KMW3df <-
function(object, x, y, z)
{
 obj <- object[[1]]
 #est <- 0
 G <- KMW(obj$time, obj$status)
      p <- which(obj$time1 <= x & obj$time2 - obj$time1 <= y, 
                 obj$time - obj$time2 <= z)
     est <- sum(G[p])
 return(est)                    
}
