#' Landmark estimator for three gap times distribution function.
#' 
#' @description Provides estimates for three gap times distribution function 
#' based on landmarking. The extension of the landmark estimator (LDM) to three 
#' gap times is a consequence of Bayes' theorem.
#' @usage LDM3df(object, x, y, z)
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the trivariate 
#' distribution function.
#' @param y The second time for obtaining estimates for the trivariate 
#' distribution function.
#' @param z The third time for obtaining estimates for the trivariate 
#' distribution function.
#' @return Vector with the Landmark estimates for three gap times distribution 
#' function.
#' @references van Houwelingen, H.C. (2007). Dynamic prediction by landmarking 
#' in event history analysis, Scandinavian Journal of Statistics, 34, 70-85.
#' 
#' Kaplan, E. and Meier, P. (1958). Nonparametric Estimation from Incomplete 
#' Observations, Journal of the American Statistical Association 53(282), 
#' 457-481. 
#' @seealso \code{\link{LDM3df}}, \code{\link{LIN3df}} and \code{\link{WCH3df}}.
#' 
#' @examples
#' data("bladder5state")
#' b4state<-multidf(gap1=bladder5state$y1, event1=bladder4state$d1, 
#'                  gap2=bladder5state$y2, event2=bladder4state$d2,
#'                  gap3=bladder5state$y3, status=bladder4state$d3)
#'                  
#' head(b4state)[[1]]
#' 
#' LDM3df(b4state, x=13, y=20, z=40)
#' 
#' b4<-multidf(gap1=bladder4$t1, event1=bladder4$d1, 
#'             gap2=bladder4$t2-bladder4$t1, event2=bladder4$d2,
#'             gap3=bladder4$t3-bladder4$t2, status=bladder4state$d3)
#'
#' LDM3df(b4,x=13,y=20,z=40)
#'
#' @author Gustavo Soutinho and Luis Meira-Machado

LDM3df <-
function(object, x, y, z)
{
  obj <- object[[1]]

  p0 <- which(obj[,'time1'] <= x & obj[,'time2']-obj[,'time1'] <= y) #S pag.6
  time2 <- obj[,'time2'] - obj[,'time1']
  time3 <- obj[,'time'] - obj[,'time2']
  ntime3 <- time3[p0] 
  p1 <- which(ntime3 <= z)
  G <- KMW(obj[p0,'time'], obj[p0,'status']) 
  
  #db3 <- multidf(time1=obj$time1, event1=obj$event1,
  #              time=obj$time2,status=obj$event2)
  
  
  db3 <- multidf(gap1=obj$time1, event1=obj$event1,
                 gap2=obj$time2-obj$time1,status=obj$event2)
  
  est <- LDMdf(object=db3, x=x, y=y) * sum(G[p1])  #KM relativamente aos dois anteriores

  return(est)
}
