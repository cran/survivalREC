#' Inverse probability of censoring weighting estimator for the bivariate
#' distribution function. 
#' @description Provides estimates for the bivariate distribution function based
#' on the Inverse Probability of Censoring Weighting estimator (IPCW).
#' @usage IPCWdf(object, x, y, covariate, cov.value, bw, window = "gaussian")
#' @param object An object of class multidf.
#' @param x The first time for obtaining estimates for the bivariate distribution
#' function.
#' @param y The second time for obtaining estimates for the bivariate distribution
#' function.
#' @param covariate Name of the quantitative covariate.
#' @param cov.value The value of the quantitative covariate.
#' @param bw A single numeric value to compute a kernel density bandwidth. Use 
#' \code{"dpik"} for the \pkg{KernSmooth} package based selector or \code{"np"} 
#' for the \code{'npudensbw'} function of the \pkg{np} package.
#' @param window A character string specifying the desired kernel. See details 
#' below for possible options. Defaults to \code{"gaussian"} where the gaussian 
#' density kernel will be used.
#' @return Vector with the IPWC estimates for the bivariate distribution 
#' function.
#' @references de Una-Alvarez, J. and Meira-Machado, L. (2008). A simple estimator
#' of the bivariate distribution function for censored gap times, Statistics and
#' Probability Letters 78, 2440-2445.
#' @author Gustavo Soutinho and Luis Meira-Machado.
#' @seealso \code{\link{KMWdf}}, \code{\link{LDMdf}}, \code{\link{LINdf}} and 
#' \code{\link{WCHdf}}.
#' @examples 
#' data("bladder4state")
#' 
#' b3state<-multidf(gap1=bladder4state$y1, event1=bladder4state$d1, 
#'                  gap2=bladder4state$y2, status=bladder4state$d2, 
#'                  size=bladder4state$size)         
#' 
#' b3size<-multidf(gap1=bladder3$t1, event1=bladder3$d1, 
#'                 gap2=bladder3$t2-bladder3$t1,status=bladder4state$d2, 
#'                 size=bladder3$size)  
#'

#' library(KernSmooth)
#' 
#' IPCWdf(object=b3state, x=13, y=15, covariate="size", cov.value=3, 
#'        window = "gaussian")
#'        
#' IPCWdf(object=b3state, x=13, y=15, covariate="size", bw=2, cov.value=3, 
#'        window = "gaussian")
#'
#' IPCWdf(object=b3size, x=13, y=15, covariate="size", cov.value=3, 
#'        window = "gaussian")
#'        
#' IPCWdf(object=b3size, x=13, y=15, covariate="size", bw=2, cov.value=3, 
#'        window = "gaussian")
#' 
#' @author Gustavo Soutinho and Luis Meira-Machado

IPCWdf <-
function(object, x, y, covariate, cov.value, bw, window = "gaussian")
{
 obj <- object[[1]]
 est <- 0
 n1 <- length(obj$time)
 be <- rep(1,n1)
 delta4 <- rep(1, n1)
 z.name <- covariate
 z.value <- cov.value
 covar <- which(names(object[[1]]) == z.name)
 
 if (missing(bw)) lbd <- dpik(x = obj[, covar])
 else lbd <- bw
 
 w <- NWW(obj[, covar], z.value, kernel = window, bw = lbd)
 
 for (k in 1:n1) be[k] <- Beran(obj$time, 1 - obj$status, obj[, covar], delta4, 
                                z.value, obj$time[k],
                                kernel = window, bw = lbd)
 
 p <- which(obj$time1 <= x & obj$time - obj$time1 <= y)
 est <- sum(w[p]/be[p])
 return(est)                    
}
