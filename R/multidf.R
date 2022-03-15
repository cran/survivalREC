#' Create a multidf object
#' 
#' @description Creates a "multidf" object, usually used as a response variable 
#' in a model formula.
#' @usage multidf(gap1, gap2, gap3=NULL, event1, status, event2=NULL, ...)
#' @param gap1 First gap time.
#' @param gap2 Second gap time.
#' @param gap3 Third gap time. By default is NULL.
#' @param event1 Indicator of the first time; 0 if the first time is censored 
#' and 1 otherwise.
#' @param status Censoring indicator of the survival time of the process; 0 if 
#' the total time is censored and 1 otherwise. For instance, for three gap times,
#' status is given by the indicator of the third time.
#' @param event2 Indicator of the second time; 0 if the first time is censored 
#' and 1 otherwise. By default is NULL.
#' @param ... Other options. Additional arguments, such as covariates, can also 
#' be included  in the data set.
#' @return An object of class "multidf". "multidf" objects are implemented as a
#' single data frame.

#' @details Arguments in this function must be introduced in the following
#' order: \code{gap1}, \code{event1}, \code{gap2} and \code{status}, where
#' \code{gap1} and \code{gap2} are ordered event times and
#' \code{event1} and \code{status} their corresponding indicator statuses.
#' Other arguments can be also added. These should consider intermediate times 
#' and corresponding censoring indicators or covariates.
#' 
#' @examples
#' library(survivalREC)
#' data("bladder4state")
#' 
#' b3state<-multidf(gap1=bladder4state$y1, event1=bladder4state$d1, 
#'                  gap2=bladder4state$y2, status=bladder4state$d2, 
#'                  size=bladder4state$size) 
#' 
#' head(b3state[[1]])
#' 
#' class(b3state)
#' 
#' b4state<-multidf(gap1=bladder4state$y1, event1=bladder4state$d1, 
#'                  gap2=bladder4state$y2, event2=bladder4state$d2,
#'                  gap3=bladder4state$y3, status=bladder4state$d3, 
#'                  size=bladder4state$size)
#' 
#' head(b4state[[1]])
#'
#' @author Gustavo Soutinho and Luis Meira-Machado
#' 
#' @importFrom "KernSmooth" dpik
#' @importFrom "survival" coxph Surv survfit strata untangle.specials
#' @importFrom "graphics" legend abline axis legend lines matplot par plot polygon
#' @importFrom "stats" pchisq pnorm quantile sd na.omit terms approxfun as.formula rnorm rpois weighted.mean
#' @importFrom "utils" capture.output
#' @importFrom "stats" model.matrix model.frame model.response model.offset  
#' @importFrom "stats" delete.response delete.response

#' @export Beran
#' @export IPCWdf
#' @export KMW
#' @export KM
#' @export KMW3df
#' @export KMWdf
#' @export LDM3df
#' @export LDMdf
#' @export LIN3df
#' @export LINdf
#' @export multidf
#' @export NWW
#' @export plot.multidf
#' @exportS3Method plot multidf
#' @export WCH3df
#' @export WCHdf


multidf <-
    function (gap1, gap2, gap3=NULL, event1, status, event2=NULL, ...)
    {
        if (missing(gap1))
            stop("Argument 'gap1' is missing, with no default")
        if (missing(event1))
            stop("Argument 'event1' is missing, with no default")
        if (missing(gap2))
            stop("Argument 'gap2' is missing, with no default")
        if (missing(status))
            stop("Argument 'status' is missing, with no default")
        
        
        if(is.null(gap3) & !is.null(event2))
            stop("Argument 'gap3' is missing")
        if(!is.null(gap3) & is.null(event2))
            stop("Argument 'event2' is missing")
        
        
        if(is.null(gap3)){
            
            data <- list(time1 = as.double(gap1), time = as.double(gap1+gap2), 
                         event1 = as.integer(event1), status = as.integer(status), ...) 
            
            datalen <- length(data)
            
            if (datalen > 4) {
                datanames <- names(data)
                
                for (i in 5:datalen) {
                    
                    if (!is.numeric(data[[i]]))
                        stop("All additional arguments must be numeric")
                    
                    if (length(data[[i]]) != length(data$time1))
                        stop("All additional arguments must have the same length as 
                     arguments 'time1', 'time', 'event1', and 'status'")
                    
                    if (datanames[i] == "")
                        datanames[i] <- paste("covariate", i - 4, sep = ".")
                    if (!is.double(data[[i]]))
                        data[[i]] <- as.double(data[[i]])
                }
                names(data) <- datanames
            }
            
            attr(data, "row.names") <- as.integer(1:length(data$time1))
            data <- as.data.frame(data)
            object <- vector(mode = "list", length = 1)
            object[[1]] <- na.omit(data)
            
        }else{
            
            data <- list(time1 = as.double(gap1), time2 = as.double(gap1+gap2), time = as.double(gap1+gap2+gap3), 
                         event1 = as.integer(event1), event2 = as.integer(event2), status = as.integer(status), ...) 
            
            datalen <- length(data)
            
            if (datalen > 6) {
                datanames <- names(data)
                
                for (i in 7:datalen) {
                    i<-7
                    if (!is.numeric(data[[i]]))
                        stop("All additional arguments must be numeric")
                    
                    if (length(data[[i]]) != length(data$time1))
                        stop("All additional arguments must have the same length as 
                     arguments 'time1', 'time', 'event1', and 'status'")
                    
                    if (datanames[i] == "")
                        datanames[i] <- paste("covariate", i - 4, sep = ".")
                    if (!is.double(data[[i]]))
                        data[[i]] <- as.double(data[[i]])
                }
                names(data) <- datanames
            }
            attr(data, "row.names") <- as.integer(1:length(data$time1))
            data <- as.data.frame(data)
            object <- vector(mode = "list", length = 1)
            object[[1]] <- na.omit(data)
            
        }
        
        class(object) <- "multidf";
        return(object)
    }
