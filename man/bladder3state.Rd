\name{bladder3state}
\alias{bladder3state}
\docType{data}
\title{bladder3state
}
\description{bladder3state data set.}
\usage{data("bladder3state")}
\format{
  A data frame with 85 observations on the following 7 variables.
  \describe{
    \item{\code{id}}{Identification number.}
    \item{\code{y1}}{First gap time.}
    \item{\code{d1}}{Indicator of the first gap time; 0 if the first time is 
    censored and 1 otherwise.}
    \item{\code{y2}}{Second gap time.}
    \item{\code{d2}}{Censoring indicator of the second gap time; 0 if the total 
    time is censored and 1 otherwise.}
    \item{\code{rx}}{Values of covariate rx.}
    \item{\code{size}}{Values of covariate size.}
  }
}
\examples{
data(bladder3state)
str(bladder3state)
}
\keyword{datasets}
