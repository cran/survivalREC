% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Beran.R
\name{Beran}
\alias{Beran}
\title{Estimation of the conditional distribution function of the response, given 
the covariate under random censoring.}
\usage{
Beran(time, status, covariate, delta, x, y, kernel = "gaussian", bw, 
lower.tail = FALSE)
}
\arguments{
\item{time}{The survival time of the process.}

\item{status}{Censoring indicator of the total time of the process; 0 if the 
total time is censored and 1 otherwise.}

\item{covariate}{Covariate values for obtaining estimates for the conditional
probabilities.}

\item{delta}{Censoring indicator of the covariate.}

\item{x}{The first time (or covariate value) for obtaining estimates for the 
conditional probabilities. 
If missing, 0 will be used.}

\item{y}{The total time for obtaining estimates for the conditional 
probabilities.}

\item{kernel}{A character string specifying the desired kernel. See details 
below for possible options. 
Defaults to "gaussian" where the gaussian density kernel will be used.}

\item{bw}{A single numeric value to compute a kernel density bandwidth.}

\item{lower.tail}{logical; if FALSE (default), probabilities are 
P(T > y|Z = z) otherwise, P(T <= y|Z = z).}
}
\value{
Vector with the estimation of the conditional distribution function 
of the response, given the covariate under random censoring.
}
\description{
Computes the conditional survival probability P(T > y|Z = z)
}
\details{
Possible options for argument window are "gaussian", "epanechnikov",
 "tricube", "boxcar",
"triangular", "quartic" or "cosine"
}
\examples{
data("bladder4state")
b3state<-multidf(gap1=bladder4state$y1, event1=bladder4state$d1, 
                 gap2=bladder4state$y2, status=bladder4state$d2, 
                 size=bladder4state$size)

head(b3state[[1]])

##P(T>y|size=3)
library(KernSmooth)

obj0 <- b3state[[1]]

h <- dpik(obj0$size)
Beran(time = obj0$time, status = obj0$status, covariate =obj0$size, x = 3, 
y = 50, bw = h)

##P(T<=y|size=3)
Beran(time = obj0$time, status = obj0$status, covariate =obj0$size, x = 3,
 y = 50, bw = h,
lower.tail = TRUE)

}
\references{
R. Beran. Nonparametric regression with randomly censored survival
 data. Technical report, University of California, Berkeley, 1981.
}
\author{
Gustavo Soutinho and Luis Meira-Machado
}
