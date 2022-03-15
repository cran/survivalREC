## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include=TRUE-------------------------------------------------------------
library(survivalREC)
data("bladder4state")
head(bladder4state)
dim(bladder4state)


## ----include=TRUE-------------------------------------------------------------
b3state<-multidf(gap1=bladder4state$y1, event1=bladder4state$d1, 
                 gap2=bladder4state$y2,status=bladder4state$d2, 
                 size=bladder4state$size)       

class(b3state)

b3state[[1]][1:10,]


## ----include=TRUE-------------------------------------------------------------
KMWdf(b3state,x=13,y=20)

LDMdf(b3state,x=13,y=20)

LINdf(b3state,x=13,y=20)

WCHdf(b3state,x=13,y=20)


## ----include=TRUE,  fig.width=7, fig.height=5---------------------------------
plot(x=b3state, t1=3, method="KMW", type = "s", 
     ylab='Prob.', ylim=c(0,.25), 
     xlab='Time to second recurrence')

legend(45,0.03 , legend=c("KMW"), col=c("Black"), lty=1,
         cex=1)

plot(x=b3state, t1=3, method="LANDMARK", type = "s",
     ylab='Prob.', ylim=c(0,.25), 
     xlab='Time to second recurrence')

legend(45,0.03 , legend=c("LDM"), col=c("Black"), lty=1,
       cex=1)

plot(x=b3state, t1=3, method="LIN", type = "s", 
     ylab='Prob.', ylim=c(0,.25), 
     xlab='Time to second recurrence')

legend(45,0.03 , legend=c("LIN"), col=c("Black"), lty=1,
      cex=1)

plot(x=b3state, t1=3, method="WCH", type = "s", ylab='Prob.',
     ylim=c(0,.25), 
     xlab='Time to second recurrence')

legend(45,0.03 , legend=c("WCH"), col=c("Black"), lty=1,
        cex=1)

## ----include=TRUE-------------------------------------------------------------
library(KernSmooth)
IPCWdf(object=b3state, x=13, y=15, 
       covariate="size", cov.value=3, window = "gaussian")

IPCWdf(object=b3state, x=13, y=15, 
       covariate="size", bw=2, cov.value=3, 
       window = "gaussian")

## ----include=TRUE-------------------------------------------------------------
b4state<-multidf(gap1=bladder4state$y1, event1=bladder4state$d1, 
                 gap2=bladder4state$y2, event2=bladder4state$d2,
                 gap3=bladder4state$y3, status=bladder4state$d3, 
                 size=bladder4state$size)   

b4state[[1]][1:10,]

KMW3df(b4state,x=13,y=15,z=10)

LDM3df(b4state,x=13,y=15,z=10)

LIN3df(b4state,x=13,y=15,z=10)

WCH3df(b4state,x=13,y=15,z=10)


