library(ctsmTMB)

model <- ctsmTMB$new()

## unchanged: dt/dw syntax is identical
model$addSystem(dT1 ~ ( 1/(C1*R2)*(T2-T1) + 1/(C1*R1)*(Te-T1) )*dt + s11*dw1)
model$addSystem(dT2 ~ ( 1/(C2*R3)*(Ti-T2) + 1/(C2*R2)*(T1-T2) )*dt + s22*dw2)

## CHANGED: symbols not strings (split into two calls if it complains)
model$addInput(Te, Ti)

## unchanged
model$addObs(q ~ 1/R3*(Ti-T2))
model$setVariance(q ~ s)

## CHANGED: init/lb/ub -> initial/lower/upper, on every parameter
model$setParameter(
  C1  = c(initial=100,  lower=0, upper=200),
  R1  = c(initial=2,    lower=0, upper=5),
  R2  = c(initial=2,    lower=0, upper=5),
  C2  = c(initial=50,   lower=0, upper=100),
  R3  = c(initial=1,    lower=0, upper=5),
  s11 = c(initial=0.01, lower=0, upper=1),
  s22 = c(initial=0.01, lower=0, upper=1),
  s   = c(initial=0.01, lower=0, upper=1)
)

## CHANGED: T10/T20 params -> fixed initial state (mean vector + covariance).
## You now choose the covariance; the old script didn't have one.
model$setInitialState(list(c(15, 25), diag(c(1, 1))))

X <- read.csv("Paper1_data.csv",sep=";",header=FALSE)
## Set the names of the columns in X
names(X) <- c("t","Te","Ti","q")


## ----plotdata-------------------------------------------------------------
## Plot the data
par(mfrow=c(3,1),mar=c(3,3.5,1,1),mgp=c(2,0.7,0)) # Setup plot
plot(X$t,X$Te,type="l")
plot(X$t,X$Ti,type="l")
plot(X$t,X$q,type="l")


## ----estimate,results="hide"----------------------------------------------
## Execute the estimation
fit <- model$estimate(X)


## ----summary--------------------------------------------------------------
## Print the parameters estimates and the Correlation Matrix of the estimates
summary(fit,extended=TRUE)


## ----comparePrmEstimates,cache=FALSE--------------------------------------
## The parameter estimates in a data.frame
Prm <- data.frame(xm=fit$xm)
row.names(Prm) <- names(fit$xm)
## The true parameter values
Prm$xmtrue <- c(13,25,100,50,1,2,0.5,0.01,0,0)
## Approximately lower 95% confidence bound
Prm$lower <- (fit$xm - 2*fit$sd)
## Approximately upper 95% confidence bound
Prm$upper <- (fit$xm + 2*fit$sd)
## Print it all
##round(Prm,digits=2)
format(Prm,digits=4)


## ----acfAndCP,fig.height=4------------------------------------------------
## Calculate the one-step ahead predictions
tmp <- predict(fit, newdata=X)
## Calculate the residuals and put them with the data in a data.frame X
X$qHat <- tmp$output$pred$q
X$residuals <- X$q - X$qHat
## Auto-correlation function and cumulated periodogram
par(mfrow=c(1,2)) # Setup plot
acf(X$residuals)
cpgram(X$residuals)


## ----timeseriesresiduals--------------------------------------------------
## Plot of inputs, output, one-step ahead predicted output, and residuals
par(mfrow=c(4,1),mar=c(3,3.5,1,1),mgp=c(2,0.7,0)) # Setup plot
plot(X$t,X$Te,type="l")
plot(X$t,X$Ti,type="l")
plot(X$t,X$q,type="l")
lines(X$t,X$qHat,col=2)
plot(X$t,X$residuals,type="l")