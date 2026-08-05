## ----init-----------------------------------------------------------------
## Set the working directory
setwd(".")
## Use the ctsmr package
library(ctsmr)


## ----definemodel----------------------------------------------------------
## Initialize the model
model <- ctsm$new()
## Specify the system equations
model$addSystem(dT1 ~ ( 1/(C1*R2) * (T2-T1) + 1/(C1*R1) * (Te-T1) ) * dt + s11 * dw1)
model$addSystem(dT2 ~ ( 1/(C2*R3) * (Ti-T2) + 1/(C2*R2) * (T1-T2) ) * dt + s22 * dw2)
## The inputs
model$addInput("Te","Ti")
## The observation equation
model$addObs( q ~ 1/R3*(Ti-T2) )
model$setVariance(q ~ s)


## ----initvalues,tidy=FALSE------------------------------------------------
## Set initial values of the states for the first time point
model$setParameter( T10 = c(init=15   ,lb=10 ,ub=20 ) )
model$setParameter( T20 = c(init=25   ,lb=20 ,ub=30 ) )
## Set intial values 
model$setParameter( C1 =  c(init=100  ,lb=0  ,ub=200) )
model$setParameter( R1 =  c(init=2    ,lb=0  ,ub=5  ) )
model$setParameter( R2 =  c(init=2    ,lb=0  ,ub=5  ) )
model$setParameter( C2 =  c(init=50   ,lb=0  ,ub=100) )
model$setParameter( R3 =  c(init=1    ,lb=0  ,ub=5  ) )
model$setParameter( s11 = c(init=0.01 ,lb=0  ,ub=1  ) )
model$setParameter( s22 = c(init=0.01 ,lb=0  ,ub=1  ) )
model$setParameter( s =   c(init=0.01 ,lb=0  ,ub=1  ) )


## ----data-----------------------------------------------------------------
## Read the data
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

