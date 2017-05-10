options(digits=7) # set the number of significant digits
l <- function(x, n=4) format(round(x,n), nsmall=n) # nicer printing of floats

standardize <- function(df) {
    # standardizing all columns in a dataframe
    sapply(as.data.frame(lapply(df,as.numeric)), function(col) {
        sapply(col, function(elem) (elem - mean(col))/sd(col))}
    )
}

standardize2 <- function(df, ref=df) {
    # standardizing all columns in a dataframe
    cols <- names(df)
    df2 <- as.data.frame(lapply(df,as.numeric))
    ref2 <- as.data.frame(lapply(ref,as.numeric))
    sapply(cols, function(col) {
        sapply(df2[,col], function(elem) (elem - mean(ref2[,col]))/sd(ref2[,col])) # this didin't work without the commas
        }
    )
}

    # 3 REGRESSION

mse <- function(sim, obs, ...) mean( (sim - obs)^2, na.rm = TRUE)

    # 4 CLASSIFICATION

qfit <- function(t) {
    # t is a table of Yes/No classification, prediction vs. true values
    # assumes:
    # dims: vertical: predictions; horizontal: true values
    # values: "Yes" and "No"
    xTP <- t["Yes","Yes"]
    xTN <- t["No","No"]
    xFP <- t["Yes","No"]
    xFN <- t["No","Yes"]
    xYes <- xTP + xFN
    xNo <- xTN + xFP
    xPosit <- xTP + xFP
    xNegat <- xTN + xFN
    xCorrect <- xTP + xTN
    xWrong <- xFP + xFN
    xAll <- sum(t)
    data.frame(Value = c(xCorrect/xAll, xTP/xYes, xTN/xNo, xWrong/xAll, xTP/xPosit, xTN/xNegat),
               row.names = c("Accuracy", "Sensitivity", "Specificity", "Error Rate", "Posit.Pred.Val", "Neg.Pred.Val"))
}
qfitt <- function(pred, actual) qfit(table(pred, actual))

# p.134
# extension for a logistic model
# select the proper cutoff to get the classification

# plot the different measures vs. cutoff level, for visual selection
fSuccess <- function(y) as.numeric(y)-1 # pass the training y
fFailure <- function(y) 2-as.numeric(y) # pass the training y
fT <- function(y) sum(fSuccess(y)) # pass the training y
fF <- function(y) sum(fFailure(y)) # pass the training y
fP <- function(x,m,yhat=m$fitted.values) sum(yhat > x)
fN <- function(x,m,yhat=m$fitted.values) sum(yhat <= x)
fTP <- function(x,y,m,yhat=m$fitted.values) sum(fSuccess(y)[yhat > x])
fFP <- function(x,y,m,yhat=m$fitted.values) sum(fFailure(y)[yhat > x])
fFN <- function(x,y,m,yhat=m$fitted.values) sum(fSuccess(y)[yhat <= x])
fTN <- function(x,y,m,yhat=m$fitted.values) sum(fFailure(y)[yhat <= x])
fSens <- function(x,y,m,yhat=m$fitted.values) fTP(x,y,m,yhat)/fT(y)
fSpec <- function(x,y,m,yhat=m$fitted.values) fTN(x,y,m,yhat)/fF(y)
fAccu <- function(x,y,m,yhat=m$fitted.values) mean(fSuccess(y) == (yhat > x))
fPPV <- function(x,y,m,yhat=m$fitted.values) fTP(x,y,m,yhat)/fP(x,y,yhat)
fNPV <- function(x,y,m,yhat=m$fitted.values) fTN(x,y,m,yhat)/fN(x,y,yhat)
# plotting example
require(ISLR)
attach(Smarket)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
par(mfrow=c(1,1))
# Plot function accepts functions or expressions with only ONE FREE variable,
# because of this you need to generate one-argument functions
# by setting the other two arguments (y, m) to the data and model at hand.
tmpfSens <- function(x) fSens(x, y=Direction, m=glm.fit)
tmpfSpec <- function(x) fSpec(x, y=Direction, m=glm.fit)
tmpfAccu <- function(x) fAccu(x, y=Direction, m=glm.fit)
# Without vectorize the whole vector of all possible cutoff values
# is passed to the measure functions, raising errors.
# After vectorize the functions are applied to the cutoff values vector element-wise,
# i.e. single values are passed to the functions.
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)

# example from
# http://stats.stackexchange.com/questions/25389/obtaining-predicted-values-y-1-or-0-from-a-logistic-regression-model-fit
# using a cutoff of cut, calculate sensitivity, specificity, and classification rate
cutoff_evaluation <- function(cut, m, y) {
    yhat <- (m$fit>cut)
    true_yes <- which(y==1)
    sensitivity <- mean( yhat[true_yes] == 1 ) 
    specificity <- mean( yhat[-true_yes] == 0 ) 
    c.rate <- mean( y==yhat ) 
    d <- cbind(sensitivity,specificity)-c(1,1)
    d <- sqrt( d[1]^2 + d[2]^2 ) # Euclidian distance
    out <- t(as.matrix(c(sensitivity, specificity, c.rate,d)))
    colnames(out) <- c("sensitivity", "specificity", "c.rate", "distance")
    return(out)
}
# data y simulated from a logistic regression model 
# with with three predictors, n=10000
x = matrix(rnorm(30000),10000,3)
lp = 0 + x[,1] - 1.42*x[2] + .67*x[,3] + 1.1*x[,1]*x[,2] - 1.5*x[,1]*x[,3] +2.2*x[,2]*x[,3] + x[,1]*x[,2]*x[,3]
p = 1/(1+exp(-lp))
y = runif(10000)<p
# fit a logistic regression model
mod = glm(y~x[,1]*x[,2]*x[,3],family="binomial")
s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,4)
for(i in 1:1000)
    OUT[i,]=cutoff_evaluation(s[i],mod,y)
par(mfrow=c(1,1))
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
lines(s,OUT[,4],col="darkred",lwd=2)
box()
legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))


# formulae creation automation
# http://stackoverflow.com/questions/4951442/formula-with-dynamic-number-of-variables
i <- 1
s <- paste0("I(horsepower^",i,")")
s <- paste0("y~", s)
as.formula(s)