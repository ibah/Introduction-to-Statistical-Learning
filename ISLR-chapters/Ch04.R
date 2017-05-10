# 4 CLASSIFICATION
# THE CHAPTER CONTENT
# p.127

# Data exploration

# p.129
?Default
str(Default)
summary(Default)
attach(Default)
# it looks different than in the book
par(mfrow=c(1,1))
require(plyr)
with(Default, plot(balance, income, cex=0.5,
                   col=mapvalues(as.integer(default),
                                 from=c(1,2),
                                 to=c("blue","red")),
                   pch=mapvalues(as.integer(default),
                                 from=c(1,2),
                                 to=c(1,3))))
par(mfrow=c(1,2))
with(Default, boxplot(balance~default, ylab="balance", col=c("blue","red")))
with(Default, boxplot(income~default, ylab="income", col=c("blue","red")))

# preparing plots in ggplot2
require(ggplot2)
ggplot(Default, aes(x=default,y=balance)) + geom_boxplot()
q <- ggplot(Default, aes(
    x=default,
    y=balance,
    fill=relevel(default,ref="Yes"))) + geom_boxplot()
q
q + guides(fill=FALSE)
q + guides(fill=guide_legend(title=NULL))
q + guides(fill=guide_legend(title="default"))

# the final plot in ggplot2
require(gridExtra)
q1 <- ggplot(Default, aes(
    x=default,
    y=balance,
    fill=relevel(default,ref="Yes"))) + geom_boxplot() + guides(fill=FALSE)
q2 <- ggplot(Default, aes(
    x=default,
    y=income,
    fill=relevel(default,ref="Yes"))) + geom_boxplot() + guides(fill=FALSE)
grid.arrange(q1, q2, ncol=2)
ls(package:ISLR)

# Logistic Model

# p.131
contrasts(Default$default) # 0 = No (base lavel), 1 = Yes, for dummy variables in models
levels(Default$default) # 1 = No, 2 = Yes, coding levels in the factors, as.numeric()
# linear regression fit
prob_of_default = as.integer(Default$default) - 1 # here: true values of 1 or 0 (in the training set)
m1 <- lm(prob_of_default~balance, data=Default)
# logit regression fit
m2 <- glm(default~balance, data=Default, family=binomial)
# plotting fitted lines
par(mfrow=c(1,2))
# linear regression
with(Default, plot(balance, prob_of_default, pch=3, col="brown", cex=0.5))
abline(h=1,lty=2)
abline(h=0,lty=2)
abline(m1, lwd=2, col="blue")
# logistic regression
with(Default, plot(balance, prob_of_default, pch=3, col="brown", cex=0.5))
abline(h=1,lty=2)
abline(h=0,lty=2)
curve(predict(m2, data.frame(balance=x), type="response"),
      lwd=2,
      col="blue",
      add=TRUE) 

# gglpot 2 print
ggplot(Default, aes(x=balance, y=prob_of_default)) + geom_point() + 
    stat_smooth(method="lm", se=FALSE)
ggplot(Default, aes(x=balance, y=prob_of_default)) + geom_point() + 
    stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

# p.134
summary(m2)$coef # just like in the book; nice high z-values
predict(m2, data.frame(balance=c(1000,2000))) # log-odds
predict(m2, data.frame(balance=c(1000,2000)), type="response") # probabilties P(default=Yes|income)
predict(m1, data.frame(balance=c(1000,2000))) # linear regression predictions
# by default predict returns log-odds (logit), not the fitted values
# if 'response' not added:
pred <- predict(m2, data.frame(balance=c(1000,2000)))
pred # log-odds
probs <- exp(pred)/(1+exp(pred))
probs # fitted values
# predict glm:
# type:
# default=link: on the scale of linear predictors (log-odds for logistic regression)
# response: on the scale of the response variable
# terms: matrix of fitted values of each term on the linear predictor scale
# (don't know what it means)

# finding the cutoff that maximizes the accuracy of the model
mean(m2$fitted.values > 0.5)
mean(prob_of_default)
# With 0.5 cutoff the predicted default rate is just 1.4%
# while in input data the rate is 3.3% (so too few defaults predicted).
# Looks like the cutoff should be lower than 0.5...
# ...but that depends on actual data fit and your measure of fit:
# Actual data:
# E.g. you may have very similar default rate predicted, but identifing wrong individuals as defaulters!
# On the other hand you may have much lower default rate predicted but for exactly the right individuals
# and that would give higher accuracy.
# Measure of fit:
# E.g. to increase accuracy -> increase the cutoff.
# to increase sensitivity -> decrease the cutfoff.

# Accuracy
mean((m2$fitted.values > 0.5) == prob_of_default)
# accuracy is 0.9725
accuracy <- function(cutoff, m, true_y) {
    return(mean((m$fitted.values > cutoff) == true_y))
}
accuracy(0.5, m2, prob_of_default) # 0.5 -> 0.9725
# opitimization 1
o <- optim(0.5, function(x,...) -accuracy(x,...),
           m=m2, true_y=prob_of_default) # minimization of -accuracy
o$par
-o$value
o <- optim(0.5, accuracy,
           m=m2, true_y=prob_of_default,
           control=list(fnscale=-1)) # maximization of accuracy
o$par
o$value
accuracy(0.55, m2, prob_of_default)
# 0.55 -> 0.9728 # higher accuracy
# optimization 2
o <- optim(0.5, accuracy,
           m=m2, true_y=prob_of_default,
           control=list(fnscale=-1),
           method="Brent", lower=0, upper=1) # using Brent method
o$par
o$value
accuracy(o$par, m2, prob_of_default)
# other measures of fit
prob_of_solvency <- 1 - prob_of_default
xT <- sum(prob_of_default)
xF <- sum(prob_of_solvency)
xP <- sum(m2$fitted.values > 0.5)
xN <- sum(m2$fitted.values <= 0.5)
xTP <- sum(prob_of_default[m2$fitted.values > 0.5])
xFP <- sum(prob_of_solvency[m2$fitted.values > 0.5])
xFN <- sum(prob_of_default[m2$fitted.values <= 0.5])
xTN <- sum(prob_of_solvency[m2$fitted.values <= 0.5])
# Sensitivity
xTP/xT # 30.0% <-- this might be crucial measure for the bank
# Specificity
xTN/xF # 99.6%
# positive predictive value
xTP/xP # 70.4%
# negative predictive value
xTN/xN # 97.6%

# plot the different measures vs. cutoff level, for visual selection
fP <- function(x) sum(m2$fitted.values > x)
fN <- function(x) sum(m2$fitted.values <= x)
fTP <- function(x) sum(prob_of_default[m2$fitted.values > x])
fFP <- function(x) sum(prob_of_solvency[m2$fitted.values > x])
fFN <- function(x) sum(prob_of_default[m2$fitted.values <= x])
fTN <- function(x) sum(prob_of_solvency[m2$fitted.values <= x])
fSens <- function(x) fTP(x)/xT
fSpec <- function(x) fTN(x)/xF
fAccu <- function(x) mean(prob_of_default == (m2$fitted.values > x))
par(mfrow=c(1,1))
# Without vectorize the whole vector of all possible cutoff values
# is passed to the measure functions, raising errors.
# After vectorize the functions are applied to the cutoff values vector element-wise,
# i.e. single values are passed to the functions.
plot(Vectorize(fSens),0,1, col="red", ylab="Fit", xlab="Cutoff")
plot(Vectorize(fSpec),0,1, col="green", add=T)
plot(Vectorize(fAccu),0,1,col="blue",add=T)
# probably the cutoff 0.2 or even 0.1 is best to get good sensitivity and still acceptable accuracy
fSens(0.2) # 60%
fSens(0.1) # 73%, avoiding 73% of defaulters
fAccu(0.1) # 94%
fSpec(0.1) # 99%, losing just 1% of potential good clients - price worth paying
# close-up
plot(Vectorize(fSens),0,1, col=2, ylab="Fit", xlab="Cutoff", ylim=c(0.96,1))
plot(Vectorize(fSpec),0,1, col=3, add=T)
plot(Vectorize(fAccu),0,1,col=4,add=T) # accuracy has two local maxima to the left and right of 0.5

# Categorical predictors

# p.134, p.135
# adding Student dummy variable to the model
m3 <- glm(default~student, data=Default, family=binomial)
summary(m3)$coefficients # student -> higer probability of default
m3$coefficients
# probabilities
# using predict function
predict(m3,
        newdata=data.frame(student=c("Yes","No"), row.names=c("Student","Other")),
        type="response")
# calculation using the coefficients
e <- exp(m3$coefficients %*%
             matrix(c(1,1,1,0),nrow=2,dimnames=list(c(),c("Student","Other"))))
# the matrix selects variables: (row 1): constant+StudentYes, (row 2): constant
e/(1+e)

# multiple predictors

# p.135
summary(glm(default~.,data=Default,family=binomial))$coef # different order than in the book
m4 <- glm(default~balance+income+student, data=Default, family=binomial)
summary(m4)

# p.136
summary(m4)$coef
# now: student -> less prob of default
# this is a case of confounding
# explanation: on average students have higher balance than non-students,
# so on average they default more often.
# But for the same balance value, students are less likely to default.

# (1) average default rates depending on Student status
with(Default, tapply(as.integer(default)-1,student,mean)) # higher average default rate for students
# (2) average balance depending on Student status
with(Default, tapply(balance,student,mean)) # higher balance for students

# p.137
# (3) default rates depending on Student status, across same balance levels
attach(Default)
# you need to gather observation into balance level buckets,
# so that you can compare default rates for similar balance levels
summary(balance)
min(balance)
max(balance)
# selecting breaks for the buckets
table(cut(balance,seq.int(0,2750, 250))) # checking
# breaks <- c(0,seq.int(250,2250,250)+125,2750)
# these breaks didn't reproduce the book result for the highest values
breaks <- c(0,seq.int(250,2000,250)+125,2750) # these breaks were OK
# obtaining the middle points of the buckets
middle_points <- (tail(breaks,-1) - head(breaks,-1))/2 + head(breaks,-1)
# the buckets/bins
bins <- cut(balance, breaks)
s <- student=="Yes"
# default rates per balance bucket/bin
ds <- tapply(as.integer(default[s])-1, bins[s], mean) # for students
do <- tapply(as.integer(default[!s])-1, bins[!s], mean) # for non-students
max(ds,do, na.rm=T) # set ylim for the plot
# plotting
# red = students, blue = others
par(mfrow=c(1,2))
plot(middle_points, ds, type="l", col="red", lwd=2, ylim=c(0,1),
     ylab="Default rate", xlab="balance")
lines(middle_points, do, col="blue", lwd=2)
abline(h=tapply(as.integer(default)-1,student,mean),
       col=c("blue","red"), lty=2) # first "No" (blue), second "Yes" (red)
plot(balance~student,
     col=mapvalues(as.integer(student),from=c(1,2),to=c("blue","red")))
# counts per Student status, for balance buckets/bins
cs <- tapply(as.integer(default[s])-1, bins[s], length) # for students
co <- tapply(as.integer(default[!s])-1, bins[!s], length) # for non-students
max(cs,co, na.rm=T) # set ylim for the plot
plot(middle_points, cs, type="l", col="red", lwd=2, ylim=c(0,1400),
     ylab="# of individuals", xlab="balance")
lines(middle_points, co, col="blue", lwd=2)
abline(v=tapply(balance,student,mean), # average balance for Students vs Others
       col=c("blue","red"), lty=2) # first "No" (blue), second "Yes" (red)
plot(balance~student,
     col=mapvalues(as.integer(student),from=c(1,2),to=c("blue","red")))

####################### IGNORE
# atttempt 1 - without the proper x-axis
bins <- cut(balance,seq.int(0,2750, 250),labels=F)
s <- student=="Yes"
# default rates per balance bucket
ds <- tapply(as.integer(default[s])-1, bins[s], mean) # for students
do <- tapply(as.integer(default[!s])-1, bins[!s], mean) # for non-students
# plot 1
plot(ds, type="l", col="red", lwd=2, ylab="Default rate", xlab="balance bucket")
lines(do, col="blue", lwd=2)
########################

# predictions
predict(m4, data.frame(
    student=c("Yes","No"),
    balance=1500,
    income=40000,
    row.names=c("Student","Other")), type="response")
# for the same balance and income, student has lower default probability

# the same check for income:
min(income)
max(income)
table(cut(income,seq.int(0,75000, 5000))) # checking
breaks <- c(seq.int(0,70000,5000),75000)
middle_points <- (tail(breaks,-1) - head(breaks,-1))/2 + head(breaks,-1)
bins <- cut(income, breaks)
s <- student=="Yes"
# default rates per income bucket
ds <- tapply(as.integer(default[s])-1, bins[s], mean) # for students
do <- tapply(as.integer(default[!s])-1, bins[!s], mean) # for non-students
# max(ds,do, na.rm=T)
# plotting
par(mfrow=c(1,2))
plot(middle_points, ds, type="l", col="red", lwd=2,# ylim=c(0,1),
     ylab="Default rate", xlab="income")
lines(middle_points, do, col="blue", lwd=2)
abline(h=tapply(as.integer(default)-1,student,mean),
       col=c("blue","red"), lty=2) # first "No" (blue), second "Yes" (red)
plot(income~student,
     col=mapvalues(as.integer(student),from=c(1,2),to=c("blue","red")))
# conclusion:
# Students have on average:
# - much lower income
# - higher default rate
# When checked for fixed income levels:
# - for every level of income, students have higher default rate

# counts per Student status, for income buckets/bins
cs <- tapply(as.integer(default[s])-1, bins[s], length) # for students
co <- tapply(as.integer(default[!s])-1, bins[!s], length) # for non-students
max(cs,co, na.rm=T) # set ylim for the plot
plot(middle_points, cs, type="l", col="red", lwd=2, ylim=c(0,1450),
     ylab="# of individuals", xlab="income")
lines(middle_points, co, col="blue", lwd=2)
abline(v=tapply(income,student,mean), # average balance for Students vs Others
       col=c("blue","red"), lty=2) # first "No" (blue), second "Yes" (red)
plot(income~student,
     col=mapvalues(as.integer(student),from=c(1,2),to=c("blue","red")))
# So income is quite a good classifier for Student.
# If we didn't had student, then income would be a proxy of being a Student.
# Guess: having only balance and income would render income coef. significant and positive
# Check:
summary(glm(default~balance+income,data=Default,family=binomial)) # guess confirmed
summary(m4)

# p.138
# LDA analysis

# p.144
# LDA for Default data
# (extension) some exploratory plots
par(mfrow=c(1,1))
plot(balance, income, cex=0.5,
     col=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c("blue","red")),
     pch=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c(1,3)))
par(mfrow=c(1,2))
s <- student=="Yes"
plot(balance[s], income[s], cex=0.5,
     col=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c("blue","red")),
     pch=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c(1,3)), main="Students")
plot(balance[!s], income[!s], cex=0.5,
     col=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c("blue","red")),
     pch=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c(1,3)), main="Others")
# LDA model
library(ISLR)
attach(Default)
require(MASS)
require(plyr)
# LDA using 2 variables
lda.fit <- lda(default~balance+income,data=Default)
lda.class <- predict(lda.fit, Default)$class
length(lda.class)
length(default)
qfitt(lda.class, default)
# accuracy 97%
# sensitivity 23%
plot(lda.fit) # plotting... ?


lda.fit <- lda(default~balance+income+student,data=Default)
lda.fit
??lda

# tasks:
# properly split into train, test, validate
# try lda, qda, mda, knn (+standardization)
# if standardizing, remember to use the same values for test and validation sets
# plot results to better understand

# redo in carret

















# THE LAB


## p.154, p.155, LAB

# 4.6.1
# Exploration
library(ISLR)
?Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
# no visible pattern
# Direction is determined by/linked with Today ('s return) sign
cor(Smarket) # error
cor(Smarket[,-9])
# no strong correlations except volume & year -> volume increasing over time
# cor (today & lags) -> close to 0
# checking correlation Volume & date
str(1:nrow(Smarket)) # integer date number
str(row.names(Smarket)) # char
day <- 1:nrow(Smarket)
cor(Smarket$Volume,day) # strong corelation, even more than between volume and year
cor(Smarket$Volume,Smarket$Year)
# Plotting
attach(Smarket)
par(mfrow=c(1,1))
plot(Volume) # data ordered day by day
abline(lm(Volume~day), lwd=2, col="blue")
plot(Volume~Year)
abline(lm(Volume~Year), lwd=2, col="blue")
head(Smarket)
tail(Smarket)

# p.156
# 4.6.2 Logistic Regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit) # no significant variables (even Intercept is insignificant)

# p.157
coef(glm.fit) # coefficients
summary(glm.fit)$coef
summary(glm.fit)$coef[,4] # p-values
contrasts(Direction) # 1 means up, so the response probability is P(going up|X=x)

# measures of fit (extension)
# see the generic functions in the extra.R
# checking the generic functions
mean(fSuccess(Direction))
mean(fFailure(Direction))
mean(as.numeric(Direction)-1)
fT(Direction) + fF(Direction)
nrow(Smarket)
fP(0.5,glm.fit)
fN(0.5,glm.fit)
fTP(0.5,Direction,glm.fit)
fSens(0.5,Direction,glm.fit)
fAccu(0.5,Direction,glm.fit)
# all works OK
par(mfrow=c(1,1))
tmpfSens <- function(x) fSens(x, y=Direction, m=glm.fit)
tmpfSpec <- function(x) fSpec(x, y=Direction, m=glm.fit)
tmpfAccu <- function(x) fAccu(x, y=Direction, m=glm.fit)
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)
# exactly as expected
# accuracy is almost flat
# sensitivity and specificity exactly replace each other in a logisitic fashion

# predictions
glm.probs <- predict(glm.fit,type="response")
summary(glm.probs)
summary(glm.fit$fitted.values) # the same
head(glm.probs)
# predicted probabilities of Up, depending on being actualy Up or Down
plot(Direction,glm.probs) # no visible difference, while we want to have the model differentiating the two
plot(Direction~day)
plot(glm.probs~day) # interesting pattern, variability of preditions is date-dependent: decreases over time

# p.158
# converting probs to class (binary responses)
n <- nrow(Smarket)
glm.pred <- rep("Down", n)
glm.pred[glm.probs > .5] <- "Up"
# in-sample error rate
table(glm.pred,Direction) # confusion matrix
accu <- mean(glm.pred == Direction) # accuracy
tmpfAccu(0.5) # the same
1 - accu # training error rate - over optimistic! so close to random guessing


# p.159
# trying to estimate the out-of-sample error rate
table(Year)
inTrain <- Year < 2005
testing <- Smarket[!inTrain,]
dim(testing)
Direction.test <- testing$Direction
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket, subset=inTrain, family=binomial)
glm.probs <- predict(glm.fit, testing, type="response")
glm.pred <- rep("Down", nrow(testing))
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred,testing$Direction)
mean(glm.pred == testing$Direction) # accuracy: 48%
mean(glm.pred != testing$Direction) # out-of-sample error rate estimate: 52%

# plotting the fit measures
par(mfrow=c(1,1))
tmpfSens <- function(x) fSens(x, y=testing$Direction, m=glm.fit, yhat=glm.probs)
tmpfSpec <- function(x) fSpec(x, y=testing$Direction, m=glm.fit, yhat=glm.probs)
tmpfAccu <- function(x) fAccu(x, y=testing$Direction, m=glm.fit, yhat=glm.probs)
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)

# p.160
# the unrelevant variables increase the model variance (without decreasing the bias)
summary(glm.fit)
# so remove all variables except Lag1 and Lag2 (these two have the lowest p-values)
glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket, subset=inTrain, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, testing, type="response")
glm.pred <- rep("Down", nrow(testing))
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred,Direction.test)
mean(glm.pred!=Direction.test) # out of sample error rate 44%, now better than 50%
fPPV(.5, y=testing$Direction, m=glm.fit, yhat=glm.probs) # positive predictive value of 58%
fNPV(.5, y=testing$Direction, m=glm.fit, yhat=glm.probs) # 50%
# plotting the fit measures - see a slight improvement
par(mfrow=c(1,1))
tmpfSens <- function(x) fSens(x, y=testing$Direction, m=glm.fit, yhat=glm.probs)
tmpfSpec <- function(x) fSpec(x, y=testing$Direction, m=glm.fit, yhat=glm.probs)
tmpfAccu <- function(x) fAccu(x, y=testing$Direction, m=glm.fit, yhat=glm.probs)
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)

# p.161
# predicting for specific values
predict(glm.fit,newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1,-0.8),
                                   row.names=c("Scenario 1","Scenario 2")),
        type="response")

# 4.6.3 Linear Discriminant Analysis

# p.161
# the LDA model
require(MASS)
require(plyr)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=inTrain)
summary(lda.fit) # not very helpful to me
lda.fit
# priors: relative counts of Up and Down
# means: means for Lag1 and Lag2, depending on Up and Down
# see tendency: Today: Up -> Lag2: down, Lag1: up
coef(lda.fit)
# coef of LDA: provides linear combination of Lag1 and Lag2
# if Lag1*coef1 + Lag2*coef2 is large -> predict up, else predict down

# plotting
str(Direction) # Down=1, Up=2
plot(Lag1,Lag2,
     col=mapvalues(as.integer(Direction),c(1,2),c("red","green"))
     )
plot(Lag1,Lag2,
     col=as.character(mapvalues(Direction,c("Down","Up"),c("red","green")))
)
plot(Lag1,Lag2,
     col=as.character(revalue(Direction,c(Down="red",Up="green"))) # this one is most readable
     )
# abline(lda.fit, lwd=2, col="blue") # plotting the boundary - wrong, see a solution below (with QDA)
plot(lda.fit) # plotting... ?

# p.162
# predictions
lda.pred <- predict(lda.fit, testing)
names(lda.pred)
class(lda.pred) # a list
sapply(lda.pred, head) # selected class + posterior cond. probs + x (?)
lda.class <- lda.pred$class
table(lda.class,Direction.test) 
mean(lda.class==Direction.test) # 56%
sum(lda.pred$posterior[,"Down"] >= .5) # number of time class "Down" is predicted
sum(lda.pred$posterior[,"Down"] < .5) # number of time class "Up" is predicted
sum(lda.pred$posterior[,"Up"] >= 0.52) # number of days "Up" is predicted with probability of 52% or more

# predicted probabilities
length(Direction.test)
length(lda.pred$posterior) # twice as long, so probably contains probs for each class (so x2) for each obserbation
head(lda.pred$posterior) # a matrix
lda.probs <- lda.pred$posterior[,"Up"] # only probabilities of Today = "Up"
length(lda.probs) # now it's OK

# fit quality
par(mfrow=c(1,1))
tmpfSens <- function(x) fSens(x, y=testing$Direction, m=lda.fit, yhat=lda.probs)
tmpfSpec <- function(x) fSpec(x, y=testing$Direction, m=lda.fit, yhat=lda.probs)
tmpfAccu <- function(x) fAccu(x, y=testing$Direction, m=lda.fit, yhat=lda.probs)
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)
# results very similar to logistic regression model

# p.163
# QDA model
require(MASS)
require(plyr)
qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=inTrain)
qda.fit
# priors and means the same as for LDA
coef(qda.fit) # nothing, as there are linear and quadratic terms

# plotting
plot(Lag1, Lag2, col=as.character(revalue(Direction,c(Down="red",Up="green"))))
l1 <- seq(-4,6,0.02)
l2 <- seq(-4,6,0.02)
l.l <- as.data.frame(expand.grid(l1,l2)) # col.names=c("Lag1","Lag2") could be set here only for a list
names(l.l) <- c("Lag1","Lag2")
l1l <- length(l1)
l2l <- length(l2)
lda.class2 <- as.integer(predict(lda.fit, l.l)$class) # classes are 1 and 2
contour(l1,l2,matrix(lda.class2,l1l,l2l),
        levels=1.5, add=TRUE, d=FALSE, lty=2)
qda.class2 <- as.integer(predict(qda.fit, l.l)$class)
contour(l1,l2,matrix(qda.class2,l1l,l2l),
        levels=1.5, add=TRUE, d=FALSE, lty=2)

# predictions
qda.pred <- predict(qda.fit, testing)
sapply(qda.pred, head) # selected class + posterior cond. probs + x (?)
qda.class <- qda.pred$class
table(qda.class,Direction.test) 
mean(qda.class==Direction.test) # 60%

# predicted probabilities
qda.probs <- qda.pred$posterior[,"Up"] # only probabilities of Today = "Up"

# fit quality
par(mfrow=c(1,1))
tmpfSens <- function(x) fSens(x, y=testing$Direction, m=qda.fit, yhat=qda.probs)
tmpfSpec <- function(x) fSpec(x, y=testing$Direction, m=qda.fit, yhat=qda.probs)
tmpfAccu <- function(x) fAccu(x, y=testing$Direction, m=qda.fit, yhat=qda.probs)
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)
# this little peak at 60% doesn't look like a stable value

# EXTENSION: MDA
# Mixture Discriminant Analysis
library(mvtnorm)
library(mda)
# MDA model
mda.fit <- mda(Direction~Lag1+Lag2, data=Smarket, subset=inTrain) # default: subclasses = 3
mda.fit
# prediction
mda.class <- predict(mda.fit, testing)
# fit quality
table(mda.class, Direction.test)
mean(mda.class == Direction.test) # 57%, so lower than QDA; if 2 or 4 subclasses -> even lower
# plotting
mda.class2 <- as.integer(predict(mda.fit, l.l)) # returns class only
plot(Lag1, Lag2, col=as.character(revalue(Direction,c(Down="red",Up="green"))))
contour(l1, l2, matrix(mda.class2, l1l, l2l),
        levels=1.5, add=TRUE, d=FALSE, lty=2)

# p.163, p.164
# KNN model
library(class)
train.X <- cbind(Lag1, Lag2)[inTrain,]
test.X <- cbind(Lag1, Lag2)[!inTrain,]
train.Direction <- Direction[inTrain]
set.seed(1) # for reproduciblitiy: R randomly breaks ties in KNN classification
# k=1
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test) # 50%, just random guessing
# k=3
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test) # 53%

# CARAVAN INSURANCE DATA

# p.165
# Exploration
?Caravan
names(Caravan)
# Var1-43: sociodem. based on zip code
# Var44-85: product ownership
# Var86 = Y: Purchase
str(Caravan)
# all numerical except Purchase: factor No/Yes
attach(Caravan)
mean(as.integer(Purchase))-1 # 6% Yes
nrow(Caravan) # 5822
sum(is.na(Caravan)) # no missing values

# SIC, KNN pred strongly depends on the scale of variables
# it treats all variables as being expressed in the same units/of equal value
# e.g. 1$ distance = 1 year distance
# so the variable with a large spread of nominal values will drive the predictions
# to solve this problem variables should be standardized

standardized.X <- as.data.frame(scale(Caravan[,-86])) # by defualt a matrix is returned
str(standardized.X)
class(standardized.X)
# checking
var(Caravan[1:2])
var(standardized.X[1:2])

# p.166
# Creating training and testing sets
inTest <- 1:1000
train.X <- standardized.X[-inTest,]
test.X <- standardized.X[inTest,]
train.Y <- Purchase[-inTest]
test.Y <- Purchase[inTest]
# model & predictions
set.seed(1)
# k=1
knn.pred <- knn(train.X, test.X, train.Y, k=1)
str(train.Y)
str(knn.pred)
t <- table(knn.pred, test.Y)
t
mean(knn.pred == test.Y) # accuracy 88%
# compare with a simple rule: predict No
mean(test.Y == "No") # 94% accuracy -> so it has even bigger accuracy than KNN!
# But say we want to identify the customers who will purchase:
# positive predictive value -> to minimize the marketing costs per insurance sold
# sensitivity -> to maximize market share (cover as many potential clients as possible)
# so look at other measures of fit:
qfit(t)
# sensitivity: 15%, PositPredVal: 12% (vs random guessing 6%)

# p.167
# try other values of k
knn.pred <- knn(train.X, test.X, train.Y, k=3)
t <- table(knn.pred, test.Y)
t
qfit(t)
# accuracy: 93%, posit.pred.val: 21%
knn.pred <- knn(train.X, test.X, train.Y, k=5)
t <- table(knn.pred, test.Y)
t
qfit(t)
# accuracy: 93%, posit.pred.val: 27%

# Logistic Model
glm.fit <- glm(Purchase~., data=Caravan, subset=-inTest, family=binomial)
glm.probs <- predict(glm.fit, Caravan[inTest,], type="response")
# default 50% cutoff
glm.pred <- rep("No", length(inTest))
glm.pred[glm.probs > .5] <- "Yes"
qfitt(glm.pred, test.Y) # 93% accuracy but 0% posit.pred.val
# set 25% cutoff
glm.pred <- rep("No", length(inTest))
glm.pred[glm.probs > .25] <- "Yes"
qfitt(glm.pred, test.Y) # 93% accuracy with 33% posit.pred.val
# set 30% cutoff
glm.pred <- rep("No", length(inTest))
glm.pred[glm.probs > .3] <- "Yes"
qfitt(glm.pred, test.Y) # 94% accuracy with 38% posit.pred.val

# SIC: note a problem: here you use the test set to select parameters for trained model
# it means you will overfit to the test set
# to properly estimate a measure of fit of the model one should:
# use say CV to select best model & parameters
# use a separate test set for a one time estimation of goodness of fit of the resulting model
