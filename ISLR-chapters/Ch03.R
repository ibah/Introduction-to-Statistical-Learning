# 3. LINEAR REGRESSION
# p.59




# REVIEW HERE





# p.90, p.91
Auto=read.table("files/Auto.data", header =T,na.strings ="?")
str(Auto)
dev.off()
attach(Auto)
X.2 = horsepower^2
X.3 = horsepower^3
X.4 = horsepower^4
X.5 = horsepower^5
fit1 = lm(mpg~horsepower)
fit2 = lm(mpg~horsepower+X.2)
fit3 = lm(mpg~horsepower+X.2+X.3+X.4+X.5)
plot(horsepower, mpg, ylab="Miles per gallon", xlab="Horsepower")
# reg. 1
abline(fit1, col="orange", lw=3)
# reg. 2
x = seq(40,250,1)
b2 = summary(fit2)$coefficients[,1]
# y2 = b[1] + b[2]*x + b[3]*x^2
# the same better, checking & preparing
head(data.frame(b[1]*x^0,b[2]*x,b[3]*x^2))
head(sapply(1:3, function(i) b[i]*x^(i-1)))
head(apply((sapply(1:3, function(i) b[i]*x^(i-1))),1,sum))
# the function
polynomial <- function(x,b,n=length(b)) {
    apply((sapply(1:n, function(i) b[i]*x^(i-1))),1,sum)
}
# applying
y2 = polynomial(x, b2)
lines(x, y2, col="blue", lw=3)
# reg. 3
b3 = summary(fit3)$coefficients[,1]
y3 = polynomial(x, b3)
lines(x, y3, col="green", lw=3)
# the same with qqplot2
library(ggplot2)
qplot(horsepower, mpg, data=Auto, ylab="Miles per gallon", xlab="Horsepower")
ggplot(Auto, aes(horsepower, mpg)) +
    geom_point() +
    labs(y="Miles per gallon", x="Horsepower")
# How to add reg. lines to this?

# p.92
# improve this plot: correct scale, add trend line
plot(na.omit(horsepower), summary(fit1)$residuals, col="orange")
points(na.omit(horsepower), summary(fit2)$residuals, col="blue")
points(na.omit(horsepower), summary(fit3)$residuals, col="green")





# REVIEW HERE






# 3.6 Lab: Linear Regression
# P.109

# p.109
library(MASS) # data and functions
library(ISLR) # for this book

# p.109
# 3.6.2 Simple Linear Regression
?Boston # medv (median house value) for 506 neighborhoods around Boston
# Task: predict medv using 13 predictors
# such as rm (average number of rooms per house), age (average age of houses),
# and lstat (percent of households with low socioeconomic status)
fix(Boston)
names(Boston)
# simle lin.reg.
lm.fit = lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit # Call, Coefficients
summary(lm.fit) # more info
names(lm.fit)
lm.fit$coefficients # avoid this
coef(lm.fit) # safer - extractor function
confint(lm.fit) # CI for beta's
# Now: intervals for fitted values / ^y / predictions of medv (given X's values):
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval='confidence')
# fit + conf interval: (????)
# = with 95% prob. the true (population) mean of medv(lstat given) lie within this CI
# = repeated samples (each of many observations) will yield CI's such that
#   the true mean of medv will lie within 95% of them
# i.e. it's about catching the true population mean of y (conditions given)
# i.e. residual errors even out
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval='prediction')
# fit + pred interval (much wider): (????)
# = with 95% prob. the next observation of medv(lstat given) will lie within this PI
# = repeated observations for medv(lstat given) will lie in 95% cases in this PI
# i.e. it's about predicting the next observation
# i.e. accounting for the (being high here) variability of residual errors
# Remark:
# Imagine two populations X1~N(0,1) and X2~N(0,10).
# The CI will be 10 times wider for X2
# but the more observations you collect for each population the narrower the CI's will be for each estim. of mean of X.
# However the PI won't be much affected by the amount of observations
# as the uncertainty of the next observation of X will only partially (???) decrease (S^2 depends on n-1)
# by the number of observations of X made so far, only the uncertainty will be more accurately estimated
# (the PI's will be more precisely centered at 0 and their widths will be based on more precise estimates of the SE's).
# plot medv and lstat along with the least squares regression
plot(lstat, medv)
abline(lm.fit) # evid. of non-lin. -> see later, e.g. try adding lstat^2
# experim. with plot and abline functions:
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col ="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)
# diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit) # or printed separatelly one by on using enter
# Interpretation:
# 1. Resid. vs. Fitted: non-lin (add lstat^2)
# 2. Std.Resid. vs. Theo.Quant.: ???? (Normal Q-Q)
# light tailed (???) or right skewed (????)
# 3. sqrt(Std.Resid.) vs. Fit.Val.: ???? (Scale-Location)
# heteroscedasticity: highest var. for more extreme lstat values
# lowess curve
# 4. Std.Resid. vs. Lever.: several high-leverage points, possible outliers
# Cook distance = estimate of the influence of a data point for LS reg.
# alternatively: use residuals() and rstudent() (studendized residuals)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# calc. the leverage statistics: hatvalues(), for any number of predictors
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
par(mfrow=c(1,1))

# my experiment:
lstat2 <- Boston$lstat^2
lm.fit = lm(medv~lstat+lstat2, data=Boston)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
# 1. nicely linear
# 2. light tailed
# 3. heteroscedasticity (higher var for higher ^y)
# 4. outliers mostly with low leverage, just one high leverage outlier (-1.9, point 415)

# p.113
# 3.6.3 Multiple Linear Regression
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
lm.fit=lm(medv~., data=Boston) # on all vars
summary(lm.fit) # age, indus with high p-vals (>0.7)
# accessing the components:
?summary.lm
summary(lm.fit)$r.sq # R^2
summary(lm.fit)$sigma # RSE
library(car)
vif(lm.fit) # detecting collinearity
# all below 10, but tax=9, rad=7; the others < 5
lm.fit1=lm(medv~.-age, data=Boston) # excluding age
summary(lm.fit1)
lm.fit1=update(lm.fit,~.-age) # the same by updating the previous fit

# p.115
# 3.6.4 Interaction Terms
# lstat:age == lstat*age
# lstat*age == lstat + age + lstat*age
summary(lm(medv~lstat*age, data=Boston))

# p.115
# 3.6.5 Non-linear Transformations of the Predictors
# I() == allow for a strandard usage of R inside of I()
# poly() == generates polynomials in formulas
# log()
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
# anova() == computes analysis of variance (or deviance) tables for fitted models
lm.fit=lm(medv~lstat)
anova(lm.fit, lm.fit2) # is adding quadratic term superior in fit?
# lm.fit - a submodel
# lm.fit2 - a full model
# H0: equal fit; Ha: full model is superior
# here: full is superior
par(mfrow=c(2,2))
plot(lm.fit2) # little discern. pattern in residuals
# fifth-order:
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5) # all significant
plot(lm.fit5)
# log
summary(lm(medv~rm, data=Boston))
summary(lm(medv~log(rm), data=Boston))

# p. 117
# 3.6.6 Qualitative Predictors
library(ISLR)
str(Carseats)
fix(Carseats)
names(Carseats)
# predict Sales (child car seat sales) in 400 locations based on a number of predictors
# qualit: ShelveLoc, Urban, US
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
# qualit. vars are signif. and have positive impact (better shelving -> more sales)
attach(Carseats)
# contrasts() == show coding of qualit. vars
contrasts(ShelveLoc)

# p.119
# 3.6.7 Writing Functions [done]

# p.120, Conceptual

# p.121, p.122, Applied
# p.?
choose(2:4,2)