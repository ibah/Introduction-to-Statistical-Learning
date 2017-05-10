

    # 5 RESAMPLING METHODS

# p.176, p.177
library(ISLR)
attach(Auto)
?Auto
names(Auto)
dim(Auto)
str(Auto)
summary(Auto)
# missing data
sum(is.na(Auto))
nrow(Auto) # 397
tail(Auto) # 392-397
sum(complete.cases(Auto)) #392
Auto <- Auto[complete.cases(Auto),]
nrow(Auto) # 392
tail(Auto) # 392-397, so the row indeces reminded the same, though the number of rows decreased
n <- nrow(Auto)
Auto[n,] # returns the last row i.e. indexed 397
# dropping rows with NA's

# lm1
lm.fit1 <- lm(mpg~horsepower, data=Auto)
summary(lm.fit1) # R^2 = 0.61
mse(mpg,lm.fit1$fitted.values) # 23.9
par(mfrow=c(1,1))
plot(mpg~horsepower)
abline(lm.fit1, lwd=2, col="blue")
par(mfrow=c(2,2))
plot(fit1) # looks like quadratic
# lm2
lm.fit2 <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
summary(lm.fit2) # R^2 = 0.69, much higher
mse(mpg,lm.fit2$fitted.values) # 19.0, much lower
# lm3
lm.fit3 <- lm(mpg ~ horsepower + I(horsepower^2) + I(horsepower^3), data=Auto)
summary(lm.fit3) # R^2 = 0.69, so no real improvement
mse(mpg,lm.fit3$fitted.values) # 18.9, slightly better (not discernable)
# plotting
par(mfrow=c(1,1))
plot(mpg ~ horsepower)
abline(lm.fit1, lwd=2, col="blue")
# adding the non-linear models:
plot_x <- 30:300
plot_pred2 <- predict(lm.fit2, data.frame(horsepower=plot_x))
lines(plot_x, plot_pred2, lwd=2, col="green")
plot_pred3 <- predict(lm.fit3, data.frame(horsepower=plot_x))
lines(plot_x, plot_pred3, lwd=2, lty=2, col="red")
# the lm3 gives no improvement, almost the same fitted line (so just increased variability)

#######################################
# adding quadratic model 2 - failed
plot(horsepower, fitted(lm.fit2), type="l", col="green", add=T) # different length of x, y due to NA's
plot(na.omit(horsepower), fitted(lm.fit2), type="l", col="green", add=T) # unordered horsepower, can't be used for plotting a line
plot(sort(horsepower, na.last=NA), fitted(lm.fit2), type="l", col="green")
points(na.omit(horsepower), fitted(lm.fit2), col="green")
#######################################

# p.177
# Plotting overvitting models - MSE on training data
mse_table <- data.frame(DegreeOfPolynomial=1:10,
                        MeanSquaredError=vector("numeric",length=10))
m <- "mpg~"
for(i in 1:10) {
    m <- paste0(m, "+I(horsepower^",i,")")
    ValidationResults$MeanSquaredError[i] <- mse(lm(as.formula(m), Auto)$fitted.values, Auto$mpg)
}
plot(ValidationResults, lwd=2, type="b", pch=19, col="red", ylim=c(17,29))
# see the constantly improving MSE

# Now splitting the observations/data into training and validation
# Figure 5.2 (left), p.178
inTrain <- sample(392, 196, replace = F)
training <- Auto[inTrain,]
validating <- Auto[-inTrain,]
obs <- validating$mpg
# Evaluating the models containing 1 to 10 degree polynomials of horsepower
ValidationResults <- data.frame(DegreeOfPolynomial=1:10,
                                MeanSquaredError=vector("numeric",length=10))
m <- "mpg~"
for(i in 1:10) {
    m <- paste0(m, "+I(horsepower^",i,")") # you could do the same with the poly() function, see the LAB
    lm.fit <- lm(as.formula(m), training)
    sim <- predict(lm.fit, validating)
    ValidationResults$MeanSquaredError[i] <- mse(sim, obs)
}
plot(ValidationResults, lwd=2, type="b", pch=19, col="red", ylim=c(17,29))
# now it looks like the quadratic model is best
# and degree>2 create overfitting (but on other runs of this code I got only more mixed results)

# Evaluating the models on 10 different data splits
# Figure 5.2 (right), p.178
plot(1, xlim=c(1,10),ylim=c(17,29), type="n",
     xlab="Degree of Polynomial", ylab="Mean Squared Error")
for(j in 1:10) {
    inTrain <- sample(392, 196, replace = F)
    training <- Auto[inTrain,]
    validating <- Auto[-inTrain,]
    obs <- validating$mpg
    ValidationResults <- data.frame(DegreeOfPolynomial=1:10,
                                    MeanSquaredError=vector("numeric",length=10))
    m <- "mpg~"
    for(i in 1:10) {
        m <- paste0(m, "+I(horsepower^",i,")")
        lm.fit <- lm(as.formula(m), training)
        sim <- predict(lm.fit, validating)
        ValidationResults$MeanSquaredError[i] <- mse(sim, obs)
    }
    lines(ValidationResults, col=j+1, lwd=2) 
}
# now the results are crazy


# p.178
# 5.1.2 Leave-One-Out Cross-Validation (LOOCV)
# do not make LOOCV here as it is computationaly demanding + there's a shortcut that should be implemented


# p.181
# 5.1.3 k-Fold Cross-Validation


# spliting into folds - different methods
n <- 392
k <- 10
# 1. simplest folds: 1,2,3...10,1,2,3...
folds <- rep(1:k, length.out=n) # 
# 2. continuous folds
fold_size <- floor(n/k)
left_overs <- n - k*fold_size # just 2 leftovers, that's OK
folds <- rep(1:k, each=fold_size, length.out=n) # continuous folds, the left overs are added to the fold #1
folds[n+1-left_overs:1] <- k # the left overs are added to the last fold (#10)
# 3. random folds
fold_size <- floor(n/k)
folds <- sample(n, replace=F)
folds <- floor(folds/fold_size+1)
table(folds)
folds[folds==k+1] <- sample(k,1) # assign a random fold to all left overs
table(folds)

# 10-fold validation on the Auto data set - plotting each individual validation
# shows huge variation
# (a figure not present in the book)
plot(1, xlim=c(1,10),ylim=c(5,40), type="n",
     xlab="Degree of Polynomial", ylab="Mean Squared Error",
     main="Test MSE on each of 10 validation sets (10-fold CV)")
n <- 392
k <- 10
# 3. random folds
fold_size <- floor(n/k)
folds <- sample(n, replace=F)
folds <- floor(folds/fold_size+1)
table(folds)
folds[folds==k+1] <- sample(k,1) # assign a random fold to all left overs
for(j in 1:10) {
    inTrain <- folds != j
    training <- Auto[inTrain,]
    validating <- Auto[!inTrain,]
    obs <- validating$mpg
    ValidationResults <- data.frame(DegreeOfPolynomial=1:10,
                                    MeanSquaredError=vector("numeric",length=10))
    m <- "mpg~"
    for(i in 1:10) {
        m <- paste0(m, "+I(horsepower^",i,")")
        lm.fit <- lm(as.formula(m), training)
        sim <- predict(lm.fit, validating)
        ValidationResults$MeanSquaredError[i] <- mse(sim, obs)
    }
    lines(ValidationResults, col=j+1, lwd=2) 
}
# plotting fold 1
plot(mpg~horsepower, col=as.integer(folds==1)+1, pch=19)

# proper 10-fold validation of the 10 models on the Auto data set
# (a figure not present in the book)
I <- 10 # number of models
n <- 392
k <- 10
fold_size <- floor(n/k)
res <- matrix(nrow=I, ncol=k)
folds <- sample(n, replace=F) # random folds
folds <- floor(folds/fold_size+1)
table(folds)
folds[folds==k+1] <- sample(k,1) # assign a random fold to all left overs
table(folds)
for(j in 1:10) {
    inTrain <- folds != j
    training <- Auto[inTrain,]
    validating <- Auto[!inTrain,]
    obs <- validating$mpg
    m <- "mpg~"
    for(i in 1:10) {
        m <- paste0(m, "+I(horsepower^",i,")")
        lm.fit <- lm(as.formula(m), training)
        sim <- predict(lm.fit, validating)
        res[i,j] <- mse(sim, obs)
    }
}
cv_mse <- apply(res,1,mean)
plot(cv_mse, lwd=2, type="b", pch=19, col="red",
     xlab="Degree of Polynomial", ylab="Mean Squared Error",
     main="Test MSE according to a 10-fold CV")

# 9x proper 10-fold validation of the 10 models on the Auto data set
# Figure 5.4 (right), p.180
plot(1, xlim=c(1,10),ylim=c(18,25), type="n",
     xlab="Degree of Polynomial", ylab="Mean Squared Error",
     main="Test MSE according to a 10-fold CV run 9 times")
CV_MAX <- 9 # number of times CV(10) is performed
I <- 10 # number of models
n <- 392
k <- 10
fold_size <- floor(n/k)
res <- matrix(nrow=I, ncol=k)
for(CV_count in 1:CV_MAX) {
    folds <- sample(n, replace=F) # random folds
    folds <- floor(folds/fold_size+1)
    folds[folds==k+1] <- sample(k,1) # assign a random fold to all left overs
    for(j in 1:10) {
        inTrain <- folds != j
        training <- Auto[inTrain,]
        validating <- Auto[!inTrain,]
        obs <- validating$mpg
        m <- "mpg~"
        for(i in 1:10) {
            m <- paste0(m, "+I(horsepower^",i,")")
            lm.fit <- lm(as.formula(m), training)
            sim <- predict(lm.fit, validating)
            res[i,j] <- mse(sim, obs)
        }
    }
    cv_mse <- apply(res,1,mean)
    lines(cv_mse,  col=CV_count+1, lwd=2)
}
# now we see that the 10-fold CV results are much more stable


# p.187
# 5.2 The Bootstrap

# p.188-p.189
library(ISLR)
plot(Portfolio, pch=19, col="green")
# alpha.fn
alpha.fn <- function(data, index=1:nrow(data)) {
    # estimate of alpha based on:
    # - (5.7) formula for alpha minimizing the variance of returns from X and Y
    # - observations select by index
    X <- data$X[index]
    Y <- data$Y[index]
    (var(Y) - cov(X,Y)) / (var(X) +  var(Y) - 2*cov(X,Y))
}
n <- nrow(Portfolio)
B <- 1000
alphas <- rep(0,B)
set.seed(1)
for (r in 1:B) {
    index <- sample(n, replace=T)
    alphas[r] <- alpha.fn(Portfolio, index)
}
# plotting the bootstrapped sample of alphas
true_alpha <- 0.6
par(mfrow=c(1,2))
hist(alphas, col="blue")
abline(v=true_alpha, lwd=2, col="red")
boxplot(alphas, col="blue", sub="Bootstrap")
abline(h=true_alpha, lwd=2, col="red")
# estimated bootstrapped SE of alpha
alpha_hat <- alpha.fn(Portfolio) # 0.5758, correct (as by boot function)
# tmp <- 0
# for (r in 1:B) {
#     tmp <- tmp + (alphas[r] - mean(alphas))^2
# }
# SE of alpha
sqrt(sum((alphas - mean(alphas))^2) / (B-1))
# 0.09481159, this is close but different than by boot() function
# I guess it's due to the randomness of the bootstrap process






    # p.190
    # 5.3 Lab: Cross-Validation and the Bootstrap


# p.191
# 5.3.1 The Validation Set Approach

# Auto data set
library(ISLR)
nrow(Auto)
# selecting the validation set
set.seed(1)
inTrain <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=inTrain)
attach(Auto)
# estimating test MSE
mean((mpg - predict(lm.fit, Auto))[-inTrain]^2) # 26.14
#mean((mpg[-inTrain] - predict(lm.fit, Auto[-inTrain,]))^2) # 26.14
#obs <- mpg[-inTrain]
#sim <- predict(lm.fit, Auto[-inTrain,])
#mse(sim, obs)
# estimating test MSE for the polynomial and cubic regressions
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=inTrain)
mean((mpg - predict(lm.fit2, Auto))[-inTrain]^2) # 19.82
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=inTrain)
mean((mpg - predict(lm.fit3, Auto))[-inTrain]^2) # 19.78
# selecting another validation set (for comparison)
set.seed(2)
inTrain <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=inTrain)
# estimating test MSE
mean((mpg - predict(lm.fit, Auto))[-inTrain]^2) # 23.30
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=inTrain)
mean((mpg - predict(lm.fit2, Auto))[-inTrain]^2) # 18.90
lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=inTrain)
mean((mpg - predict(lm.fit3, Auto))[-inTrain]^2) # 19.26
# conclusion: 2 performs better than 1; 3 should not be preferred to 2

# p.192
# 5.3.2 Leave-One-Out Cross-Validation
# Auto data
glm.fit <- glm(mpg~horsepower, data=Auto) # performs linear regression
coef(glm.fit) # 39.94, -0.16
library(boot)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta # CV results: 24.23, 24.23
# automatically fitting polynomial models
cv.error <- rep(0,5) # vector("numerical",5)
for (i in 1:5) {
    glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
    cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error # 24.23, 19.25, ..., 19.03
plot(cv.error, type='b', col='red', lwd=2, pch=19)
# conclusion: 2 >> 1, 3+ ~~ 2

# p.193
# 5.3.3 k-Fold Cross-Validation
# Auto dataset
set.seed(17)
cv.error.10 <- rep(0,10)
for (i in 1:10) {
    glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
    cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10 # 24.21, 19.19, ..., 19.50
# computation time is much shorter than for LOOCV
# LOOCV does not make use of the shortcut formula...
# conclusion: quadratic model seems best

# p.194
# 5.3.4 The Bootstrap

# Estimating the Accuracy of a Statistic of Interest
# Portfolio data set (X,Y with returns and variances)
library(ISLR)
?Portfolio
nrow(Portfolio)
str(Portfolio)
alpha.fn <- function(data, index) {
    # estimate of alpha based on:
    # - (5.7) formula for alpha minimizing the variance of returns from X and Y
    # - observations select by index
    X <- data$X[index]
    Y <- data$Y[index]
    (var(Y) - cov(X,Y)) / (var(X) +  var(Y) - 2*cov(X,Y))
}
alpha.fn(Portfolio, 1:100) # based on whole data set, 0.576
# example bootstap set
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T)) # 0.596
# bootstrap
set.seed(1)
boot(Portfolio, alpha.fn, R=1000)
# bias: 0.002705445 std.err.: 0.09197062 sum: 0.09467607

# p.195
# Estimating the Accuracy of a Linear Regression Model

# Auto data set

# Linear model
boot.fn <- function(data, index) {
    coef(lm(mpg~horsepower, data=data, subset=index))
}
# estimates of beta.0 and beta.1
boot.fn(Auto,1:392)
# bootstrapped estimates of beta.0 and beta.1
set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))
# compute std.err. of 1,000 bootstrapped estimates of betas
boot(Auto, boot.fn, 1000)
# Bootstrap Statistics :
#     original      bias    std. error
# t1* 39.9358610  0.02972191 0.860007896
# t2* -0.1578447 -0.00030823 0.007404467
summary(lm(mpg~horsepower, data=Auto))$coef
# Estimate  Std. Error   t value      Pr(>|t|)
# (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
# horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81
# Discussion:
# SE estimates (by summary) depend on:
# - estimate of sigma^2 assumes the linear model is correct (but there's nonlinearity)
# - x.i are fixed and all variation comes from epsilons
# Bootstrap likely gives better estimates

# Quadratic model
boot.fn <- function(data, index) {
    coef(lm(mpg~horsepower+I(horsepower^2), data=Auto, subset=index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
# Here the differences are smaller as the quadratic model fits the data better
# (so the assumptions behind the standard formulas for SE(betas) are better satisfied)

