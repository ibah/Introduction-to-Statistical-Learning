# 4 CLASSIFICATION
# THE CHAPTER CONTENT
# p.127
# Data exploration
# p.129
library(ISLR)
?Default
dim(Default)
head(Default)
str(Default)
summary(Default)
attach(Default)
sum(is.na(Default)) # no NA's
n <- nrow(Default) # 1,000

# the response variable
D <- mean(default=='Yes') # 0.0333
plot(default)

# Prediction 1: no
mean(default=='No') #.9667
# sensitivity = 0
# specificity = 1
# pos.pred.val., not exisit. [0/0]
# neg.pred.val. = 0.9667
summary(default)
# all 9667 No picked correctly, all 333 Yes missed

# Prediction 2: random (correctly weighted) guess
pred <- sample(c("Yes","No"), size=n, replace=T, prob=c(D,1-D))
mean(default==pred) # .9363, .9382, accuracy
summary(default[default!=pred])
# Missed 310 No's and 327 Yes's; so much worse than the deterministic rule of always 'No'
# the errors are balanced between wrong "yes" and wrong "no" (say type 1 and 2 errors)

# Investigating predictors

# single predictors
plot(student) # 2x more Other than Student
hist(balance) # left skewed
hist(income) # hump-shaped but with two peaks -> maybe this indicates two sub-groups?
# -> maybe the two groups are students and others?
par(mfrow=c(1,2))
hist(income[student=='No'], col='blue')
hist(income[student=='Yes'], col='red')
library(lattice)
histogram(~income|student)
bwplot(~income|student)
densityplot(~income|student)
# -> very similar bell-shaped distrib.
par(mfrow=c(1,1))
hist(income, col='gray')
abline(v=mean(income[student=='No']), col='blue', lwd=2)
abline(v=mean(income[student=='Yes']), col='red', lwd=2)
# -> OK looks like we pinned it down, there are two distributions overlapped in income
#   one is for Students (low income), the other for non-students (high income).
# Check the same for balance
hist(balance, col='gray')
abline(v=mean(balance[student=='No']), col='blue', lwd=2)
abline(v=mean(balance[student=='Yes']), col='red', lwd=2)
histogram(~balance|student)
bwplot(~balance|student)
densityplot(~balance|student)
# -> the means are similar
#   relatively many non-student have low balance (<100, thick tail, cut at 0)
#   while relatively fewer students have low balance
#   students have on average higher balance than non-studetns
par(mfrow=c(1,2))
hist(balance[student=='No'], col='blue')
hist(balance[student=='Yes'], col='red')
# -> the distributions look bell-shaped but with
#   lower tail observations aggregated close to but above 0.

# Covariance between all variables (predictors and response)
pairs(Default)
# looks like:
# balance+ (higher balance -> default Yes)
# income-
head(Default[3:4])
cov(Default[3:4])
cor(Default[3:4]) # weak negative correlation between income and balance
cor(Default[student=='Yes',3:4]) # uncorrelated for students (weakly negative)
cor(Default[student=='No',3:4]) # uncorrelated for non-students
# -> interesting, the weak correlation is present in combined data
#   but not after the split.
#   Guess: there's no direct relationship. The appearance stems from the fact that
#   students (low income & a bit higher balance)
#   are mixed with others (high income & a bit lower balance)
#   and that creates the slight negative apparent correlation between income & balance.


# Response variable and predictors
# Viewing response (defualt Yes/No) in terms of predictor values


# Student Yes/No
mean(default[student=='Yes']=='Yes') # 4.3%
mean(default[student=='No']=='Yes') # 2.9%
#   -> potentially students are more risky - but control for other variables,
#   especially low income and high balance


# Income & Student
summary(income) # see min and max
table(cut(income,seq.int(0,75000, 5000))) # checking
breaks <- c(seq.int(0,70000,5000),75000)
middle_points <- (tail(breaks,-1) - head(breaks,-1))/2 + head(breaks,-1)
bins <- cut(income, breaks)
d <- default=='Yes'
s <- student=="Yes"
# default rates per income bucket
d_all <- tapply(as.integer(default)-1, bins, mean)
ds <- tapply(as.integer(default[s])-1, bins[s], mean) # for students
do <- tapply(as.integer(default[!s])-1, bins[!s], mean) # for non-students
# max(ds,do, na.rm=T)
# plotting
par(mfrow=c(1,2))
plot(middle_points, d_all, type="l", col="green", lwd=2, ylim=c(0,0.15),
     ylab="Default rate", xlab="income")
lines(middle_points, ds, col="red", lwd=2)
lines(middle_points, do, col="blue", lwd=2)
# -> it looks like Students are more risky for the same levels of income
#   (be careful: highest income students are very few and the results might be unstable).
# -> for non-students there's no clear relationship:
#   it looks like risk is rising slowly with income but that fails on the highest levels on income.

abline(h=mean(as.integer(default)-1), col='green')
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

# counts per Student status, for income buckets/bins (these are histograms simply...)
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

# Income (again)
# Here:
#   the bins are quantiles
#   number of bins depends proportionally on the amount of avaialble data points
#   the reference points are medians of the bins
d <- default=='Yes'
s <- student=="Yes"
n <- nrow(Default)
g_all <- 20
g_s <- round(g_all*sum(s)/n,0)
g_ns <- g_all - g_s
cuts <- cut2(income, g=g_all)
#table(cuts)
#plot(cuts)
y <- tapply(d, cuts, mean)
x <- tapply(income, cuts, median)
par(mfrow=c(1,1))
plot(x, y, type="l", lwd=2, col="green", ylim=c(0,0.1))
cuts <- cut2(income[s], g=g_s)
y <- tapply(d[s], cuts, mean)
x <- tapply(income[s], cuts, median)
cuts <- cut2(income[!s], g=g_ns)
lines(x, y, lwd=2, col="red")
y <- tapply(d[!s], cuts, mean)
x <- tapply(income[!s], cuts, median)
lines(x, y, lwd=2, col="blue")



# Balance

# Balance & Students
summary(balance) # sic: thick left tail, cut at 0
# selecting breaks for the buckets
table(cut(balance,seq.int(0,2750, 250))) # checking
# breaks <- c(0,seq.int(250,2250,250)+125,2750)
# these breaks didn't reproduce the book result for the highest values
breaks <- c(0,seq.int(250,2000,250)+125,2750) # these breaks were OK
# obtaining the middle points of the buckets
middle_points <- (tail(breaks,-1) - head(breaks,-1))/2 + head(breaks,-1)
# the buckets/bins
bins <- cut(balance, breaks)
d <- default=="Yes"
s <- student=="Yes"
# default rates per balance bucket/bin
d_all <- tapply(as.integer(default)-1, bins, mean) # for all
ds <- tapply(as.integer(default[s])-1, bins[s], mean) # for students
do <- tapply(as.integer(default[!s])-1, bins[!s], mean) # for non-students
max(ds,do, na.rm=T) # set ylim for the plot
# plotting
# red = students, blue = others
par(mfrow=c(1,2))
plot(middle_points, d_all, type="l", col="green", lwd=2, ylim=c(0,1),
     ylab="Default rate", xlab="balance")
lines(middle_points, ds, col="red", lwd=2)
lines(middle_points, do, col="blue", lwd=2)
# -> strong relationship: balance+ -> default
#   but it's stronger for Other than Students
#   However one has to control for income
#   (maybe high income is the risk factor and students have a low income on average).
abline(h=mean(as.integer(default)-1), col='green', lty=2)
abline(h=tapply(as.integer(default)-1,student,mean),
       col=c("blue","red"), lty=2) # first "No" (blue), second "Yes" (red)
# -> but students have on average higher default rate.
require(plyr)
plot(balance~student,
     col=mapvalues(as.integer(student),from=c(1,2),to=c("blue","red")))
# -> students have on average higher balance,
#   that may explain why their average default rate is higher.

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

# balance (again)
# Here:
#   the bins are quantiles
#   number of bins depends proportionally on the amount of avaialble data points
#   the reference points are medians of the bins
d <- default=='Yes'
s <- student=="Yes"
n <- nrow(Default)
g_all <- 20
g_s <- round(g_all*sum(s)/n,0)
g_ns <- g_all - g_s
cuts <- cut2(balance, g=g_all)
#table(cuts)
#plot(cuts)
y <- tapply(d, cuts, mean)
x <- tapply(balance, cuts, median)
par(mfrow=c(1,1))
plot(x, y, type="l", lwd=2, col="green", ylim=c(0,0.5))
cuts <- cut2(balance[s], g=g_s)
y <- tapply(d[s], cuts, mean)
x <- tapply(balance[s], cuts, median)
cuts <- cut2(balance[!s], g=g_ns)
lines(x, y, lwd=2, col="red")
y <- tapply(d[!s], cuts, mean)
x <- tapply(balance[!s], cuts, median)
lines(x, y, lwd=2, col="blue")

# Comment: using quantiles of bins isn't good
#   It means the more extreme ranges of values are not represented
#   as they are melted with more numerous less extreme ranges of values.
#   So it's a good plot only for moderate, most typical values
#   Also using the median (or mean) isn't good
#   as it makes composite data series not comparable with their subseries,
#   especially for low and high value ranges.
#   Merging two series toghether means that the most extreme bins
#   are strongly shifted to the extremes for the composite data series.
#   So:
#   One should avoid this approach and use fixed, equal-size bins
#   and control for too small samples within bins for any data series
#   (as small samples lead to high variability and not reliable results).

# Conclusions:
# it looks like the risk is higher for:
# high balance
# high balance non-student
# high income student
# Still we can't rule out some variables are not sifnificant.
# E.g. it is possible that high income is a bigger risk for students
# simply because they have higher balance. So possibly accounting just
# for income and balance can explain the variation in default.

# Balance & Income -> response

par(mfrow=c(1,1))
require(plyr)
# default ~ balance + income
# Scatterplot
par(mfrow=c(1,1))
plot(balance, income, cex=0.5,
     col=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c("blue","red")),
     pch=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c(1,3)))
# -> significant overlap.

# Balance & Income & Student -> response

# The same per student status
par(mfrow=c(1,2))
plot(balance[student=="Yes"], income[student=="Yes"],
     cex=0.5,
     col=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c("blue","red")),
     pch=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c(1,3)))
plot(balance[student=="No"], income[student=="No"],
     cex=0.5,
     col=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c("blue","red")),
     pch=mapvalues(as.integer(default),
                   from=c(1,2),
                   to=c(1,3)))
# -> it looks even worse, much overlap.
xyplot(income~balance|stu, groups=def, auto.key=list(space="top"))
# -> hmm in lattice it doesn't look that helpless
#   This is actually the best plot, allowing to break down all data as much as possible.
#   It's not obvious how non-linear the best (highest sensitivity?) boundary is.
#   Visual inspection: all 1500+ balance -> risky


# Prediction: balance > 1500 -> default
pred <- ifelse(balance > 1500, 'Yes', 'No')
table(pred, default)
mean(pred == default) # 0.9272 accuracy
mean(pred[d] == default[d]) # 0.7748, sensitivity
mean(pred[!d] == default[!d]) # 0.9325, specificity
# -> reference levels (visual inspection of the data).

# Prediction: balance > 1300 -> default
pred <- ifelse(balance > 1300, 'Yes', 'No')
table(pred, default)
mean(pred == default) # 0.8530 accuracy
mean(pred[d] == default[d]) # 0.9039, sensitivity
mean(pred[!d] == default[!d]) # 0.8512, specificity
# -> reference levels (visual inspection of the data).


# Reverse view: comparing response groups
# Do different response groups (default yes/no) show different
# characteristics in therms of the predictors?

# Groups and supgroups comparisons (Lattice)
# preparing for Lattice
def <- revalue(default, c('Yes'='Default','No'='Solvent'))
stu <- revalue(student, c('Yes' ='Student', 'No'='Other'))
histogram(~income|def*stu)
# -> student yes/no decides on the distribution
histogram(~balance|def*stu)
# -> here default yes/no makes real change:
#   defaulters have higher balance on average, whether student or not.
#   -> so high balance should be considered as a potential risk factor.
# -> Defaulting students seem to have a bit higher balance
#   than the defaulting non-students
#   (but Students have higher balance than non-student also among the solvent individuals).
#   -> Maybe students are more robust to higher balances?
#   But one should control for income to verify this.

# Comparing groups by default Yes/No:
# (1) income, balance
# Boxplots (basic)
par(mfrow=c(1,2))
boxplot(balance~default, ylab="balance", col=c("blue","red"), main="Default")
boxplot(income~default, ylab="income", col=c("blue","red"), main='Default')
# Boxplots (ggplot2)
require(ggplot2)
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
# -> great difference in balance (Default-Yes -> very high)
#   not much difference in income.
# (2) student
histogram(~stu|def)
# -> Among the defaulters there're relatively more students.



# Models


# 10-fold cross-validation
K <- 10
# maximizing: accuracy (minimizing the error rate)

# Logistic regression

glm.fit <- glm(default~., data=Default, family=binomial)
summary(glm.fit)
# -> student- balance+, AIC 1580
#   Looks like income is not significant
#   but as I noted above, one can use income as an instrument for student status.
#   We should then compare two models: balance+income, balance+student
# Model assessment
require(boot)
err <- cv.glm(Default, glm.fit, K=K)$delta[1] # 0.0214 error rate
1 - err # 0.9786, accuracy (out of sample, estimated)
glm.probs <- glm.fit$fitted.values
summary(glm.probs)
# summary(predict(glm.fit, type='response')) # the same
pred <- rep('No', n)
pred[glm.probs>.5] <- 'Yes'
table(pred,default)
mean(pred == default) # 0.9732, accuracy (a bit better than 'No')
mean(pred[d] == default[d]) # 0.3153, sensitivity
# increase the sensitivity to around 77%
pred <- rep('No', n)
pred[glm.probs>.08] <- 'Yes'
mean(pred == default) # 0.9244, accuracy (no better than balance>1500)
mean(pred[d] == default[d]) # 0.7658, sensitivity
mean(pred[!d] == default[!d]) # 0.9299, specificity
# increase the sensitivity to around 90%
# increase the sensitivity to around 77%
pred <- rep('No', n)
pred[glm.probs>.03] <- 'Yes'
mean(pred == default) # 0.8591, accuracy (no better than balance>1300)
mean(pred[d] == default[d]) # 0.9039, sensitivity
mean(pred[!d] == default[!d]) # 0.8575, specificity

# Now check the two logistic models: balance+income, balance+student

# Logistic model: balance+student
glm.fit <- glm(default~balance+student, data=Default, family=binomial)
summary(glm.fit)
# -> student- balance+, AIC 1578 (lowest)
require(boot)
err <- cv.glm(Default, glm.fit, K=K)$delta[1]
1-err # .9786 accuracy (a bit better than No)
glm.probs <- glm.fit$fitted.values
# 50% cutoff
pred <- rep('No', n)
pred[glm.probs>.5] <- 'Yes'
table(pred,default)
mean(pred == default) # 0.9733, accuracy (a bit better than 'No')
mean(pred[d] == default[d]) # 0.3153, sensitivity
mean(pred[!d] == default[!d]) # 0.9960, specificity
# aiming at 90% sensitivity
pred <- rep('No', n)
pred[glm.probs>.03] <- 'Yes'
mean(pred == default) # 0.8591, accuracy (no better than balance>1300)
mean(pred[d] == default[d]) # 0.9039, sensitivity
mean(pred[!d] == default[!d]) # 0.8575, specificity
# -> exactly the same as the larger model (with all variables).
# Plotting
xyplot(income~balance|stu, groups=pred, auto.key=list(space="top", columns=2))

# Logistic: balance+income
glm.fit <- glm(default~balance+income, data=Default, family=binomial)
summary(glm.fit)
# -> blance+, income+, AIC 1585 (highest)
require(boot)
err <- cv.glm(Default, glm.fit, K=K)$delta[1]
1-err # .9785 accuracy (a bit better than No)
glm.probs <- glm.fit$fitted.values
pred <- rep('No', n)
pred[glm.probs>.03] <- 'Yes'
mean(pred == default) # 0.8567, accuracy (no better than balance>1300)
mean(pred[d] == default[d]) # 0.9009, sensitivity
mean(pred[!d] == default[!d]) # 0.8551, specificity

# Conclusions:
# Best models seems to be balance+student
# but it performs not much better than a simple rule involving balance only

# Plotting Fit measures for different cutoff levels for the best logistic model
glm.fit <- glm(default~balance+student, data=Default, family=binomial)
tmpfSens <- function(x) fSens(x, y=default, m=glm.fit)
tmpfSpec <- function(x) fSpec(x, y=default, m=glm.fit)
tmpfAccu <- function(x) fAccu(x, y=default, m=glm.fit)
plot(Vectorize(tmpfSens), 0, 1, col="red", lwd=2, ylab="Fit", xlab="Cutoff")
plot(Vectorize(tmpfSpec), 0, 1, col="green", lwd=2, add=T)
plot(Vectorize(tmpfAccu), 0, 1, col="blue", lwd=2, add=T)
identify(x=seq(0,0.1, by=0.01)) # doesn't work
# adding reference levels
pred <- ifelse(balance > 1500, 'Yes', 'No')
abline(h=c(mean(pred == default), # 0.9272 accuracy
           mean(pred[d] == default[d]), # 0.7748, sensitivity
           mean(pred[!d] == default[!d])), # 0.9325, specificity
       col=c('blue','red','green'),
       lty=2)
pred <- ifelse(balance > 1300, 'Yes', 'No')
abline(h=c(mean(pred == default), # 0.9272 accuracy
           mean(pred[d] == default[d])), # 0.7748, sensitivity
           #mean(pred[!d] == default[!d])), # 0.9325, specificity
       col=c('blue','red'),#,'green'),
       lty=3)

# Additional models: logistic regression, non-linear
glm.fit <- glm(default~balance*student, data=Default, family=binomial)
summary(glm.fit)
# -> balance+, AIC 1580
#   student, student:balance - not significant.
#   This is not a good model.
glm.fit <- glm(default~balance*student+income*student, data=Default, family=binomial)
summary(glm.fit)
# -> balance+, AIC 1580
#   all other - not significant.
#   This is not a good model.



# LDA


# LDA - just for practice - it shouldn't help much
lda.fit <- lda(default~., data=Default)
lda.fit
# -> student- balance+ income+
lda.pred <- predict(lda.fit, Default)
head(as.data.frame(lda.pred))
pred <- lda.pred$class
table(pred, default)
mean(pred == default) # 0.9724, accuracy
mean(pred[d] == default[d]) # 0.2372, sensitivity
# -> worse than the logistic models.
# Changing the cutoff level:
pred <- rep('No', n)
pred[lda.pred$posterior[,'Yes']>.04] <- 'Yes'
mean(pred == default) # 0.8620, accuracy (no better than balance>1300)
mean(pred[d] == default[d]) # 0.9009, sensitivity
mean(pred[!d] == default[!d]) # 0.8607, specificity
# -> it seems to be slightly best than the best logistic model.
#   To be sure one should CV this result (as this maybe due to overfitting).
# Plotting
xyplot(income~balance|stu, groups=pred, auto.key=list(space="top", columns=2))

# QDA

require(MASS)
qda.fit <- qda(default~., data=Default)
qda.fit
qda.pred <- predict(qda.fit, Default)
head(as.data.frame(qda.pred))
pred <- qda.pred$class
table(pred, default)
mean(pred == default) # 0.9730, accuracy
mean(pred[d] == default[d]) # 0.2823, sensitivity
# -> worse than the logistic models, but better than LDA.
# Changing the cutoff level:
pred <- rep('No', n)
pred[qda.pred$posterior[,'Yes']>.035] <- 'Yes'
mean(pred == default) # 0.8581, accuracy (no better than balance>1300)
mean(pred[d] == default[d]) # 0.9039, sensitivity
mean(pred[!d] == default[!d]) # 0.8565, specificity
# -> like LDA, a bit worse maybe.
# Plotting
xyplot(income~balance|stu, groups=pred, auto.key=list(space="top", columns=2))

# MDA








# KNN


# The KNN function classifies data points by calculating the Euclidean distance
# between the points. That's a mathematical calculation requiring numbers.
# All variables in KNN must therefore be coerce-able to numerics.
#
# The data preparation for KNN often involves three tasks:
# (1) Fix all NA or "" values
# (2) Convert all factors into a set of booleans,
#     one for each level in the factor
# (3) Normalize the values of each variable to the range 0:1 so that no variable's
#     range has an unduly large impact on the distance measurement.



library(class)
n <- nrow(Default)
test1 <- data.frame(income = 1000, balance = 1000, student = 'Yes')
test2 <- Default[1,]
str(Default)
str(test2)
# knn: balance+income, k=3
keeps <- c('balance','income')
pred <- knn(Default[,keeps], Default[,keeps], default, k=3)
table(pred,default)
mean(pred == default) # 0.9783, accuracy
mean(pred[d] == default[d]) # 0.4535, sensitivity
mean(pred[!d] == default[!d]) # 0.9964, specificity
# -> this is markedly better than the best Logistic model.


# KNN: balance+income, k=3
# + standardizing all variables
newStudent <- as.numeric(student)
NewDefault <- data.frame(
    income = (income - mean(income)) / sd(income),
    balance = (balance - mean(balance)) / sd(balance),
    student = (newStudent - mean(newStudent)) / sd(newStudent))
summary(NewDefault)
sapply(NewDefault, sd)
# NewDefault <- standardize(Default[-1]) # the same
pred <- knn(NewDefault, NewDefault, default, k=3)
table(pred, default)
mean(pred == default) # 0.9777, accuracy
mean(pred[d] == default[d]) # 0.5075, sensitivity
mean(pred[!d] == default[!d]) # 0.9939, specificity
# -> highest sensitivity so far
# Plotting
require(lattice)
xyplot(income~balance|stu, groups=pred, auto.key=list(space="top", columns=2))
# -> possible case of overfitting.
# Compare with actual data:
xyplot(income~balance|stu, groups=def, auto.key=list(space="top", columns=2))
# -> not much can be done. All previous methods draw the boundary
#   around balance = 2,000 (for maximum accuracy). Asking for higher
#   sensitivity (with lower accuracy) means moving this boundary
#   to the left (around balance = 1,500).
#   KNN-3 leads to overfitting. There seems to be no reason
#   for the pattern produced by KNN. CV should confirm that.


# KNN: balance+income, k=5
# + standardizing all variables
newStudent <- as.numeric(student)
NewDefault <- data.frame(
    income = (income - mean(income)) / sd(income),
    balance = (balance - mean(balance)) / sd(balance),
    student = (newStudent - mean(newStudent)) / sd(newStudent))
summary(NewDefault)
sapply(NewDefault, sd)
# alternative:
StdDefault <- standardize(Default[-1])
summary(StdDefault)
require(class)
pred <- knn(NewDefault, NewDefault, default, k=5)
table(pred, default)
mean(pred == default) # 0.9748, accuracy
mean(pred[d] == default[d]) # 0.4024, sensitivity
mean(pred[!d] == default[!d]) # 0.9945, specificity
require(lattice)
xyplot(income~balance|stu, groups=pred, auto.key=list(space="top", columns=2))


# plotting the classes
StdDefault <- standardize2(Default[-1])
Background <- (expand.grid(
    income = seq(0,80000,1000),
    balance = seq(0,2500,50),
    student = c("non-student","student"))
)
nrow(Background)
# StdBackground <- standardize2(Background)
# wrong! you should use mean and sd from the Default set
# otherwise you treat relative positions of data points within each data set as equal,
# instead of treating data points of the same values as equal.
StdBackground <- standardize2(Background, Default[-1])
# OK, it seems I fixed it, but still I get strange results
summary(StdBackground)
PredBackground <- knn(StdDefault, StdBackground, default, k=3)
summary(PredBackground)
with(Background,
     xyplot(income~balance|student, groups=PredBackground,
            pch=19,
            auto.key=list(space="top", columns=2))
     )



# Introduce carret:
# proper CV to select the model (including k for KNN)
# proper final estimation of the out of sample error



# Checking the nature of the data
# some visual investigation of the data
require(lattice)
xyplot(income~balance|def*stu)
par(mfrow=c(2,2))
hist(income[s&d])
hist(income[s&!d])
hist(income[!s&d])
hist(income[!s&!d])
hist(balance[s&d])
hist(balance[s&!d])
hist(balance[!s&d])
hist(balance[!s&!d])
par(mfrow=c(1,2))
hist(balance[s])
hist(balance[!s])

# Checking for normality:
# - income in every subgroup (student * default)
#   it looks like these are just random normals.
# - balance in student yes/no subgroup
#   it looks like these are normal distributions but cut and aggregated close to 0











