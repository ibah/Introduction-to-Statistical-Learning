# PML - Prediction Study Design
# 
# 1. Define your error rate (here we use only generic error rate)
# 2. Split data into 2 (or 3):
#     Training, Testing, Validation (optional)
# 3. On the training set pick features
# Use crossvalidation
# 4. On the training set pick prediction function & estimate
# Use crossvalidation
# 5. If no validation:
#     pick your best model and apply 1x to test set (one time) (more times = training)
# 6. If validation set:
#     Apply to test set and refine => out of this pick your best model
# Apply 1x to validation (apply only the best model)
# Know prediction benchmarks e.g. if all 0, or mean(x) etc.

# Data Splits
# large data: 60% training, 20% test, 20% validation
# medium: 60% training, 40% testing
# small: do cross validation & report caveat ("small sample size"); or give up

# Features
# Things you should be looking for
# * Imbalance in outcomes/predictors
#   e.g. all x=x0 in one outcome group, all x=x1 in the other outcome group
#   but if count(x0) <<<< cound(x1) than be careful
# * Outliers 
# * Groups of points not explained by a predictor
# * Skewed variables 
#   e.g. you may want to transform them

# Preprocessing
# highly skewed -> standardize
# continouous but not normal -> box-cox
# outliers -> log, box-cox
# missing data -> method = c("knnImpute")
# removing zero covariate (or low variabilty)
# indicator variables (dummyVars) (???)
# correlated variables -> use PCA to extract the info & reduce noise
#       but: mostly for linear models; plot first; dela with outliers

# Covariate creation
# Raw
# -> Tidy (selected, transformed)
# -> New Covariates (more for reg., svms; less for class. trees)
#   1. Exploratory analysis
#   2. Creating and adding to the data frames (only to training)

# Cross-Validation
# Approach:
# 1. Use the training set
# 2. Split it into training/test sets
# 3. Build a model on the training set
# 4. Evaluate on the test set
# 5. Repeat and average the estimated errors -> kind of estimating the test set error
# 6. The original test set is left intact -> so it will be unbiased measurement of the out of sample accuracy
# Used for:
# 1. Picking variables to include in a model
# 2. Picking the type of prediction function to use
# 3. Picking the parameters in the prediction function
# 4. Comparing different predictors


# THE FIRST MODEL & FIT


library(ISLR)
data("Default")
attach(Default)
library(caret)
# note: to get the proper meaning of sensitivity and specificity one should relevel the response factor
# Sensitivity is calculated for the reference (base) level
levels(Default$default)
Default$default <- relevel(Default$default, 'Yes')
inTrain <- createDataPartition(Default$default, p=0.8, list=F)
training <- Default[inTrain,]
testing <- Default[-inTrain,]
dim(training)
fit <- train(default~., data=training, method='glm')
fit # .9719 accuracy
fit$finalModel
summary(fit$finalModel) # one should drop income, it just increases the model variance
pred <- predict(fit, newdata=testing)
confusionMatrix(pred, testing$default) # .9735 accuracy, .3485 accuracy


# DATA SPLITS


# k-fold
folds <- createFolds(Default$default, k=10, list=T, returnTrain=T) # return Train
summary(folds)
folds <- createFolds(Default$default, k=10, list=T, returnTrain=F) # return Test
summary(folds)
# resampling
folds <- createResample(Default$default, times=10, list=T) # this is with replacement!
summary(folds)


# TRAINING OPTIONS


# train options
args(train.default)
?train.default

# metric
# "RMSE" and "Rsquared" for regression
# "Accuracy" and "Kappa" for classification
# Custom: trainControl -> summaryFunction

# list of all fitting methods (class. or ref. models)
cat(names(getModelInfo()), sep=', ')
# preProcess
# "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", "range", "knnImpute",
# "bagImpute", "medianImpute", "pca", "ica" and "spatialSign"

# trainControl options
args(trainControl)
# resampling: method, number, repeats


# EXPLORING PREDICTORS


# predictors
summary(Default)

# featurePlot
# For classification: box, strip, density, pairs or ellipse
# For regression, pairs or scatter
featurePlot(training[-1], training$default, plot='pairs')
featurePlot(training[-1:-2], training$default, plot='box') # bad... same scale used -> scale first
featurePlot(training[-1:-2], training$default, plot='density') # same here
featurePlot(training[-1:-2], training$default, plot='strip') # same here
featurePlot(training[-1:-2], training$default, plot='ellipse') # nice!

# ggplot2
qplot(income,balance, data=training) # looks like a 2-var Norm. distr., cut at balance=0
qplot(income,balance, colour=default, data=training)
#qplot(income[default=='Yes'],balance[default=='Yes'], data=training)
#hist(balance[default=='Yes'],data=training)
#qplot(balance[default=='Yes'],data=training)
qq <- qplot(income,balance, colour=default, data=training)
qq + geom_smooth(method='lm', formula=y~x)
require(Hmisc)
cutIncome <- cut2(training$income,g=5)
p1 <- qplot(cutIncome, balance, data=training, fill=cutIncome, geom='boxplot')
p1
p2 <- qplot(cutIncome, balance, data=training, fill=cutIncome, geom=c('boxplot','jitter'))
p2
require(gridExtra)
grid.arrange(p1,p2,ncol=2)
qplot(balance, colour=student, data=training, geom='density')
qplot(income, colour=student, data=training, geom='density')

# tables
t1 <- table(cutIncome,training$student)
t1
prop.table(t1,1)


# PREPROCESSING


# preProcess()
preObj <- preProcess(training[-1]) # defaults: method=c('center','scale')
trainAdj <- predict(preObj, training[-1])
summary(trainAdj)

# train(proProcess=)
fit <- train(default~., data=training,
             preProcess=c('center','scale'), # here default is NULL
             method='glm')
fit # .9733 accuracy (so better)

# impute missing data
n <- nrow(training)
training$bal <- training$balance
selectNA <- rbinom(n,1,.05)==1 # 5% missing data
sum(selectNA)/n
training$bal[selectNA] <- NA
summary(training)

preObj <- preProcess(training[c(-1,-3)], method='knnImpute') # don't include the full true balance
balImp <- predict(preObj, training[c(-1,-3)])$bal # error!

# converting factor variables to indiator variables
dummies <-dummyVars(balance~student,data=training)
head(predict(dummies,newdata=training))

# removing zero covariates
nsv <- nearZeroVar(training,saveMetrics=TRUE)
summary(training)
nsv

# splines
# basis
library(splines)
bsBasis <- bs(training$income, df=3)
head(bsBasis)
# fitting curves
lm.bs <- lm(balance~bsBasis, data=training)
plot(training$income,training$balance)
points(training$income, predict(lm.bs, newdata=training), col='red')

# PCS (Principal Components Analysis)
cor(training[,c('income','balance')])
# -> see discussion in D_default
#   there's a weak negative correlation but only due to mixing two subgroups
#   with different avarage levels of income and balance (students and others).
#   Withing those two groups there's no correlation.

# Spam example
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
names(spam)[c(34,32)]  # plotting the two correlated columns
plot(spam[,34],spam[,32])
# plot: linear transformation of the predictors to caputre most of the variability in X
par(mfrow=c(1,2))
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)
# plot: PCA
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation
# PCA, 2
par(mfrow=c(1,2))
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")
prComp$rotation
# PCA, 2, using caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)
# model fit
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)
# model evaluation
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))
# alternative
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

# Default data set
# Stat: prcomp
prComp <- prcomp(training[,c('balance','income')])
prComp # see, here PCA is useless, as there's no correlation
# caret: preProcess
preProc <- preProcess(training[,c('balance','income')], method='pca')
trainPC <- predict(preProc, training)
# plotting
par(mfrow=c(1,3))
plot(training$income, training$balance, col=training$default)
plot(prComp$x[,1],prComp$x[,2],col=training$default, xlab='PC1', ylab='PC2')
plot(trainPC$PC1, trainPC$PC2, col=trainPC$default)
#   -> how to interpret this?
tapply(training$balance, training$student, mean)
tapply(prComp$x[,2], training$student, mean)
tapply(trainPC$PC1, trainPC$student, mean)
tapply(trainPC$PC2, trainPC$student, mean)
# caret: model fitting
# 1
m1.fit <- train(default~., method='glm', data=trainPC)
summary(m1.fit$finalModel)
testPC <- predict(preProc, testing)
confusionMatrix(predict(m1.fit,testPC),testPC$default) # accuracy 0.9735, sensitivity 0.3030
# 2
m2.fit <- train(default~., method='glm', preProcess='pca', data=training)
summary(m2.fit$finalModel)
confusionMatrix(predict(m2.fit, testing), testing$default) # accuracy 0.9735, sensitivity 0.3030


# TREES


# fast review of the data
require(lattice)
require(plyr)
?revalue
trainStu <- revalue(training$student, c("Yes"="Student", "No"="Non-Student"))
trainDef <- revalue(training$default, c("Yes"="Defaulter", "No"="Solvent"))
with(training, xyplot(income~balance|trainStu, groups=trainDef, auto.key=list(space="right")))
# sic: better use 'groups' than 'colour'.
# qplot(balance, income, colour=trainDef, data=training)
ggplot(aes(balance, income, colour=default), data=data.frame(training,trainStu)) +
    geom_point() + facet_grid(.~trainStu)
# model fitting
tree.fit <- train(default~., method='rpart', data=training)
tree.fit$finalModel
# plotting the tree
par(mfrow=c(1,1))
# 1
plot(tree.fit$finalModel, uniform=T, main='Default yes/no')
text(tree.fit$finalModel, use.n=T, all=T)
# 2
library(rattle)
fancyRpartPlot(tree.fit$finalModel, main='Default yes/no')
# model evalutation
confusionMatrix(predict(tree.fit, testing),testing$default) # accuracy .9725, sensitivity 0.2424


# BAGGING


# = Bootstrap aggregating
# similar bias, but lower variance
# better for non-linear functions
# bagged loess

# Ozone example
# This is regression example: response is a mean of bootstrapped predictions
library(ElemStatLearn)
?ozone
str(ozone)
summary(ozone)
cor(ozone)
plot(ozone)
ozone <- ozone[order(ozone$ozone),]
head(ozone)
n <- nrow(ozone)
# bootstrapping
n_samples <- 10
max_ozone <- 155
ll <- matrix(NA, nrow=n_samples,ncol=max_ozone)
for(i in 1:n_samples) {
    ss <- sample(1:n, replace = T)
    ozone0 <- ozone[ss,]
    ozone0 <- ozone0[order(ozone0$ozone),]
    loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
    ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:max_ozone))
}
# -> this bootstrapps ozone data set (10x resampling),
#   fits a loess each time (temparature ~ ozone),
#   generates loess fitted values for each fit for ozone in range 1:155
#   (so this is for plotting).
# Plotting
plot(ozone$ozone, ozone$temperature) # the data
for(i in 1:n_samples) lines(1:max_ozone, ll[i,], col="grey") # individual fits
lines(1:max_ozone, apply(ll,2,mean), col="red", lwd=2) # aggregated prediction

# Bagging in caret
# train: method = bagEarth, treebag, bagFDA
# bag: x, y, B, bagControl
library(caret)
predictors <- data.frame(ozone$ozone)
temperature <- ozone$temperature
tree.bag <- bag(predictors, temperature, B=n_samples,
                bagControl=bagControl(
                    fit=ctreeBag$fit,
                    predict=ctreeBag$pred,
                    aggregate=ctreeBag$aggregate
                ))
tree.bag
tree.bag$fits # list of fits (here: 10)
plot(ozone$ozone,temperature)
points(ozone$ozone, predict(tree.bag$fits[[1]]$fit, predictors), col='red')
points(ozone$ozone, predict(tree.bag$fits[[2]]$fit, predictors), col='orange')
points(ozone$ozone, predict(tree.bag$fits[[3]]$fit, predictors), col='yellow')
points(ozone$ozone, predict(tree.bag, predictors), col='blue') # aggregated predictions
# ctreeBag fuctions
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate

# Tree Bagging: Default data set
tree.bag <- bag(training[-1], training$default, B=10,
                bagControl=bagControl(
                    fit=ctreeBag$fit,
                    predict=ctreeBag$pred,
                    aggregate=ctreeBag$aggregate
                ))
tree.bag
summary(tree.bag$fits[[1]])
test <- data.frame(income=1000,
                   balance=1000,
                   student=factor('Yes', levels=c('No','Yes')))
predict(tree.bag,test)
pred <- predict(tree.bag,testing[-1])
confusionMatrix(predict(tree.bag,testing[-1]),testing$default)
# -> accuracy .9735, sensit .3788, actually results better than the best logistic model
#   another run: .9750 & .3030
#   but all this is useless if we can't increase the sensitivity.
# Plotting
ggplot(aes(balance, income, colour=default), data=data.frame(training,trainStu)) +
    geom_point() + facet_grid(.~trainStu)
confusionMatrix(predict(tree.bag,training[-1]), training$default) # accu. .9735, sens. .3333
ggplot(aes(balance, income, colour=predict(tree.bag,training[-1])), data=data.frame(training,trainStu)) +
    geom_point() + facet_grid(.~trainStu)

# How to set a contraint (minimum) for sensitivity?

# Tree bagging in Caret (using train function)
# 2
tree.bag2 <- train(default~., data=training, method='treebag') # how to select the number of resamples?
tree.bag2
tree.bag2$finalModel # default: 25 bootstrap
sapply(tree.bag2$finalModel$mtrees, names) # complicated...
pred <- predict(tree.bag2, testing)
confusionMatrix(pred, testing$default) # accu. .969, sens.2727
# 3
ctr1 <- trainControl(method = "cv", number = 3, returnResamp = "all", # 3-fold CV
                    classProbs = TRUE, 
                    summaryFunction = twoClassSummary)
ctr2 <- trainControl(number = 3, returnResamp = "all", # 3 reps bootstrapped
                    classProbs = TRUE, 
                    summaryFunction = twoClassSummary)
ctr2.1 <- trainControl(number = 3, returnResamp = "all", # 3 reps bootstrapped
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary,
                     metric='ROC')
ctr3 <- trainControl(number=3, classProbs=T)
tree.bag3 <- train(default~., data=training, method='treebag',
                   trControl=ctr3) # warnings: duplicate rows
tree.bag3
tree.bag3$finalModel # default: 25 bootstrap (why the same text indep. of the actuall bootstrap reps #?)
pred <- predict(tree.bag3, testing)
prob <- predict(tree.bag3, testing, type='prob')
confusionMatrix(pred, testing$default) # accu. .969, sens.2727
defaultSummary(data.frame(pred=pred, obs=testing$default)) # OK
# ROC 1
make.names(levels(pred))
tmp <- data.frame(pred=pred, obs=testing$default, prob)
head(tmp);tail(tmp)
twoClassSummary(data.frame(pred=pred, obs=testing$default, prob)) # classProbs needed to get ROC measures
extractProb(tree.bag3, testX=testing[-1], testY=testing$default)
extractProb(tree.bag3, unkX=testing[-1])
# ROC 2
library(pROC)
tree.bag.ROC <- roc(testing$default, prob$Yes)
plot(tree.bag.ROC)
plot(tree.bag.ROC, type='S', print.thres=.5)


# RANDOM FOREST


# Method:
# bootstrap samples -> bootstrap variables
# multiple trees -> votes
# pros: accuracy
# cons: speed, interpretability, overfitting

# Random Forest: iris data set
library(caret)
inTrain <- createDataPartition(iris$Species, p=.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
# model fitting
rf.fit <- train(Species~., data=training, method='rf', prox=T)
rf.fit
getTree(rf.fit$finalModel, k=2) # getting a single tree
# plotting class centers
irisP <- classCenter(training[,3:4], training$Species, rf.fit$finalModel$proximity)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
head(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)
# model evaluation
pred <- predict(rf.fit, testing)
confusionMatrix(pred, testing$Species)
# plotting
testing$predRight <- pred == testing$Species
qplot(Petal.Width, Petal.Length, col=predRight, data=testing, main='newdata Predictions')


# Random Forest: Default data set
rng <- 200:500
rf.fit <- train(default~., data=training[rng,], method='rf', prox=T,
                trontrol=trainControl(method='repeatedcv', number=2, repeats=2))
rf.fit
# plotting class centers
trainP <- classCenter(training[rng,c(-1,-2)], training[rng,]$default, rf.fit$finalModel$proximity)
trainP <- as.data.frame(trainP)
trainP$default <- rownames(trainP)
p <- qplot(balance, income, col=default, data=training[rng,])
p
p + geom_point(aes(x=balance, y=income, col=default), size=5, shape=4, data=trainP)
# model evaluation
pred <- predict(rf.fit, testing)
confusionMatrix(pred, testing$default)


# BOOSTING


# Idea: many weak predictors -> weighted sum -> one strong predictor
# can be done with any subset of classifiers
# e.g gradient boosting
# # R:
# * [gbm] - boosting with trees
# * [mboost] - model based boosting
# * [ada] - statistical boosting based on additive logistic regression
# * [gamBoost] - boosting generalized additive models

# Boosting with Trees: Default data set
rng <- 200:500
gbm.fit <- train(default~., data=training[rng,], method='gbm', verbose=F) # verbose=F, otherwise the screen gets flooded
                #trontrol=trainControl(method='repeatedcv', number=2, repeats=2))
gbm.fit
# model evaluation
pred <- predict(rf.fit, testing)
confusionMatrix(pred, testing$default)
qplot(balance, income, col=pred, data=testing)


# MODEL BASED PREDICTIONS


# Data are probabilistic -> Bayes model

# LDA, Naive Bayes
# model fitting
lda.fit <- train(default~., data=training, method='lda')
nb.fit <- train(default~., data=training, method='nb')
qda.fit <- train(default~., data=training, method='qda')
mda.fit <- train(default~., data=training, method='mda')
# model evaluation
lda.pred <- predict(lda.fit, testing)
nb.pred <- predict(nb.fit, testing)
qda.pred <- predict(qda.fit, testing)
mda.pred <- predict(mda.fit, testing)
confusionMatrix(lda.pred, testing$default) # low sensitivity
confusionMatrix(nb.pred, testing$default) # even lower sensitivity
confusionMatrix(qda.pred, testing$default) # low sensitivity
confusionMatrix(mda.pred, testing$default) # low sensitivity
# model comparison
table(lda.pred, nb.pred)
























