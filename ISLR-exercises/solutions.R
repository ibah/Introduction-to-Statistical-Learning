################################################################################

# 2 Statistical Learning

# p.52, Conceptual
# ex.1
# (a) flexible: large n, small p
# (b) inflexible model: better fit unless the assumption about the shape of f is very wrong
                    # more flexible model will overfit (account for the variability in the data)
# (c) use more flexible method to account for the high non-linearity
        # compare with point (b): b was valid (less flexible method is better)
        # unless the assumption about the functional form is very wrong.
        # In such a case the (c) point is relevan.







# p.54, Applied
# ex.8
# or:
library(ISLR)
data(College)
?College
fix(College)
  # a
college <- read.csv("files/College.csv")
  # b
dim(college) # 777 x 19
head(college,2) # see column X
str(college) # X is a name of a Uni
sum(is.na(college)) # no NA's
fix(college)
rownames(college) <- college[,1] # as a vector
fix(college)
college <- college[,-1]
fix(college)
  # c1
summary(college)
names(college)
names(college)[1:10]
pairs(college[,1:10])
# see some variables have strong linear relationship/correlation
attach(college)
plot(Outstate~Private)
#par(mfrow=c(4,4), mar=c(2, 2, 1, 0))
#for (i in 2:17)
#    boxplot(College[, i] ~ College[, 1], xlab="", main=colnames(College)[i])
#dev.off()
    # c4: create Elite variable
summary(Top10perc)
boxplot(Top10perc, xlab='Top10perc')
Elite <- rep("No", nrow(college))
Elite[college$Top10perc>50] <- "Yes" # "Yes" == ">50%"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
library(Hmisc)
Elite2 <- cut2(Top10perc,50) # "Yes" == ">=50%" (note the difference)
levels(Elite2) <- c("No", "Yes")
Elite3 <- cut(Top10perc,c(0,50,100)) # "Yes" == ">50%"
levels(Elite3) <- c("No", "Yes")
summary(Elite)
summary(Elite2) # different result as "Yes" == ">=50%"
summary(Elite3)
    # c5: histograms
str(college)
for(i in seq_along(college))
    if (class(college[,i])=='numeric') {
        #str(x)
        hist(college[i])
        cat ("Press [enter] to continue")
        line <- readline()
    }
# -> intersting, many different distributions
#   some very exponential (log them)
#   a few quite normal looking
#   some skewed left or right
    # c6: explore
# apply many simple exploring methods










summary(college)
pairs(college)
pairs(college[,1:10])
  # c3 Use the plot() function to produce side-by-side boxplots of Outstate versus Private
attach(college)
plot(Private, Outstate, xlab="Private", ylab="Outstate")
# or:
par(mfrow=c(4,4)) # to wide margins
par(mfrow=c(4,4), mar=c(2, 2, 1, 0)) # a bit to narrow top margin
par(mfrow=c(4,4), mar=c(2, 2, 2, 1)) # adjusting top and right margins
for (i in 2:17)
    boxplot(College[, i] ~ College[, 1], xlab="", main=colnames(College)[i])
# or:
dev.off()
par(mfrow=c(1,2))
i=2
plot(College[,i] ~ College$Private)
plot(College[,i] ~ College$Outstate)
dev.off()
par(mfrow=c(4,4), mar=c(2, 2, 1, 0))
c1 = subset(College, select = -c(Private, Outstate))
for (i in 1:ncol(c1)) {
    plot(c1[,i] ~ College$Private)
    plot(c1[,i] ~ College$Outstate)
}
# see two pages of plots (32 plots)
  # c4
Elite=rep("No",nrow(college ))
Elite[college$Top10perc >50]= "Yes"
Elite=factor(Elite, levels=c("No", "Yes")) # here "factor" to def. levels so to have "No" first
college=data.frame(college ,Elite)
summary(Elite)
plot(Elite, Outstate, xlab="Elite", ylab="Outstate")
# or:
dev.off()
College$Elite <- College$Top10perc > 50
summary(College[, c("Top10perc", "Elite")])
boxplot(Outstate ~ Elite, data=College)
  # c5
str(college)
par(mfrow = c(2,2))
hist(Apps)
hist(Accept)
hist(Enroll)
hist(Top10perc)
hist(Top25perc)
hist(F.Undergrad)
hist(P.Undergrad)
hist(Outstate)
hist(Room.Board)
hist(Books)
hist(Personal)
hist(PhD)
hist(Terminal)
hist(S.F.Ratio)
hist(perc.alumni)
hist(Expend)
hist(Grad.Rate)
dev.off()
    # c6
# Heatmap:
# Note that for this plot, we:
# 1. standardize each variable to mean 0 and standard deviation 1 using scale(),
# 2. convert the data.frame to a matrix as required by heatmap functions,
# 3. transpose the matrix to show the variables as rows rather than columns, just for convenient viewing,
# 4. use the pheatmap library, just because it by default produces a prettier heatmap than the built-in heatmap,
# 5. Annotate the columns by whether the university is private or not.
library(pheatmap)
pheatmap(t(as.matrix(scale(College[, 2:18]))),
         annotation=College[1],
         show_colnames=FALSE)

# p. 56 ex. 9
Auto = read.table("files/Auto.data", stringsAsFactors = F)
Auto = read.table("files/Auto.data", stringsAsFactors = F, header = T)
head(Auto)
head(Auto)
tail(Auto)
str(Auto)
summary(Auto)
sum(!complete.cases(Auto))
summary(complete.cases(Auto))
fix(Auto)
sapply(Auto,function(x) which(x=="?"))
# so missing values are only for horsepower, rows 34 128 332 338 356
Auto = read.table("files/Auto.data", na.strings = "?", header=T)
sum(!complete.cases(Auto)) # 5 missing
Auto = na.omit(Auto)
    # (a)
str(Auto)
summary(Auto)
# quantitative: mpg, cylinders, disp., horsepower, weight, acceleration, year
# qualitative: name, origin
    # (b)
sapply(subset(Auto, select=-c(name,origin)), range)
# the same longer:
sapply(Auto[-which(names(Auto)%in%c("name","origin"))], range)
    # (c)
x = sapply(subset(Auto, select=-c(name,origin)), function(x) c(range(x), mean(x),sd(x)))
dimnames(x)[[1]] = c("range.min","range.max","mean", "std.dev")
x
    # example - setting matrix cols and rows names
    set.seed(1)
    rmatrix  <-  matrix(sample(0:100, 16), ncol=4)
    dimnames(rmatrix) <- list(rownames(rmatrix, do.NULL = FALSE, prefix = "row"),
                          colnames(rmatrix, do.NULL = FALSE, prefix = "col"))
    # (d)
nrow(Auto)
AutoSub = Auto[-10:-85,]
x = sapply(subset(AutoSub, select=-c(name,origin)), function(x) c(range(x), mean(x),sd(x)))
dimnames(x)[[1]] = c("range.min","range.max","mean", "std.dev")
x
    # (e)
str(Auto)
AutoQuant = subset(Auto, select=-c(name,origin))
AutoQual = Auto[c("name", "origin")]
par(mfrow=c(3,3))
invisible(lapply(1:ncol(AutoQuant), function(i) hist(
    AutoQuant[,i],
    main=paste("Histogram of", colnames(AutoQuant)[i]),
    xlab=colnames(AutoQuant)[i])))
hist(Auto$origin)
pairs(subset(Auto, select=-c(name,origin)))
print("Positive corr. for displacement, horsepower, weight and cylinders,
      negative between these and mpg, weak negative with acceleration.
      No clear patter for years, except positive (years, mpg) and (years, acceleration")
heatmap(t(scale(as.matrix(subset(Auto, select=-c(name,origin))))))
pheatmap(t(scale(as.matrix(subset(Auto, select=-c(name,origin))))),
         show_colnames=FALSE)
    # (f)
# predicting mpg
# Plots  suggest negative rel. with disp., horse., weight,
# weak neg. with cylin., weak posit. with accel. and year.

    # alternative:
    # a
# turn origin into a factor
summary(Auto$origin)
unique(Auto$origin)
head(unique(Auto$name[Auto$origin==1]), 10)
head(unique(Auto$name[Auto$origin==2]), 10)
head(unique(Auto$name[Auto$origin==3]), 10)
Auto$origin <- factor(Auto$origin, levels=1:3, labels=c("U.S.", "Europe", "Japan"))
sapply(Auto, class)
# select quantiative vars:
quant <- sapply(Auto, is.numeric)
quant
    # b
sapply(Auto[, quant], range)
    # c
sapply(Auto[, quant], function(x) signif(c(mean(x), sd(x)), 2)) # rounding to signific. digits
    # d
output <- sapply(Auto[-10:-85, quant], function(x) round(c(range(x), mean(x), sd(x)), 2))
rownames(output) <- c("min", "max", "mean", "sd")
output
    # e
library(pheatmap)
pheatmap(t(scale(as.matrix(Auto[, quant]))), 
         annotation=Auto["origin"],
         show_colnames=FALSE)

# p. 56 ex. 10
    # (a)
library(MASS)
?Boston
dim(Boston)
summary(Boston)
# 506 rows, towns
# 14 columns: crime rate, resident. land zone, indust. acres, Charles River, nox, ...
    # (b)
pairs(Boston)
# interesting findings
pheatmap(t(scale(as.matrix(Boston))), 
         show_colnames=FALSE)
# Notice:
# 1) “chas” is a binary variable
# 2) “crim” has outliers
# 3) There are some collinear variables, like rad/tax
# 4) rad/tax have a lot of constant values:
summary(Boston$rad)
table(Boston$rad) # see 24 is standing out
# It’s those 24’s that stand out in the heatmap
# I’ll bet these are some kind of weird coding and not real values of 24.
# Let’s set those to NA:
Boston$rad[Boston$rad==24] <- NA
# tax has a lot of “666” values that I don’t believe are really 666
table(Boston$tax)
Boston$tax[Boston$tax==666] <- NA
# ptratio
summary(Boston$ptratio)
table(Boston$ptratio) # 140x 20.2, normal?
    # (c)
pairs(Boston)
# crime rate
# +: age, lstat + ptratio, rad, tax, nox, indus
# -: dis, medv
pheatmap(cor(Boston, use="pairwise.complete.obs"))
    # (d)
x = sapply(subset(Boston, select=c(crim,tax,ptratio)), function(x) c(range(x), mean(x),sd(x)))
dimnames(x)[[1]] = c("range.min","range.max","mean", "std.dev")
x
# crime
head(Boston[order(Boston$crim, decreasing=T),])
# range 0-89; highest for 381, 419, 406 (>50)
# tax
head(Boston[order(Boston$tax, decreasing=T),])
# range 187-711, highest for 489-493 (711, >670)
# ptratio
head(Boston[order(Boston$ptratio, decreasing=T),],10)
# range 12-22, change slowly 355-356 (22), then many with (21.2)
# alternative:
# Make histograms of each.
# breaks=“FD” tends to result in more bins in the histogram than the default:
par(mfrow=c(2,2))
hist(Boston$crim, main="Crime Rates\n (note the long tail)",breaks="FD")
hist(Boston$crim, main="Crime Rates with y-axis limited", 
     ylim=c(0, 40), breaks="FD")
hist(Boston$tax, main="Tax rates\n (note some high-tax outliers)", breaks="FD")
hist(Boston$ptratio, main="Pupil-teacher ratio\n (no real outliers)", breaks="FD")
    # (e)
sum(Boston$chas) # 35
summary(Boston$chas==1)
    # (f)
median(Boston$ptratio)
    # (g)
which(Boston$medv==min(Boston$medv)) # 399 406
which.min(Boston$medv) # wrong, as it returns only one value
x = sapply(Boston, function(x) c(range(x), mean(x),sd(x)))
dimnames(x)[[1]] = c("range.min","range.max","mean", "std.dev")
round(x,2)
signif(x,2)
Boston[399,] # see age, rad, nox, balck, lstat (tax?) (rather extreme values)
Boston[406,] # similar
# alternativelly:
par(mfrow=c(5,3), mar=c(2, 2, 1, 0))
for (i in 1:ncol(Boston)){
    hist(Boston[, i], main=colnames(Boston)[i], breaks="FD")
    abline(v=Boston[399, i], col="red", lw=3)
}
    # (h)
n = nrow(Boston)
k = sum(Boston$rm>7)
k
summary(Boston$rm > 7)
library(scales)
percent(k/n)
k = sum(Boston$rm>8)
k
percent(k/n)
summary(Boston$rm > 8)
idx <- Boston$rm > 8
summary(idx)
par(mfrow=c(5,3), mar=c(2, 2, 1, 0))
for (i in 1:ncol(Boston)){
    hist(Boston[, i], main=colnames(Boston)[i], breaks="FD")
    abline(v=Boston[idx, i], col="red", lw=1)
}
# low lstat, crim, dis
# high black, rather high age

################################################################################

    # 3 LINEAR REGRESSION

# p.120, Conceptual

# WAIT





# p.121, Applied

# WAIT




# ex.8
str(Auto)
    # (a)
f <- mpg~horsepower
attach(Auto)
lm.fit <- lm(f)
summary(lm.fit)
summary.lm(lm.fit) # the same output
    # horse+10 -> mpg-1.58, highly signif.
    # R^2 0.6
    # strong rel., t-val -24; negative rel.
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(98), interval='confidence') # as.data.frame works as well
predict(lm.fit, data.frame(horsepower=c(98)), interval='confidence') # more elegant
predict(lm.fit, data.frame(98), interval='prediction')
    # (b) regression plot
plot(horsepower,mpg)
abline(lm.fit)
identify (horsepower ,mpg ,name) # learn how to use it
    # (c) diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)
    # 1. nonlinear, add x^2
    # 2. right skewed distrib.
    # 3. heteroscedasticity (extreme mpg -> higher variance)
    # 4. several high leverage observations e.g. 117, 94
# experiment: removing high leverage observations
d2 <- Auto[-c(117,94),]
lm.fit2 <- lm(f,data=d2)
summary(lm.fit2)
par(mfrow=c(1,1))
plot(horsepower,mpg)
abline(lm.fit)
abline(lm.fit2, col='red')
# experiment: quadratic var
f3 <- mpg~poly(horsepower,2)
lm.fit3 <- lm(f3) # error, NA's not acceptable in poly()
d3 <- Auto[complete.cases(Auto),]
lm.fit3 <- lm(f3, data=d3)
summary(lm.fit3) # great improvment, R^2=0.7, all significant
par(mfrow=c(2,2))
plot(lm.fit3) # very nice, slight heterosc., all high leverage have very small residuals
# checking higher powers
summary(lm(mpg~poly(horsepower,3),data=d3)) # ^3 is irrelevant
summary(lm(mpg~poly(horsepower,7),data=d3)) # 5 **, 6 * but might this be just overfitting (too high model variance)?

# p. 122, ex. 9
    # cor() - correlation matrix
str(Auto)
    # (a)
plot(Auto)
    # (b)
cor(Auto["name"]) # error, name var. is qualit.
head(Auto[names(Auto)])
    # 4 (or 6) different ways to drop a column
# caution: "<- NULL" deletes a column in the source data frame
head(Auto[-which(names(Auto) %in% c('name'))])
head(Auto[!(names(Auto) %in% c('name'))])
head(Auto[-which(names(Auto)=='name')])
head(Auto[!(names(Auto)=='name')])
head(subset(Auto, select=-name))
head(Auto[names(Auto)!='name'])
cor(Auto[names(Auto)!='name'])
    # (c)
lm.fit <- lm(mpg~.-name, data=Auto)
summary(lm.fit)
# there's rel. (see F-stat)
# sig. pred.: (intercept), weight, year, origin, displacement
# e.g. year, the older => the higher mileage Miles/(US) gallon
# R^2 = 82%
    # (d)
par(mfrow=c(2,2))
plot(lm.fit)
    # 1. some non-lin., maybe try quadratic terms, some outliers
    # 2. light/thin upper tail
    # 3. variance more concentrated for smaller ^y's
    # 4. one observation with very high leverage
lm.check <- lm(mpg~.-name, data=Auto[-14,]) # dropping the high-lever. observ.
summary(lm.check) # the estimates changed noticeably

################################################################################


    # 4 CLASSIFICATION

# p.168, Conceptual
# ex.1
# odds for P(X) <-> logistic function for P(X)
# done in memory
# ex.2
# Prove the discriminant function
# 4.12, 4.13 (p.139-p.140)
# I gave up
# ex.3





# p.171, Applied

    # ex.10
# compare: Ch.4, LAB
library(ISLR)
?Weekly
dim(Weekly) # 1089, 9
str(Weekly)
sum(is.na(Weekly))
summary(Weekly)
head(Weekly)
    # a: explore
pairs(Weekly) # volume increases with year
attach(Weekly)
# Volume & time
plot(Year, Volume)
# plot(rownames(Weekly),Volume)
days <- seq_along(Volume)
plot(days,Volume)
m1 <- lm(Volume~days)
summary(m1) # R=.71
abline(m1, lwd=2, col="blue")
m2 <- lm(Volume~exp(days)) # error, inf values
m2 <- lm(log(Volume)~days)
summary(m2) # R=.97
volume_hat <- exp(m2$fitted.values)
lines(days, volume_hat, lwd=2, col="red")
m3 <- lm(Volume~poly(days,2))
summary(m3) # R=.85
lines(days, m3$fitted.values, lwd=2, col="green")
m4 <- lm(Volume~poly(days,7))
summary(m4) # R=.93
lines(days, m4$fitted.values, lwd=2, col="orange")
# more
# Error - use cor(), not cov()
# cov(Weekly[-9]) # here some stronger correlations than in case of Smarket
# # Relation with Today:
# # Lags: 1- 2+ 3-
# # lags 4 and 5 not correlated
# # weak Volume-
cor(Weekly[-9])
# no correlations except Year and Volume
    # b: logistic model
names(Weekly)
glm.fit <- glm(Direction~., data=subset(Weekly, select=Lag1:Volume), family = binomial)
summary(glm.fit)
# only Lag2 appears to be somewhat significant (*, 3%). Intercept is the most important term.
    # c: confusion matrix + interpret
glm.probs <- predict(glm.fit, type='response') # x = training data, probabilities
# glm.fit$fitted.values is the same
n <- nrow(Weekly)
pred <- rep("Down", n)
pred[glm.probs>.5] <- "Up"
pred <- as.factor(pred)
table(pred, Direction) # see Down is ok, while Up 601 missed out of 605
# -> very low specificity; mostly errors of 'type-2' meaning many positives (Up) missed
# Accuracy (overall rate of correct predictions)
mean(pred==Direction) # accuracy = 44.5%, it's very low, see:
# pred = Down
mean("Down"==Direction) # 44.4%, reference level
# pred = Up
mean("Up"==Direction) # 55.6%, reference level
# pred = random guess
guess <- sample(c("Down","Up"), n, replace = T)
mean(guess==Direction) # 49.9%, reference level
    # d: validation set logistic regression











################################################################################


    # 5 RESAMPLING METHODS

# p.197, Conceptual
# ex.1
# derive alpha that minimizes Var(X+Y)
#   1. split the Var(...) by '+'
#   2. FOC: derivative w.r.t. alpha
#   3. set =0, find alpha
#   4. the Var is convex with alpha so this is a minimum.



# p.198, Applied
# ex.5









################################################################################


    # 6 Linear Model Selection and Regularization

# p.259, Conceptual

# p.262, Applied


################################################################################


    # 7 Moving Beyond Linearity

# p.297, Conceptual

# p.299, Applied

################################################################################


    # 8 Tree-Based Methods








