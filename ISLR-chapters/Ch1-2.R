# n - number of disctinct data points / observations in the sample
# p - number of variables
# x.i.j - the value of j-th variable of i-th observation
# i in 1..n
# j in 1..p
# X = n x p (matrix)
# x.i, x.j (vectors)
# y.i - output value for observation i
# lower case bold = a vector length n


    # 2 Statistical Learning

# p. 16/30
library(ISLR)
ls(package:ISLR) # no Advertising data, but it can be downloaded separately
Adv <- read.csv("files/Advertising.csv")
dim(Adv)
head(Adv)
str(Adv)
summary(Adv)
Adv$X <- NULL
str(Adv)
par(mfrow=c(1,3))
# for (x in Adv[-4]) { # list(Adv$TV, Adv$Radio, Adv$Newspaper)) {
#     plot(Sales~x, data=Adv, col=2)
#     abline(lm(Sales~x, data=Adv), col="blue", lwd=2)
# }
for (j in 1:3) { # much better (as the names are preserved)
    plot(Sales~., data=Adv[c(j,4)], col=2)
    abline(lm(Sales~., data=Adv[c(j,4)]), col="blue", lwd=2)
}
library(ggplot2)
ggplot(Adv, aes(y=Sales)) +
    geom_point(aes(x=TV), col='red')

# KNN
# p.38
library(ElemStatLearn)
require(class)
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod15 <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod15, "prob")
prob <- ifelse(mod15=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob15 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
            "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()










    # 2.3 Lab: Introduction to R
    # p.42
x<-1;y<-2
ls()
rm(x,y)
rm(list=ls())

# p.45
x=rnorm(50)
y=x+rnorm(50, mean=50, sd=.1)
cor(x,y)

set.seed(1303)
mean(x)
var(x)
sd(x)
sqrt(var(x))

plot(x,y)
# xlab, ylab, main

# p.46
dir.create("files")
pdf("files/Figure.pdf")
# jpeg
plot(x,y,col="green")
dev.off()

seq(1,10)
x=1:10
-pi:pi
x=seq(-pi,pi,length=50)

# contour - 3D topographical map
y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour (x,y,f,nlevels =45, add=T)
fa=(f-t(f))/2
contour (x,y,fa,nlevels =15)

# p.47
# image - 3D heatmap
# persp - 3D plot, theta, phi control the view angles
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)

# indexing data
A=matrix (1:16 ,4 ,4)
A[c(1,3) ,c(2,4)]
A[ ,1:2]
A[-c(1,3) ,-c(1,3,4)]
dim(A)

# p.48
# loading data
Auto=read.table("files/Auto.data ")
fix(Auto)
# data editor - close before performing other commands
Auto=read.table("files/Auto.data", header =T,na.strings ="?")
fix(Auto)
Auto=read.csv("files/Auto.csv", header =T,na.strings ="?")
fix(Auto)
dim(Auto)
sum(is.na(Auto))
Auto[!complete.cases(Auto),] # horsepower is missing for 5 cars
Auto[1:4 ,]
Auto=na.omit(Auto)
# na.omit - removes rows with with NA values
dim(Auto)
names(Auto)

# p.49, p.50
# Additional Graphical and Numerical Summaries
plot(Auto$cylinders,Auto$mpg)
attach(Auto)
# revers: detach(Auto)
# attach - makes the col names available as vars
plot(cylinders , mpg)
# change the var to a qualitative one, (you'll see boxplots)
cylinders=as.factor(cylinders)
plot(cylinders , mpg)
plot(cylinders , mpg , col ="red ")
plot(cylinders , mpg , col ="red", varwidth =T)
plot(cylinders , mpg , col ="red", varwidth =T,horizontal =T)
plot(cylinders , mpg , col ="red", varwidth =T, xlab=" cylinders ",ylab ="MPG ")
hist(mpg)
hist(mpg ,col =2)
hist(mpg ,col =2, breaks =15)
# pairs - scatterplot matrix of all var pairs
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration , Auto)
# indetify - interactive method for identifing the values
# args: x, y, var
# then click on a plot, value of the var will be printed
# to exit: right-click on the plot # here: click Finish to stop and print the values
plot(horsepower ,mpg)
identify (horsepower ,mpg ,name)

# p. 51
# summary
summary(Auto)
summary(mpg) # for a single variable

# Exiting
q() # you may save the workspace
# saving and relaoding the history (commands) (you can used it before exiting R)
savehistory()
loadhistory()




