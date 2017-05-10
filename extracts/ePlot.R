library(ggplot2)
gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 10
cols = gg_color_hue(n)
dev.new(width = 4, height = 4)
plot(1:n, pch = 16, cex = 2, col = cols)

require(gridExtra)
plot1 <- qplot(1)
plot2 <- qplot(1)
grid.arrange(plot1, plot2, ncol=2)

# print logistic regression fitted line
data(mtcars)
dat <- subset(mtcars, select=c(mpg, am, vs))
logr_vm <- glm(vs ~ mpg, data=dat, family=binomial)
#logr_vm <- glm(vs ~ mpg, data=dat, family=binomial(link="logit"))
#summary(logr_vm)
library(ggplot2)
ggplot(dat, aes(x=mpg, y=vs)) + geom_point() + 
    stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
par(mar = c(4, 4, 1, 1)) # Reduce some of the margins so that the plot fits better
plot(dat$mpg, dat$vs)
curve(predict(logr_vm, data.frame(mpg=x), type="response"), add=TRUE) 



x <- factor(c("yes", "no"))
x
relevel(x, ref="yes")

x <- c(0,1)
library(plyr)
mapvalues(x, from=c(0,1), to=c(2,3))
x

# Basic: show %
dates <-  1:10
returns <- runif(10)
plot(dates, returns, yaxt="n")
axis(2, at=pretty(returns), lab=pretty(returns) * 100, las=TRUE)
# Basic: show % (2)
dates <-  1:100
returns <- runif(100)
yticks_val <- pretty_breaks(n=5)(returns)
plot(dates, returns, yaxt="n")
axis(2, at=yticks_val, lab=percent(yticks_val))

# Basic: Axes
# A Silly Axis Example
# specify the data 
x <- c(1:10); y <- x; z <- 10/x
# create extra margin room on the right for an axis 
par(mar=c(5, 4, 4, 8) + 0.1)
# plot x vs. y 
plot(x, y,type="b", pch=21, col="red", 
     yaxt="n", lty=3, xlab="", ylab="")
# add x vs. 1/x 
lines(x, z, type="b", pch=22, col="blue", lty=2)
# draw an axis on the left 
axis(2, at=x,labels=x, col.axis="red", las=2)
# draw an axis on the right, with smaller text and ticks 
axis(4, at=z,labels=round(z,digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# add a title for the right axis 
mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")
# add a main title and bottom and left axis labels 
title("An Example of Creative Axes", xlab="X values",
      ylab="Y=X")
