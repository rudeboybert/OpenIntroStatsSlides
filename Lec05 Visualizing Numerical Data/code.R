setwd("./Lec05 Exam:Vis Numerical Data/figure/")
library(openintro)


pdf("hist.pdf", width=8, height=6)
hist(email50$num_char, breaks=seq(0, 65, by=5), xlab="Number of Characters (in thousands)", 
     main="Number of Characters in 50 Sampled Emails")
dev.off()


pdf("MLB.pdf", width=8, height=6)
hist(MLB$salary, breaks=seq(0, 35000, by=2500), xlab="Annual Salary (in thousands of $)", 
     main="Histograms of Major League Baseball Salaries")
dev.off()


pdf("MLB2.pdf", width=8, height=6)
hist(MLB$salary, breaks=seq(0, 35000, by=2500), xlab="Annual Salary (in thousands of $)", 
     main="Histogram of Major League Baseball Salaries")
legend("topright", legend=c("mean", "median"), col=c("red", "blue"), lty=c(1,1), 
       lwd=2, bty='n')
abline(v=mean(MLB$salary), col="red", lwd=2)
abline(v=median(MLB$salary), col="blue", lwd=2)
dev.off()


pdf("spread.pdf", width=8, height=4)
set.seed(76)
x <- rnorm(1000, mean=20, sd=1)
y <- rnorm(1000, mean=20, sd=3)

range <- range(c(x,y))
range <- c(floor(range[1]), ceiling(range[2]))
breaks <- seq(range[1], range[2], by=1)

par(mfrow=c(1,2))
hist(x, xlim=range, breaks=breaks, ylim=c(0,360), main="", xlab="value")
hist(y, xlim=range, breaks=breaks, ylim=c(0,360), main="", xlab="value")
dev.off()


pdf("spread2.pdf", width=8, height=4)
set.seed(76)
x <- rnorm(1000, mean=20, sd=1)
y <- rnorm(1000, mean=20, sd=3)

range <- range(c(x,y))
range <- c(floor(range[1]), ceiling(range[2]))
breaks <- seq(range[1], range[2], by=1)

par(mfrow=c(1,2))
hist(x, xlim=range, breaks=breaks, ylim=c(0,360), 
     main=sprintf("Sample SD = %4.3f", sd(x)), xlab="value")
hist(y, xlim=range, breaks=breaks, ylim=c(0,360), 
     main=sprintf("Sample SD = %4.3f", sd(y)), xlab="value")
dev.off()



