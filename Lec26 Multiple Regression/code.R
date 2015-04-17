n <- 100
x <- runif(n, min = 10, max = 20)
y <- 3*x + 5

width <- 8
height <- 6


pdf("./Lec26 Multiple Regression/figure/plot1.pdf", width = width, height = height)
plot(x, y)
model1 <- lm(y~x)
abline(model1, col="red", lwd=2)
text(18, 40, paste("Var(y) = ", round(var(y),3)))
dev.off()

pdf("./Lec26 Multiple Regression/figure/plot2.pdf", width = width, height = height)
e <- y-fitted(model1)
plot(x, e, ylim=c(-1,1), ylab=expression(e[i]))
abline(h=0, col="red", lwd=2)
title(expression(paste("Residuals: ",  y[i], " - ", hat(y)[i], " = ", e[i])))
text(18, -0.75, paste("Var(e) = ", round(var(e),3)))
dev.off()

pdf("./Lec26 Multiple Regression/figure/plot3.pdf", width = width, height = height)
set.seed(76)
y <- 3*x + 5 + rnorm(n, mean = 0, sd = 5)
plot(x, y)
model2 <- lm(y~x)
abline(model2, col="red", lwd=2)
text(18, 40, paste("Var(y) = ", round(var(y),3)))
dev.off()

pdf("./Lec26 Multiple Regression/figure/plot4.pdf", width = width, height = height)
e <- y-fitted(model2)
plot(x, e, ylab=expression(e[i]))
abline(h=0, col="red", lwd=2)
title(expression(paste("Residuals: ",  y[i], " - ", hat(y)[i], " = ", e[i])))
text(18, 10, paste("Var(e) = ", round(var(e),3)))
summary(model2)
dev.off()

1 - var(e)/var(y)
