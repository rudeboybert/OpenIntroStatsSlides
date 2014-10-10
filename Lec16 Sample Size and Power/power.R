x <- seq(10, 20, by=0.01)
y1 <- dnorm(x, 15, 0.25)
y2 <- dnorm(x, 15, 2)
pdf("./Lec16 Sample Size and Power/norm.pdf", width=8*0.8, height=6*0.8)
plot(x, y1, type='l', col="red", ylab="", xlab="x", lwd=2)
lines(x, y2, col="blue", lwd=2)
abline(v=15, lty=2)
legend("topleft",
       legend=c(expression(paste(sigma," = 0.25")),
                expression(paste(sigma," = 2"))),
       lty=c(1,1),
       col=c("red", "blue"), bty='n')
dev.off()
