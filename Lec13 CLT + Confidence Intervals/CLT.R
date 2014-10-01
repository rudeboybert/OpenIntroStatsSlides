# setwd("./Lec13 Confidence Intervals/figure")

#-------------------------------------------------------------------------------
# Bimodal distribution of dragon wing spans:
# http://www.nytimes.com/2013/09/24/science/as-normal-as-rabbits-weights-and-dragons-wings.html
#-------------------------------------------------------------------------------
# Set up bimodal distribution as weighted average of two normals
mu.1 <- 5
mu.2 <- 10
p.1 <- 0.7
p.2 <- 0.3
mu <- mu.1*p.1 + mu.2*p.2
x <- seq(2, 13, by=0.01)
y <- dnorm(x, mean=mu.1)*p.1 + dnorm(x, mean=mu.2)*p.2

# Sampling distribution values
n <- 15
x.bars <- 
  replicate(10000, 
            mean(
              rnorm(n, 
                    mean=sample(c(mu.1, mu.2), n, replace=TRUE, prob=c(p.1, p.2))
              )
            )
  )


pdf("CLT1.pdf", width=8, height=4)
par(mfrow=c(1, 2))
# Population Distribution
plot(x, y, type='l', xlab="x: wing span (in feet)", ylab="",
     main="Population Dist'n")
abline(v=mu, col="red", lwd=2)
dev.off()


pdf("CLT2.pdf", width=8, height=4)
par(mfrow=c(1, 2))
# Population Distribution
plot(x, y, type='l', xlab="x: wing span (in feet)", ylab="",
     main="Population Dist'n")
abline(v=mu, col="red", lwd=2)

# Sampling Distribution
hist(x.bars, xlim=range(x), 
     xlab=expression(paste(bar(x), ": avg wing span (in feet)")), 
     prob=TRUE, ylab="",
     main="Sampling Dist'n")
abline(v=mu, col="red", lwd=2)
dev.off()





#-------------------------------------------------------------------------------
# Show effect of large n counteracting skew
#-------------------------------------------------------------------------------
meanlog <- -5
sdlog <- 1
x.max <- 0.04
breaks <- seq(0, 5, by=0.0005)

# Population Distribution
pdf("true.pdf", width=8*0.8, height=6*0.8)
x <- seq(0, x.max, by=0.0001)
plot(x, dlnorm(x, meanlog = meanlog, sdlog=sdlog), xlab="x", ylab="", 
     xlim=c(0, x.max), type='l')
abline(v=exp(meanlog + sdlog^2/2), col="red", lwd=2)
dev.off()

for(n in c(2, 10, 30, 75)) {
  sim.means <- replicate(10000, mean(rlnorm(n, meanlog = meanlog, sdlog=sdlog)))
  pdf(paste("hist", n, ".pdf", sep=""), width=8*0.8, height=6*0.9)
  hist(sim.means, main="", xlab=expression(bar(x)), xlim=c(0, x.max), 
       breaks = breaks, ylim=c(0, 1300))
  abline(v=exp(meanlog + sdlog^2/2), col="red", lwd=2)
  dev.off()
}