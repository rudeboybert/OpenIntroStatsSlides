setwd("~/Documents/Teaching/MATH141/Lectures/2.1 Confounding/")

library(ggplot2)
library(plyr)
library(scales)
data(UCBAdmissions)
UCB <- as.data.frame(UCBAdmissions)

# Plot overall acceptance rate
pdf("overall.pdf", width=4, height=4)
overall.accpt <- ggplot(
  ddply(UCB, .(Admit), summarize, Freq=sum(Freq)), 
  aes(x=factor(""), y=Freq, fill = Admit)
) + ggtitle("Overall Acceptance Rate") + guides(fill = guide_legend(reverse = TRUE))
overall.accpt + geom_bar(stat = "identity") + xlab("") + ylab("# of Applicants")
dev.off()


# Plot acceptance rate split by gender
gender.accpt <- ggplot(
  ddply(UCB, .(Admit, Gender), summarize, Freq=sum(Freq)), 
  aes(x=Gender, y=Freq, fill = Admit)
) + ggtitle("Acceptance Rate Split by Gender") + guides(fill = guide_legend(reverse = TRUE))

pdf("gender-accpt-count.pdf", width=8, height=4)
gender.accpt + geom_bar(stat = "identity") + ylab("# of Applicants")
dev.off()

pdf("gender-accpt.pdf", width=8, height=4)
gender.accpt + geom_bar(position = "fill") + ylab("% of Applicants") + 
  scale_y_continuous(labels = percent_format())
dev.off()


# Plot acceptance rate split by department
dept.accpt <- ggplot(
  ddply(UCB, .(Admit, Dept), summarize, Freq=sum(Freq)), 
  aes(x=Dept, y=Freq, fill = Admit)
) + ggtitle("Acceptance Rate Split by Department") + 
  guides(fill = guide_legend(reverse = TRUE)) + xlab("Department")
#dept.accpt + geom_bar(stat = "identity") + ylab("# of Applicants")

pdf("dept-accpt.pdf", width=10, height=5)
dept.accpt + geom_bar(position = "fill") + ylab("% of Applicants") + 
  scale_y_continuous(labels = percent_format())
dev.off()


# Plot applicant's gender split by department
dept.gender <- ggplot(
  ddply(UCB, .(Gender, Dept), summarize, Freq=sum(Freq)), 
  aes(x=Dept, y=Freq, fill = Gender, order = -as.numeric(Gender))
) + ggtitle("Applicant's Gender Split by Department") + xlab("Department")
#dept.gender + geom_bar(stat = "identity") + ylab("# of Applicants")

pdf("dept-gender.pdf", width=10, height=5)
dept.gender + geom_bar(position = "fill") + ylab("% of Applicants") + 
  scale_y_continuous(labels = percent_format())
dev.off()


# Plot acceptance rate split by gender x department
split.accpt <- ggplot(
  UCB, 
  aes(x=Gender, y=Freq, fill = Admit)
) + ggtitle("Acceptance Rate Split by Gender & Department") + facet_wrap(~ Dept, nrow = 2) + 
  guides(fill = guide_legend(reverse = TRUE))
# split.accpt + geom_bar(stat = "identity") + ylab("# of Applicants") 

pdf("split-accpt.pdf", width=10, height=7)
split.accpt + geom_bar(position = "fill") + ylab("% of Applicants") + 
  scale_y_continuous(labels = percent_format())
dev.off()




# Toy plots illustrating Simpson's Paradox
x <- c(1,2,3,4,8,9,10,11)
y <- c(6,7,8,9,1,2,3,4)
color <- c(rep("lightseagreen", 4), rep("purple", 4))

w <- 6
h <- 4

pdf("simpsons1.pdf", width=w, height=h)
plot(x, y, pch=19, cex=2, xlim=c(0, 12), ylim=c(0, 10))
dev.off()

pdf("simpsons2.pdf", width=w, height=h)
plot(x, y, pch=19, cex=2, xlim=c(0, 12), ylim=c(0, 10))
abline(lm(y~x), lwd=2, lty=2)
dev.off()

pdf("simpsons3.pdf", width=w, height=h)
plot(x, y, pch=19, cex=2, col=color, xlim=c(0, 12), ylim=c(0, 10))
dev.off()

pdf("simpsons4.pdf", width=w, height=h)
plot(x, y, pch=19, cex=2, col=color, xlim=c(0, 12), ylim=c(0, 10))
abline(lm(y[1:4]~x[1:4]), lwd=2, col="lightseagreen")
abline(lm(y[4+1:4]~x[4+1:4]), lwd=2, col="purple")
dev.off()

pdf("simpsons5.pdf", width=w, height=h)
plot(x, y, pch=19, cex=2, col=color, xlim=c(0, 12), ylim=c(0, 10))
abline(lm(y[1:4]~x[1:4]), lwd=2, col="lightseagreen")
abline(lm(y[4+1:4]~x[4+1:4]), lwd=2, col="purple")
abline(lm(y~x), lwd=2, lty=2)
legend("topright", legend="overall trend", bty='n', lty=2, lwd=2, col="black")
dev.off()
