# Change directory
setwd("./Lec02 Sampling/figure/")

# Install these packages first
library(ggplot2)
library(dplyr)
library(scales)


#-------------------------------------------------------------------------------
#
# Illustrate Simpsons Paradox via regression
#
#-------------------------------------------------------------------------------
# Toy plots illustrating Simpson's Paradox
x <- c(1,2,3,4,8,9,10,11)
y <- c(6,7,8,9,1,2,3,4)
color <- c(rep("lightseagreen", 4), rep("purple", 4))

fig.width <- 6
fig.height <- 4

pdf("simpsons1.pdf", width=fig.width, height=fig.height)
plot(x, y, pch=19, cex=2, xlim=c(0, 12), ylim=c(0, 10))
dev.off()

pdf("simpsons2.pdf", width=fig.width, height=fig.height)
plot(x, y, pch=19, cex=2, xlim=c(0, 12), ylim=c(0, 10))
abline(lm(y~x), lwd=2, lty=2)
legend("topright", legend="overall trend", bty='n', lty=2, lwd=2, col="black")
dev.off()

pdf("simpsons3.pdf", width=fig.width, height=fig.height)
plot(x, y, pch=19, cex=2, col=color, xlim=c(0, 12), ylim=c(0, 10))
dev.off()

pdf("simpsons4.pdf", width=fig.width, height=fig.height)
plot(x, y, pch=19, cex=2, col=color, xlim=c(0, 12), ylim=c(0, 10))
abline(lm(y[1:4]~x[1:4]), lwd=2, col="lightseagreen")
abline(lm(y[4+1:4]~x[4+1:4]), lwd=2, col="purple")
dev.off()

pdf("simpsons5.pdf", width=fig.width, height=fig.height)
plot(x, y, pch=19, cex=2, col=color, xlim=c(0, 12), ylim=c(0, 10))
abline(lm(y[1:4]~x[1:4]), lwd=2, col="lightseagreen")
abline(lm(y[4+1:4]~x[4+1:4]), lwd=2, col="purple")
abline(lm(y~x), lwd=2, lty=2)
legend("topright", legend="overall trend", bty='n', lty=2, lwd=2, col="black")
dev.off()




#-------------------------------------------------------------------------------
#
# ADVANCED:  Illustrate Simpsons Paradox via the UC Berkeley Admissions Data
# We make use of the ggplot2 and dplyr packages
#
#-------------------------------------------------------------------------------
# Load UC Berkeley admissions data
data(UCBAdmissions)
UCB <- as.data.frame(UCBAdmissions)


#---------------------------------------------------------------
# Plot overall acceptance rate
#---------------------------------------------------------------
overall.accpt <-
  group_by(UCB, Admit) %>%
  summarize(Freq=sum(Freq))

pdf("overall.pdf", width=4, height=4)
ggplot(overall.accpt, aes(x=factor(""), y=Freq, fill = Admit)) +
  ggtitle("Overall Acceptance Rate") +
  guides(fill = guide_legend(reverse = TRUE)) + geom_bar(stat = "identity") +
  xlab("") + ylab("# of Applicants")
dev.off()


#---------------------------------------------------------------
# Plot acceptance rate split by gender
#---------------------------------------------------------------
gender.accpt <-
  group_by(UCB, Admit, Gender) %>%
  summarize(Freq=sum(Freq))

gender.accpt.plot <- ggplot(gender.accpt, aes(x=Gender, y=Freq, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Gender") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ylab("# of Applicants")

pdf("gender-accpt-count.pdf", width=8, height=4)
gender.accpt.plot + geom_bar(stat = "identity")
dev.off()

pdf("gender-accpt.pdf", width=8, height=4)
gender.accpt.plot + geom_bar(position = "fill") + scale_y_continuous(labels = percent_format())
dev.off()


#---------------------------------------------------------------
# Plot acceptance rate split by department
#---------------------------------------------------------------
dept.accpt <-
  group_by(UCB, Admit, Dept) %>%
  summarize(Freq=sum(Freq))

dept.accpt.plot <-
  ggplot(dept.accpt, aes(x=Dept, y=Freq, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Department") +
  guides(fill = guide_legend(reverse = TRUE)) + xlab("Department")
#dept.accpt + geom_bar(stat = "identity") + ylab("# of Applicants")

pdf("dept-accpt.pdf", width=10, height=5)
dept.accpt.plot + geom_bar(position = "fill") + ylab("% of Applicants") +
  scale_y_continuous(labels = percent_format())
dev.off()


#---------------------------------------------------------------
# Plot applicant's gender split by department
#---------------------------------------------------------------
dept.gender <-
  group_by(UCB, Gender, Dept) %>%
  summarize(Freq=sum(Freq))

dept.gender.plot <-
  ggplot(dept.gender, aes(x=Dept, y=Freq, fill = Gender, order = -as.numeric(Gender))
) + ggtitle("Applicant's Gender Split by Department") + xlab("Department")
#dept.gender + geom_bar(stat = "identity") + ylab("# of Applicants")

pdf("dept-gender.pdf", width=10, height=5)
dept.gender + geom_bar(position = "fill") + ylab("% of Applicants") +
  scale_y_continuous(labels = percent_format())
dev.off()


#---------------------------------------------------------------
# Plot acceptance rate split by gender x department
#---------------------------------------------------------------
split.accpt <-
  ggplot(UCB, aes(x=Gender, y=Freq, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Gender & Department") +
  facet_wrap(~ Dept, nrow = 2) +
  guides(fill = guide_legend(reverse = TRUE))
# split.accpt + geom_bar(stat = "identity") + ylab("# of Applicants")

pdf("split-accpt.pdf", width=10, height=7)
split.accpt + geom_bar(position = "fill") + ylab("% of Applicants") +
  scale_y_continuous(labels = percent_format())
dev.off()