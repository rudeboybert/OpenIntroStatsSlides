# Change directory
setwd("./Lec03 Confounding/figure/")

# Install these packages first:
# -ggplot2
# -dplyr
# -scales
# by goign to the Files Panel of RStudio -> Packages Tab -> Install ->
# Lookup package name -> Install

# Load the packages
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

# Save all plots as pdf files.
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
# We make use of the ggplot2 and dplyr packages.  To learn more about
# -ggplot: http://www.r-bloggers.com/basic-introduction-to-ggplot2/
# -dplyr: http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
#
#-------------------------------------------------------------------------------
# Load UC Berkeley admissions data and rename elements in Admit variable
data(UCBAdmissions)
UCB <- as.data.frame(UCBAdmissions)
UCB <- mutate(UCB, Admit = ifelse(Admit =="Admitted", "Accepted", "Rejected"))

#---------------------------------------------------------------
# Plot overall acceptance rate
#---------------------------------------------------------------
accpt <-
  group_by(UCB, Admit) %>%
  summarize(Freq=sum(Freq))
accpt

plot <-
  ggplot(accpt, aes(x=factor(""), y=Freq, fill = Admit)) +
  ggtitle("Overall Acceptance Rate") +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("# of Applicants")
plot

pdf("overall.pdf", width=4, height=4)
plot
dev.off()


#---------------------------------------------------------------
# Plot acceptance rate split by gender
#---------------------------------------------------------------
gender <-
  group_by(UCB, Gender) %>%
  summarize(Total=sum(Freq))
gender

gender.accpt <-
  group_by(UCB, Admit, Gender) %>%
  summarize(Freq=sum(Freq)) %>%
  inner_join(gender) %>%
  mutate(Prop=Freq/Total)
gender.accpt

# Counts
plot <-
  ggplot(gender.accpt, aes(x=Gender, y=Freq, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Gender") +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(stat = "identity") +
  xlab("Gender") + ylab("# of Applicants")
plot

pdf("gender-accpt-count.pdf", width=8, height=4)
plot
dev.off()

# Proportions
plot <- ggplot(gender.accpt, aes(x=Gender, y=Prop, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Gender") +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(stat = "identity") +
  xlab("Gender") + ylab("Proportion of Applicants") +
  scale_y_continuous(labels = percent_format())
plot

pdf("gender-accpt.pdf", width=8, height=4)
plot
dev.off()


#---------------------------------------------------------------
# Plot acceptance rate split by department
#---------------------------------------------------------------
dept <-
  group_by(UCB, Dept) %>%
  summarize(Total=sum(Freq))
dept

dept.accpt <-
  group_by(UCB, Admit, Dept) %>%
  summarize(Freq=sum(Freq)) %>%
  inner_join(dept) %>%
  mutate(Prop=Freq/Total)
dept.accpt

plot <-
  ggplot(dept.accpt, aes(x=Dept, y=Prop, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Department") +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(stat = "identity") +
  xlab("Department") + ylab("Proportion of Applicants")+
  scale_y_continuous(labels = percent_format())
plot

pdf("dept-accpt.pdf", width=10, height=5)
plot
dev.off()


#---------------------------------------------------------------
# Plot applicant's gender split by department
#---------------------------------------------------------------
dept.gender <-
  group_by(UCB, Gender, Dept) %>%
  summarize(Freq=sum(Freq)) %>%
  inner_join(dept) %>%
  mutate(Prop=Freq/Total)
dept.gender

plot <-
  ggplot(dept.gender, aes(x=Dept, y=Prop, fill = Gender, order = -as.numeric(Gender))) +
  ggtitle("Applicant's Gender Split by Department") +
  geom_bar(stat = "identity") +
  xlab("Department") + ylab("% of Applicants") +
  scale_y_continuous(labels = percent_format())
plot

pdf("dept-gender.pdf", width=10, height=5)
plot
dev.off()


#---------------------------------------------------------------
# Plot acceptance rate split by gender x department
#---------------------------------------------------------------
UCB.totals <- group_by(UCB, Gender, Dept) %>%
  summarise(Total=sum(Freq))
UCB.totals

UCB <- inner_join(UCB, UCB.totals) %>%
  mutate(Prop=Freq/Total)
UCB

plot <-
  ggplot(UCB, aes(x=Gender, y=Prop, fill = Admit)) +
  ggtitle("Acceptance Rate Split by Gender & Department") +
  facet_wrap(~ Dept, nrow = 2) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_bar(stat = "identity") +
  xlab("Gender") + ylab("% of Applicants") +
  scale_y_continuous(labels = percent_format())
plot


pdf("split-accpt.pdf", width=10, height=7)
plot
dev.off()

