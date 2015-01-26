# For Titanic data, investigation of
# 1. effect of class on survival
# 2. effect of "women and children" first policy of the day

# Install these two packages first
library(ggplot2)
library(xtable)

# Load data.
data(Titanic)

# Titanic is a 4 dimensional array, where
# Dim 1 is Class: 1st, 2nd, 3rd, or crew
# Dim 2 is Sex: male or female
# Dim 3 is Age: child or adult
# Dim 4 is Survival (outcome variable):  no or yes
dim(Titanic)
?Titanic



#------------------------------------------------
# Overall survival rates
#------------------------------------------------
# To get overall survival rates, we must sum over all dimensions except the 4th
# one, which is survival.  apply() says: to the Titanic data, apply the function
# sum to all dimensions, except the 4th.
overall <-  apply(Titanic, 4, sum)
names(overall) <- c("Died", "Survived")

# To save plot as pdf, uncomment the pdf() and dev.off() lines:
# pdf("barplot.pdf", width=6, height=6)
barplot(overall, main="Overall Titanic Survival", names.arg=c("Died", "Survived"))
# dev.off()



#------------------------------------------------
# Survival by class
#------------------------------------------------
# To get survival split by class, we must sum over all dimensions except the
# 4th (survival) and the 1st (class)
by.class <- apply(Titanic, c(4,1), sum)

# Stacked barplot
# pdf("barplot2.pdf", width=10, height=5)
barplot(by.class, legend.text=c("Died", "Survived"),
        args.legend=list(x="topleft", bty='n'),
        main="Titanic Survival by Class")
# dev.off()

# Normalized/standardized barplot:
# get the class sums
class.sums <- apply(by.class, 2, sum)
# divide each row of by.class by class.sums to get proportions
by.class.norm <- apply(by.class, 1, function(x){x/class.sums})
# transpose (i.e. make the rows the columns, make the columns the rows)
by.class.norm <- t(by.class.norm)

# pdf("norm_barplot.pdf", width=10, height=5)
barplot(by.class.norm,
        legend.text=c("Died", "Survived"),
        args.legend=list(x="topleft"),
        main="Titanic Survival Normalized by Class")
#dev.off()

# the following is not quite what we want:
mosaicplot(by.class.norm, main="Titanic Survival by Class")

# pdf("mosaic_plot.pdf", width=10, height=5)
# rather we transpose the matrix and switch the rows
by.class.norm <- t(by.class.norm[2:1,])
mosaicplot(by.class.norm, main="Titanic Survival by Class")
# dev.off()

# LaTeX output
xtable(by.class)



#------------------------------------------------
# Survival by class x gender x age
#------------------------------------------------
# We use the ggplot package: http://ggplot2.org/
# Convert Titanic data to data.frame
Titanic.data.frame <- as.data.frame(Titanic)
Titanic.data.frame

# pdf("titanic.pdf", width=10, height=6)
pg.titanic <-
  ggplot(Titanic.data.frame, aes(Class, Freq, fill = Survived)) +
  facet_wrap(~ Age + Sex, nrow = 1) + coord_flip() +
  ggtitle("Titanic Survival Counts by Class x Gender x Age") +
  geom_bar(stat = "identity")
print(pg.titanic)
# dev.off()

# pdf("titanic2.pdf", width=10, height=6)
pg.titanic.prop <-
  ggplot(Titanic.data.frame, aes(x=Class, y=Freq)) +
  geom_bar(aes(fill = Survived), stat = "identity", position="fill") +
  facet_wrap( ~ Age + Sex, nrow = 1) +
  ggtitle("Titanic Survival Proportions by Class x Gender x Age") +
  coord_flip()
print(pg.titanic.prop)
# dev.off()



#------------------------------------------------
# Etc
#------------------------------------------------
temp <- apply(Titanic, c(4,1,2), sum)
temp.male <- temp[,,1]
temp.female <- temp[,,2]

temp.male <- rbind(temp.male, apply(temp.male, 2, sum))
temp.male <- cbind(temp.male, apply(temp.male, 1, sum))
colnames(temp.male)[5] <- "Total"
rownames(temp.male) <- c("Died", "Survived", "Total")
xtable(temp.male)

temp.female <- rbind(temp.female, apply(temp.female, 2, sum))
temp.female <- cbind(temp.female, apply(temp.female, 1, sum))
colnames(temp.female)[5] <- "Total"
rownames(temp.female) <- c("Died", "Survived", "Total")
xtable(temp.female)
