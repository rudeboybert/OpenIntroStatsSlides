install.packages("RCurl")
install.packages("dplyr")


# This package allows for fast, consistent, and convenient tools for working 
# with data frame like objects
library(dplyr)

# This package allows R to to compose general HTTP requests and provides 
# convenient functions to fetch URIs, get & post forms, etc. and process the 
# results returned by the Web server.
library(RCurl)
raw.data <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv")
booze <- read.csv(text = raw.data, header=TRUE)

# Look at data
head(booze)


arrange(booze, desc(spirit_servings)) %>% select(country, spirit_servings) %>% head(n=10)


arrange(booze, desc(beer_servings)) %>% select(country, beer_servings) %>% head(n=10)


arrange(booze, desc(wine_servings)) %>% select(country, wine_servings) %>% head(n=10)


arrange(booze, desc(total_litres_of_pure_alcohol)) %>% select(country, total_litres_of_pure_alcohol) %>% head(n=10)


arrange(booze, total_litres_of_pure_alcohol) %>% select(country, total_litres_of_pure_alcohol) %>% head(n=10)


alcohol <- booze$total_litres_of_pure_alcohol
mu <- mean(alcohol)
mu <- round(mu, 3)
sigma <- sd(alcohol)
sigma <- round(sigma, 3)
par(mfrow=c(1, 2))
hist(alcohol, xlab="litres of pure alcohol", ylab="# of counties", main="Observed Annual Alcohol Consumption")
legend("topright", bty='n', 
       legend=c(paste("mean =", mu), paste("sd =", sigma))
       )


theor.hist <- hist(rnorm(10^6, mean=mu, sd=sigma), plot=FALSE, breaks=8)
theor.hist$counts <- theor.hist$density * length(alcohol)
plot(theor.hist, xlab="litres of pure alcohol", ylab="# of counties",
     main="Theoretically Normal Data")
legend("topright", bty='n', 
       legend=c(paste("mean = ", round(mu,3)), 
                paste("sd = ", round(sigma,3))
       ))


# A nice function that allows you to plot all
par(mfrow=c(1,2))
plot(ecdf(alcohol),
     xlab="quantiles of litres of pure alcohol", 
     ylab="proportion less than quantile", main="Observed Quantiles")


plot(ecdf(rnorm(length(alcohol), mean=mu, sd=sigma)), 
     xlab="quantiles litres of pure alcohol", 
     ylab="proportion less than quantile", main="Theoretical Normal Quantiles")


epsilon <- 0.0001
p <- seq(epsilon, 1-epsilon, length=length(alcohol))
par(mfrow=c(1, 1))
plot(qnorm(p, mean=mu, sd=sigma), quantile(alcohol, probs = p), pch=19, 
     xlab="Theoretical Quantiles", ylab="Sample Quantiles",
     main="Normal Q-Q Plot")
abline(c(0, 1))


