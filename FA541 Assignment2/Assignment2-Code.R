data <- read.csv("gasmileage.csv", header=TRUE)
mpg_values <- data$MPG
mean_mpg <- mean(mpg_values)
mean_mpg
sigma <- 3.5
n <- length(mpg_values)

sigma_x <- sigma / sqrt(n)
sigma_x

hist(mpg_values, main="Histogram of MPG Values", xlab="MPG", ylab="Frequency", col="blue", breaks=10)
boxplot(mpg_values, main="Boxplot of MPG Values", ylab="MPG")
qqnorm(mpg_values, main="QQ Plot of MPG Values")
qqline(mpg_values, col="red")

install.packages("moments")
library(moments)

summary(mpg_values)
skewness(mpg_values)
kurtosis(mpg_values)

alpha <- 0.05
z_star <- qnorm(1 - alpha/2)
margin_of_error <- z_star * sigma_x

ci_lower <- mean_mpg - margin_of_error
ci_upper <- mean_mpg + margin_of_error

ci_lower
ci_upper

cat("95% Confidence Interval for the mean mpg: (", ci_lower, ", ", ci_upper, ")\n")


sigma <- 0.0002
n <- 5
sample_mean <- 10.0023
z_star <- qnorm(0.99)
margin_of_error <- z_star * (sigma / sqrt(n))
ci_lower <- sample_mean - margin_of_error
ci_upper <- sample_mean + margin_of_error
cat("98% Confidence Interval for the mean: (", ci_lower, ", ", ci_upper, ")\n")

desired_margin_of_error <- 0.0001
required_n <- (z_star * sigma / desired_margin_of_error)^2
cat("Required sample size for a margin of error of ±0.0001 with 98% confidence: ", ceiling(required_n), "\n")

data <- read.csv("mpgcomparison.csv")
mpg_comparison <- data$Diff
mpg_comparison
sigma <- 3.0
sample_mean <- mean(mpg_comparison)
n <- length(mpg_comparison)
t_statistic <- sample_mean / (sigma / sqrt(n))
p_value <- 2 * (1 - pnorm(abs(t_statistic)))
cat("Sample mean:", sample_mean, "\n")
cat("Sample size:", n, "\n")
cat("T-statistic:", t_statistic, "\n")
cat("P-value:", p_value, "\n")
