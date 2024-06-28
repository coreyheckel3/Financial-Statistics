library(ggplot2)

data <- read.csv('pickcount.csv')

histogram <- ggplot(data, aes(x = PickCount)) +  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = 'blue', alpha = 0.7) + geom_density(color = 'red') + ggtitle('Histogram')

print(histogram)

boxplot <- ggplot(data, aes(y = PickCount)) + geom_boxplot(fill = 'blue', alpha = 0.7) + ggtitle('Boxplot')

print(boxplot)

qqplot <- ggplot(data, aes(sample =PickCount)) + stat_qq() + stat_qq_line() + ggtitle('Normal Q-Q Plot')

print(qqplot)


mean <- mean(data$PickCount)
std_dev <- sd(data$PickCount)
standard_error <- std_dev / sqrt(length(data$PickCount))

mean
std_dev
standard_error

confidence_level <- 0.90
degrees_freedom <- length(data$PickCount) - 1
t_value <- qt((1 + confidence_level) / 2, degrees_freedom)
margin_of_error <- t_value * standard_error
confidence_interval <- c(mean - margin_of_error, mean + margin_of_error)

confidence_interval

#Question 5

sigma <- 15
mu_0 <- 0
mu_1 <- 2
alpha <- 0.05
power <- 0.80
z_alpha <- qnorm(1 - alpha)
z_beta <- qnorm(power)

n <- ((z_alpha + z_beta) * sigma / mu_1) ^ 2

n

effect_size_3 <- 3
power_mu_3 <- pnorm((effect_size_3 / sigma) * sqrt(n) - z_alpha)

power_mu_3
