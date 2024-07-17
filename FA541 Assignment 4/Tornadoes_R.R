tornado_data <- read.csv("tornadoes.CSV")

ggplot(tornado_data, aes(x = Year, y = Tornadoes)) +
  geom_point() +
  geom_line() +
  labs(title = "Annual Number of Tornadoes (1953-2008)", x = "Year", y = "Number of Tornadoes") +
  theme_minimal()

fit_tornado <- lm(Tornadoes ~ Year, data = tornado_data)

summary(fit_tornado)

intercept_tornado <- coef(fit_tornado)[1]
slope_tornado <- coef(fit_tornado)[2]
cat("Equation of least-squares line: tornadoes =", intercept_tornado, "+", slope_tornado, "* year\n")

r_squared_tornado <- summary(fit_tornado)$r.squared
cat("R-squared:", r_squared_tornado, "\n")

confint(fit_tornado, level = 0.95)

residuals_tornado <- residuals(fit_tornado)

ggplot(data.frame(Year = tornado_data$Year, Residuals = residuals_tornado), aes(x = Year, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Year", x = "Year", y = "Residuals") +
  theme_minimal()

qqnorm(residuals_tornado)
qqline(residuals_tornado, col = "red")

tornado_data_no_2004 <- tornado_data[tornado_data$Year != 2004,]

fit_tornado_no_2004 <- lm(Tornadoes ~ Year, data = tornado_data_no_2004)

summary(fit_tornado_no_2004)

intercept_no_2004 <- coef(fit_tornado_no_2004)[1]
slope_no_2004 <- coef(fit_tornado_no_2004)[2]
cat("Equation of least-squares line without 2004: tornadoes =", intercept_no_2004, "+", slope_no_2004, "* year\n")

r_squared_no_2004 <- summary(fit_tornado_no_2004)$r.squared
cat("R-squared without 2004:", r_squared_no_2004, "\n")

confint(fit_tornado_no_2004, level = 0.95)

install.packages("ggcorrplot")
library(ggcorrplot)

cor_matrix <- cor(tornado_data)

ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Heatmap")

print(tornado_data$Census.thousands)
fit_tornado_multi <- lm(Tornadoes ~ Year + Census.thousands, data = tornado_data)

summary(fit_tornado_multi)

intercept_multi <- coef(fit_tornado_multi)[1]
slope_year_multi <- coef(fit_tornado_multi)[2]
slope_population_multi <- coef(fit_tornado_multi)[3]

cat("Equation of the multiple regression model: tornadoes =", intercept_multi, "+", 
    slope_year_multi, "* year +", slope_population_multi, "* census\n")

residuals_multiple <- residuals(fit_tornado_multi)

ggplot(data.frame(Year = tornado_data$Year, Residuals = residuals_multiple), aes(x = Year, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Year", x = "Year", y = "Residuals") +
  theme_minimal()

ggplot(data.frame(Population = tornado_data$Census.thousands, Residuals = residuals_multiple), aes(x = Population, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Census", x = "Census", y = "Residuals") +
  theme_minimal()

qqnorm(residuals_multiple)
qqline(residuals_multiple, col = "red")


summary(fit_tornado_multi)$coefficients["Year",]

test_statistic <- summary(fit_tornado_multi)$coefficients["Year", "t value"]
p_value <- summary(fit_tornado_multi)$coefficients["Year", "Pr(>|t|)"]
cat("Test statistic for year:", test_statistic, "\n")
cat("P-value for year:", p_value, "\n")

