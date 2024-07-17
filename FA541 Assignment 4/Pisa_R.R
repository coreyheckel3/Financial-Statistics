library(ggplot2)

pisa_data <- read.csv("PISA.CSV")

ggplot(pisa_data, aes(x = year, y = lean)) +
  geom_point() +
  geom_line() +
  labs(title = "Lean of the Tower of Pisa (1975-1987)", x = "Year", y = "Lean (in tenths of mm over 2.9 meters)") +
  theme_minimal()

fit <- lm(lean ~ year, data = pisa_data)

summary(fit)

intercept <- coef(fit)[1]
slope <- coef(fit)[2]
cat("Equation of least-squares line: lean =", intercept, "+", slope, "* year\n")

r_squared <- summary(fit)$r.squared
cat("R-squared:", r_squared, "\n")

confint(fit, level = 0.99)

year_1918 <- 18
predicted_lean_1918 <- intercept + slope * year_1918
cat("Predicted lean in 1918:", predicted_lean_1918, "tenths of mm over 2.9 meters\n")


year_2012 <- 112
predicted_lean_2012 <- intercept + slope * year_2012
cat("Predicted lean in 2012:", predicted_lean_2012, "tenths of mm over 2.9 meters\n")
