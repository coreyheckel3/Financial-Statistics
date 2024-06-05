NBA = read.csv("NBATeamValuation.csv")

plot(NBA$Revenue, NBA$Current.Value, main = "Team Value vs Revenue", xlab = "Revenue (in millions)", ylab = "Team Value (in millions)", pch = 19)

plot(NBA$Debt.Value, NBA$Current.Value, main = "Team Value vs Debt", xlab = "Debt (in millions)", ylab = "Team Value (in millions)", pch = 19)

plot(NBA$Operating.Income, NBA$Current.Value, main = "Team Value vs Operating Income", xlab = "Operating Income (in millions)", ylab = "Team Value (in millions)", pch = 19)

cor(NBA$Current.Value, NBA$Revenue)
cor(NBA$Current.Value, NBA$Debt.Value)
cor(NBA$Current.Value, NBA$Operating.Income)

indices_to_exclude <- c(7, 19)
cleaned_nbaData <- NBA[-indices_to_exclude, ]

plot(cleaned_nbaData$Operating.Income, cleaned_nbaData$Current.Value, 
     main = "NBA Team Value vs Operating Income (Excluding Indices 7 and 19)",
     xlab = "Operating Income (in millions)", 
     ylab = "Team Value (in millions)", 
     pch = 19)
cor(cleaned_nbaData$Current.Value, cleaned_nbaData$Operating.Income)
