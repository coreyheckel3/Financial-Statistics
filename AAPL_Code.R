AAPL=read.csv("AAPL.csv")

cat("Number of Rows:", nrow(AAPL), "\n")
cat("Number of Columns:", ncol(AAPL), "\n")

five_num_summary_Open <- fivenum(AAPL$Open)
five_num_summary_Close <- fivenum(AAPL$Close)
five_num_summary_High <- fivenum(AAPL$High)
five_num_summary_Low <- fivenum(AAPL$Low)
five_num_summary_Adj <- fivenum(AAPL$Adj.Close)
five_num_summary_Volume <- fivenum(AAPL$Volume)
print(five_num_summary_Open)
print(five_num_summary_Close)
print(five_num_summary_High)
print(five_num_summary_Low)
print(five_num_summary_Adj)
print(five_num_summary_Volume)

AAPL$Daily_Mean <- (AAPL$Open + AAPL$Close) / 2
print(AAPL$Daily_Mean)

hist(AAPL$Open, main = "Histogram of Open Prices", xlab = "Open Price", col = "blue", breaks = 30)

boxplot(AAPL$Open, AAPL$High, AAPL$Low, AAPL$Close, names = c("Open", "High", "Low", "Close"),
        main = "Price Distribution", col = "red")

boxplot(AAPL$Open, AAPL$High, AAPL$Low, AAPL$Close, names = c("Open", "High", "Low", "Close"),
        main = "Price Distribution Without Outliers", col = "green", outline = FALSE)
