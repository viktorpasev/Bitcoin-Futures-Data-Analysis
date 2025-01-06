library(ggplot2)
library(tidyverse)
library(zoo)
library(dplyr)

futures <- read.csv("BitcoinFutures.csv")
head(futures)

futures <- futures %>%
  mutate(
    Date = as.Date(Date, format = "%b %d, %Y"),
    Open = as.numeric(gsub(",", "", Open)),
    High = as.numeric(gsub(",", "", High)),
    Low = as.numeric(gsub(",", "", Low)),
    Close = as.numeric(gsub(",", "", Close)),
    Volume = as.numeric(gsub(",", "", Volume))
  ) %>%
  arrange(Date) 

futures <- futures %>%
  mutate(
    Daily_Return = (Close - lag(Close)) / lag(Close) * 100,
    Log_Return = log(Close / lag(Close)) * 100
  )

head(futures)

calc_rolling_vol <- function(x, n = 20) {
  sqrt(rollmean(x^2, k = n, fill = NA, align = "right") * 252)
}

futures$Rolling_Vol <- calc_rolling_vol(futures$Daily_Return)

futures$SMA_20 <- rollmean(futures$Close, k = 20, fill = NA, align = "right")
futures$SMA_50 <- rollmean(futures$Close, k = 50, fill = NA, align = "right")

head(futures, 50)

trend <- ggplot(futures, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Price")) +
  geom_smooth(aes(y = Close), method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Stock Price Movement with Trend",
       y = "Price",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "none")

volatility <- ggplot(futures, aes(x = Date)) +
  geom_line(aes(y = Rolling_Vol, color = "20-day Volatility")) +
  labs(title = "Rolling Volatility",
       y = "Annualized Volatility (%)",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "none")

volume <- ggplot(futures, aes(x = Date)) +
  geom_bar(aes(y = Volume), stat = "identity", fill = "darkblue", alpha = 0.5) +
  labs(title = "Trading Volume",
       y = "Volume",
       x = "Date") +
  theme_minimal()

moving_averages <- ggplot(futures, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Price")) +
  geom_line(aes(y = SMA_20, color = "20-day MA")) +
  geom_line(aes(y = SMA_50, color = "50-day MA")) +
  scale_color_manual(values = c("Price" = "black", "20-day MA" = "blue", "50-day MA" = "red")) +
  labs(title = "Price with Moving Averages",
       y = "Price",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "none")


summary_stats <- futures %>%
  summarise(
    Mean_Daily_Return = mean(Daily_Return, na.rm = TRUE),
    Std_Dev_Return = sd(Daily_Return, na.rm = TRUE),
    Annualized_Vol = Std_Dev_Return * sqrt(252),
    Max_Daily_Loss = min(Daily_Return, na.rm = TRUE),
    Max_Daily_Gain = max(Daily_Return, na.rm = TRUE),
    Average_Volume = mean(Volume, na.rm = TRUE),
    Median_Volume = median(Volume, na.rm = TRUE)
  )

print("Summary Statistics:")
print(summary_stats)

write.csv(futures, "futures_processed.csv", row.names = FALSE)