# Time series 
# Prophet
# xgboost
# flexdashboard

library('crypto') # Coinmarketcup
library('prophet') # Time series model
library('lubridate') # Handle date - tranfrom character or numeric to date
# df$date <- ymd(df$date)

df <- crypto_history(coin = 'bitcoin')
df <- df[, c(4, 9)]

colnames(df) <- c('ds', 'y')

m <- prophet(df)
future <- make_future_dataframe(m, periods = 100) 
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)

f <- select(forecast, c(ds, yhat))
f %>% top_n(60, ds)

###############################################################################

library(crypto) # Coinmarketcup
library(ggplot2)
df <- crypto_history(coin="bitcoin")
df <- df[,c(4,9)]
str(df)
library(lubridate) # Handle date
head(df)
df$date <- ymd(df$date)

library(prophet) # Time Series model.
colnames(df) <- c("ds","y")
?prophet
m <- prophet(df)
future <- make_future_dataframe(m,periods = 60)
forecast <- predict(m,future)
plot(m,forecast)
prophet_plot_components(m,forecast)

f <- select(forecast, c(ds,yhat))
f %>% top_n(60, ds)
