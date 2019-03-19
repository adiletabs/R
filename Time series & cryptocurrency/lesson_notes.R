# Time series - библиотека для работы с временными данными
# Prophet
# xgboost
# flexdashboard

library('crypto') # Coinmarketcup
library('prophet') # Time series model
library('lubridate') # Handle date - tranfrom character or numeric to date
# df$date <- ymd(df$date)

# для работы с prophet необходимы две колонки
# ds - дата, y - показатель
# prophet хорошо работает с дневными

df <- crypto_history(coin = 'bitcoin')
df <- df[, c(4, 9)]

colnames(df) <- c('ds', 'y')

m <- prophet(df)
future <- make_future_dataframe(m, periods = 100) # спрогнозировать 100 дней
forecast <- predict(m, future)
plot(m, forecast) # синяя линия - прогноз, черная - фактические, тень - коридор ошибки
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
#   Для работы с prophet, необходимы две колонки
#   ds - data, y - показатель
#   prophet   хорошо работает с дневными
colnames(df) <- c("ds","y")
?prophet
m <- prophet(df)
future <- make_future_dataframe(m,periods = 60)
forecast <- predict(m,future)
plot(m,forecast)
prophet_plot_components(m,forecast)

f <- select(forecast, c(ds,yhat))
f %>% top_n(60,ds)