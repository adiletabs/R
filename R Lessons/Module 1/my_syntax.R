data <- read.csv('evals.csv')

a <- -10

if (a > 0) {
  print('positive')
} else
{
  print('negative')
  a <- a + 1
}

for (i in 1:10)
  print(i)

for (i in 1:ncol(data))
  print(data[1,][i])

data$quality <- rep(NA, nrow(data))
for (i in 1:nrow(data))
{
  if (data$score[i] >= 4.5)
  {
    data$quality[i] <- 'good'
  } else data$quality[i] <- 'bad'
}

data$quality <- ifelse(data$score >= 4.5, 'good', 'bad')


good_months <- NULL
for (i in 2:144)
{
  if (AirPassengers[i] > AirPassengers[i - 1])
    good_months <- c(good_months, AirPassengers[i])
}

moving_average <- numeric(135)
for (i in 1:135)
{
  moving_average[i] <- mean(AirPassengers[i:(i + 9)])
}