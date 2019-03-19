library(stringr)
library(purrr)
library(dplyr)
library(randomForest)

sample <- read.csv('sample_submission.csv')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

train2 <- train
test2 <- test
mn <- mean(train2$trip_duration)
sn <- sd(train2$trip_duration)
train2 <- train2[train2$trip_duration <= mn + sn & train2$trip_duration >= mn - sn,]
train2 <- sample_n(train2, 2500)
train2$id <- NULL
train2$dropoff_datetime <- NULL
test2$id <- NULL
train2$vendor_id <- as.factor(train2$vendor_id)
test2$vendor_id <- as.factor(test$vendor_id)

time <- str_split(train2$pickup_datetime, " ")
time <- map_chr(time, 2)
time <- str_split(time, ":")
time <- map_chr(time, 1)
time <- as.vector(time)
time <- as.factor(time)
train2$pickup_datetime <- time

time_test <- str_split(test$pickup_datetime, " ")
time_test <- map_chr(time_test, 2)
time_test <- str_split(time_test, ":")
time_test <- map_chr(time_test, 1)
time_test <- as.vector(time_test)
time_test <- as.factor(time_test)
test2$pickup_datetime <- time_test

dist_train <- abs(train2$pickup_longitude - train2$dropoff_longitude) + 
  abs(train2$pickup_latitude - train2$dropoff_latitude)
dist_test <- abs(test2$pickup_longitude - test2$dropoff_longitude) + 
  abs(test2$pickup_latitude - test2$dropoff_latitude)
train2$dist <- dist_train
test2$dist <- dist_test

rf <- randomForest(trip_duration ~ ., train2,
                   type = "regression",
                   ntree = 270,
                   do.trace = TRUE)

res <- predict(rf, test2)
res <- as.integer(res)

ans <- data.frame(sample$id, res)
colnames(ans) <- colnames(sample)
write.csv(ans, 'ans.csv', row.names = F)
