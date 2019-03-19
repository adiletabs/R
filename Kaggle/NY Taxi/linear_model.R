library(stringr)
library(purrr)
library(dplyr)

sample <- read.csv('sample_submission.csv')
train <- read.csv('train.csv')
test <- read.csv('test.csv')
train$id <- NULL
test$id <- NULL
train$dropoff_datetime <- NULL
train$vendor_id <- as.factor(train$vendor_id)
test$vendor_id <- as.factor(test$vendor_id)

train2 <- train
test2 <- test

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

mn <- mean(train2$trip_duration)
sn <- sd(train2$trip_duration)
train3 <- train2[train2$trip_duration <= mn + sn & train2$trip_duration >= mn - sn,]

fit <- lm(trip_duration ~ ., train3)
res <- predict(fit, test2)

res_pos <- ifelse(res < 0, 1, res)
res2 <- as.integer(res_pos)
ans <- data.frame(sample$id, res2)
colnames(ans) <- colnames(sample)
write.csv(ans, 'ans3.csv', row.names = F)

###################################################################################

train4 <- sample_n(train3, 1500)
fit_sample <- lm(trip_duration ~ ., train4)
summary(fit_sample_best)
fit_sample_best <- step(fit_sample)
res_sample <- predict(fit_sample_best, test2)
res_sample_pos <- ifelse(res_sample < 0, 1, res_sample)
res_sample2 <- as.integer(res_sample_pos)
ans_sample <- data.frame(sample$id, res_sample2)
colnames(ans_sample) <- c('id', 'trip_duration')
write.csv(ans, 'ans.csv', row.names = F)
