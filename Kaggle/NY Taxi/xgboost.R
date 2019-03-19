library(stringr)
library(purrr)
library(dplyr)
library(xgboost)

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
train2$store_and_fwd_flag <- as.integer(train2$store_and_fwd_flag)
test2$store_and_fwd_flag <- as.integer(test2$store_and_fwd_flag)

time <- str_split(train2$pickup_datetime, " ")
time <- map_chr(time, 2)
time <- str_split(time, ":")
time <- map_chr(time, 1)
time <- as.vector(time)
time <- as.integer(time)
train2$pickup_datetime <- time

time_test <- str_split(test$pickup_datetime, " ")
time_test <- map_chr(time_test, 2)
time_test <- str_split(time_test, ":")
time_test <- map_chr(time_test, 1)
time_test <- as.vector(time_test)
time_test <- as.integer(time_test)
test2$pickup_datetime <- time_test

dist_train <- abs(train2$pickup_longitude - train2$dropoff_longitude) + 
  abs(train2$pickup_latitude - train2$dropoff_latitude)
dist_test <- abs(test2$pickup_longitude - test2$dropoff_longitude) + 
  abs(test2$pickup_latitude - test2$dropoff_latitude)
train2$dist <- dist_train
test2$dist <- dist_test

train_matrix <- data.matrix(select(train2, -trip_duration))
test_matrix <- data.matrix(test2)

train_target <- train2$trip_duration
test_target <- numeric(nrow(test2))

dtrain <- xgb.DMatrix(data = train_matrix, label = train_target)
ctest <- xgb.DMatrix(data = test_matrix, label = test_target)

fit <- xgb.train(data=dtrain,
                 nround=1000,
                 max_depth=6,
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 eta = 0.1,
                 alpha=1,
                 lambda=0,
                 colsample_bytree=0.5,
                 subsample = 0.5)

res <- predict(fit, ctest)
res_pos <- ifelse(res < 0, 1, res)
res_pos <- as.integer(res_pos)
ans <- data.frame(sample$id, res_pos)
colnames(ans) <- colnames(sample)
write.csv(ans, 'ans7.csv', row.names = F)
