library(dplyr)
library(randomForest)

train <- read.csv('train.csv')
train$Id <- NULL
test <- read.csv('test.csv')
test$Id <- NULL

cols <- sapply(train, function(x){sum(is.na(x)) <= 400})
train <- train[,cols]
cols2 <- sapply(test, function(x){sum(is.na(x)) <= 400})
test <- test[,cols2]

train <- train %>% mutate_if(is.factor, as.integer)
test <- test %>% mutate_if(is.factor, as.integer)

train <- as.data.frame(apply(train, 2, function(x){ifelse(is.na(x), median(x, na.rm = T), x)}))
test <- as.data.frame(apply(test, 2, function(x){ifelse(is.na(x), median(x, na.rm = T), x)}))

rf <- randomForest(SalePrice ~ ., train,
                       type = "regression",
                       ntree = 350,
                       do.trace = TRUE)

res <- predict(rf, test)
ans <- data.frame(1461:2919, res)

colnames(ans) <- c('Id', 'SalePrice')
write.csv(ans, 'ans.csv', row.names = F)