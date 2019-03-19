library(xgboost)
library(dplyr)
library(missForest)

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

m_train <- missForest(train)
m_test <- missForest(test)

train <- m_train$ximp
test <- m_test$ximp

train_matrix <- data.matrix(select(train, -SalePrice))
test_matrix <- data.matrix(test)

train_target <- train$SalePrice
test_target <- numeric(nrow(test))

dtrain <- xgb.DMatrix(data = train_matrix, label = train_target)
ctest <- xgb.DMatrix(data = test_matrix, label = test_target)

fit <- xgb.train(data=dtrain,
                 nround=2500,
                 max_depth=7,
                 objective='reg:linear',
                 eval_metric='rmse',
                 eta=0.1,
                 alpha=1,
                 lambda=0,
                 colsample_bytree=0.5,
                 subsample = 0.5)

res <- predict(fit, ctest)
ans <- data.frame(1461:2919, res)

colnames(ans) <- c('Id', 'SalePrice')
write.csv(ans, 'ans30.csv', row.names = F)