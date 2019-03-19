library(xgboost)
library(dplyr)

train <- read.csv('train.csv')
train$PassengerId <- NULL
train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL
train$Embarked <- NULL

test <- read.csv('test.csv')
test$PassengerId <- NULL
test$Name <- NULL
test$Ticket <- NULL
test$Cabin <- NULL
test$Embarked <- NULL

train <- train %>% mutate_if(is.factor, as.numeric)
test <- test %>% mutate_if(is.factor, as.numeric)

train_matrix <- data.matrix(select(train, -Survived))
test_matrix <- data.matrix(test)

train_target <- train$Survived
test_target <- head(train_target, 418)

dtrain <- xgb.DMatrix(data = train_matrix, label = train_target)
ctest <- xgb.DMatrix(data = test_matrix, label = test_target)

fit <- xgb.train(data=dtrain,
                 nround=1000,
                 max_depth=7,
                 eval_metric = "auc",
                 eta = 0.1,
                 alpha=1,
                 lambda=0,
                 colsample_bytree=0.5,
                 subsample = 0.5,
                 objective = "binary:logistic")
res <- predict(fit, ctest)
res2 <- ifelse(res > 0.9, 1, 0)


ans <- data.frame(892:1309, res2)
colnames(ans) <- c('PassengerId', 'Survived')
write.csv(ans, 'ans.csv', row.names = F)
# 0.75119
