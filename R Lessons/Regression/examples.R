library(data.table)
library(dplyr)
library(ggplot2)
library(Matrix)
library(xgboost)
library(rpart)
library(randomForest)
library(ROSE)
library(car)
library(caret)
library(lubridate)
data("diamonds")
df <- diamonds
diamonds <- diamonds %>% mutate_if(is.integer,as.numeric)

# rpart   
# lm
# glm
# randomForest regression
# randomForest classification
# xgboost

index <- createDataPartition(diamonds$cut,
                             p=0.7,
                             list=FALSE)
                                         
train_fac <- diamonds[index,]   # 70% 
test_fac <- diamonds[-index,]   # 30% 

index_num <- createDataPartition(diamonds$price,
                                 p=0.7,
                                 list=FALSE)
train_num <- diamonds[index_num,]
test_num <- diamonds[-index_num,] 


# rpart

decision_tree <- rpart(cut~.,train_fac)
pred_dt_fac <- predict(decision_tree,test_fac,type="class")
confusionMatrix(test_fac$cut,pred_dt_fac)


# lm

fit <- lm(price~.,sample_n(train_num,4999))
shapiro.test(fit$residuals)
boxplot(fit$residuals)
hist(fit$residuals)

log_method <- function(x)
{
  ifelse(abs(x)>0, log(abs(x)), 0)
}
str(train_num)
cols_num <- sapply(train_fac, function(x){is.numeric(x)})

train_num_log <- as.data.frame(sapply(train_num[,cols_num],log_method))
test_num_log <- as.data.frame(sapply(test_num[,cols_num],log_method))
str(train_num_log)
fit_log <- lm(price~.,sample_n(train_num_log,4999))
shapiro.test(exp(fit_log$residuals))
summary(fit)
summary(fit_log)


str(train_num)
check <- lm((fit$residuals^2)~.,sample_n(train_num,4999))
summary(check)


check_log <- lm((fit_log$residuals^2)~.,sample_n(train_num_log,4999))
summary(check_log)

pred <- predict(fit,test_num)
pred_log <- exp(predict(fit_log,test_num_log))

RMSE(test_num$price,pred)
RMSE(exp(test_num_log$price),pred_log)

# glm

fit_fac <- glm(cut~.,train_fac,family = "binomial")
fit_fac
predict_glm <- predict(fit_fac,test_fac,type="response")
predict_glm

# randomforest 
rf_fac <- randomForest(cut~.,train_fac,
                       type="classification",
                       ntree=300,
                       do.trace=TRUE)
predict_rf_fac <- predict(rf_fac,test_fac,type="class")
confusionMatrix(test_fac$cut,predict_rf_fac)
#varImpPlot(rf_fac)


rf_num <- randomForest(price~.,train_num,
                       type="regression",
                       ntree=300,
                       do.trace=TRUE)
predict_rf_num <- predict(rf_num,test_num)
RMSE(test_num$price,predict_rf_num)


# XGBOOST

train_matrix <- data.matrix(select(train_num,-price))
test_matrix <- data.matrix(select(test_num,-price))

train_target <- train_num$price
test_target <- test_num$price

dtrain <- xgb.DMatrix(data=train_matrix,label=train_target)
ctest <- xgb.DMatrix(data=test_matrix,label=test_target)

watchlist <- list(train=dtrain,test=ctest)
bst <- xgb.train(data=dtrain,
                 nround=2500,
                 #maximize = FALSE,
                 #early_stopping_rounds = 10,
                 #watchlist = watchlist,
                 max_depth=7,
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 eta = 0.1,
                 alpha=1,
                 lambda=0,
                 colsample_bytree=0.5,
                 subsample = 0.5
)
predict <- predict(bst,ctest)
RMSE(test_target,predict)

cv <- xgb.cv(data=dtrain,
             nround=500,
             #maximize = FALSE,
             #early_stopping_rounds = 10,
             watchlist = watchlist,
             nfold=6,
             max_depth=7,
             objective = "reg:linear",
             eval_metric = "rmse",
             alpha=0.01,
             lambda=0.01,
             colsample_bytree=0.7,
             subsample = 0.7
)
