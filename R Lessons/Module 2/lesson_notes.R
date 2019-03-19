mtcars$new <- ifelse(mtcars$mpg > mean(mtcars$mpg), 1, 0)
mtcars$mpg <- NULL

#formula to split data 80/20
install.packages("caret", dependencies = T)
library(caret)
index <- createDataPartition(mtcars$new, p = 0.8, list = F)

train <- mtcars[index,]
test <- mtcars[-index,]

library(rpart)
decision_tree <- rpart(new~., train)
#rpart(ЗП~., data = )
#rpart(ЗП~a+b+c+d+..., data =)
#. означает все переменные как НП
predicted <- predict(decision_tree, test)
#predicted <- predict(decision_tree, test, type = 'class')
library(ROSE)
#roc.curve - метрика исчисления эффективности модели, только для бинарных данных
roc.curve(test$new, predicted)
#confusionMatrix(test$new, predicted)