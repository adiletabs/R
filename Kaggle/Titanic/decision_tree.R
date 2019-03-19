library(rpart)

train <- read.csv('train.csv')
train$PassengerId <- NULL
train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL
train$Embarked <- NULL
train$Survived <- as.factor(train$Survived)

test <- read.csv('test.csv')
test$PassengerId <- NULL
test$Name <- NULL
test$Ticket <- NULL
test$Cabin <- NULL
test$Embarked <- NULL

decision_tree <- rpart(Survived ~ ., train)
res <- predict(decision_tree, test, type='class')

ans <- data.frame(892:1309, res)
colnames(ans) <- c('PassengerId', 'Survived')
write.csv(ans, 'ans3.csv', row.names = F)