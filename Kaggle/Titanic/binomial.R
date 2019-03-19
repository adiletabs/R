train <- read.csv('train.csv')
train$PassengerId <- NULL
train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL
train$Embarked <- NULL
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$SibSp <- as.factor(train$SibSp)
train$Age <- as.integer(train$Age)

test <- read.csv('test.csv')
test$PassengerId <- NULL
test$Name <- NULL
test$Ticket <- NULL
test$Cabin <- NULL
test$Embarked <- NULL
test$Pclass <- as.factor(test$Pclass)
test$SibSp <- as.factor(test$SibSp)
test$Age <- as.integer(test$Age)
hist(train$Age)
shapiro.test(train$Age)
train$Age <- ifelse(is.na(train$Age), 28, train$Age)
test$Age <- ifelse(is.na(test$Age), 27, test$Age)
test$Fare[153] <- median(test$Fare[-153])

fit <- glm(Survived ~ ., train, family = 'binomial')
fit_best <- step(fit)
prob <- predict(fit_best, test, type = 'response')
res <- ifelse(prob > 0.67, 1, 0)

ans <- data.frame(892:1309, res)

colnames(ans) <- c('PassengerId', 'Survived')

write.csv(ans, 'ans.csv', row.names = F)
