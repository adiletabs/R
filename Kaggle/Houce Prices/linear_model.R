library(dplyr)

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

mn <- mean(train$SalePrice)
sn <- sd(train$SalePrice)
train <- train[train$SalePrice <= mn + sn & train$SalePrice >= mn - sn,]

fit <- lm(SalePrice ~ ., train)
fit_best <- step(fit)
prices <- predict(fit_best, test)
answer <- data.frame(1461:2919, prices)
hist(fit_best$residuals)
colnames(answer) <- c('Id', 'SalePrice')
write.csv(answer, 'ans.csv', row.names = F)
