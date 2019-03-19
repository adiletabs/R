train <- read.csv("train.csv")
sum(is.na(train))
moda(train$Alley)
?mode

# apply(data, x, y, function(x))
sum(is.na(train))
get_na <- function(x)
{
  sum(is.na(x))
}
howmany_na <- sapply(train, get_na)

## Data engineering
# load data
# how to handle w/ NA

ggplot(train, aes(SaleType, SalePrice))+geom_boxplot()

rm_na <- function(x)
{
  if (is.numeric(x))
  {
    ifelse(is.na(x), median(x), x)
  }
  else
  {
    ifelse(is.na(x), moda(x), x)
  }
}
str(train)
train <- data.frame(sapply(train, ,2, rm_na))
fit <- lm(SalePrice ~ ., train) #R 0.9194
summary(fit)

#library(lmtest)
#shapiro.test(fit$residuals)
#bptest(fit) #less than 0.05

check <- lm((fit$residulas)^2~., train)
summary(check) # 0.5457
# if R-sq big - hetero
train_tiny <- train[train$SalePrice <= mean(train$SalePrice) + sd(train$SalePrice),]
boxplot(train_tiny$SalePrice)

fit_tiny <- lm(SalePrice~., train_tiny) # 0.9081
summary(fit_tiny)
check_tiny <- lm((fit_tiny$residuals)^2~., train_tiny) # 0.06339
summary(check_tiny)

which.max(train$Alley)
length(train$Alley[is.na(train$Alley) == F & train$Alley == 'Pave'])
moda <- function(x)
{
  t <- 0
  ans <- NULL
  for (i in levels(x))
  {
    if (length(x[is.na(x) == F & x == i]) > t)
    {
      t <- length(x[is.na(x) == F & x == i])
      ans <- i
    }
  }
  return(ans)
}
moda(train$Alley)
