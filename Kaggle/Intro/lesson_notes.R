library(dplyr)

df <- read.csv('train.csv')
# get NA by columns

# apply, sapply - делает какие либо операции по столбцам или 
# строкам

# apply(data,1-row and 2-column,function(x))

get_NA <- function(x)
{
  sum(is.na(x))
}

get_NA(df)

sapply(df,function(x){sum(is.na(x))})


apply(df,2,get_NA) # by column
apply(df,1,get_NA)

## Data engineering
# load data
# how to handle with NA
# 
#

#library(ggplot2)
#ggplot(data=df,aes(x=SalePrice))+geom_boxplot()
boxplot(df$SalePrice)
str(df)

fit <- lm(SalePrice ~ ., df) # R 0.9194 
summary(fit)
#library(lmtest)
#shapiro.test(fit$residuals)
#bptest(fit) # less than 0.05 - 
check <- lm((fit$residuals)^2~.,df)
summary(check) # 0.5457 
# if R-sq big - hetero
df_tiny <- df[df$SalePrice<=mean(df$SalePrice)+sd(df$SalePrice),]
boxplot(df_tiny$SalePrice)

fit_tiny <- lm(SalePrice~.,df_tiny) # 0.9081
summary(fit_tiny)
check_tiny <- lm((fit_tiny$residuals)^2~.,df_tiny) # 0.06339
summary(check_tiny)

fit_tiny_log <- lm(log(SalePrice)~.,df_tiny)
summary(fit_tiny_log)
check_tiny_log <- lm((fit_tiny_log$residuals)^2~.,df_tiny) # 0.06339
summary(check_tiny_log)
