?swiss
data(swiss)
View(swiss)

fill_na <- function(x)
{
  df <- subset(x, is.na(x$y) == F)
  fit <- lm(y ~ x_1 + x_2, df)
  x$y_full <- ifelse(is.na(x$y), predict(fit, data.frame(x_1 = x$x_1, x_2 = x$x_2)), x$y)
  return(x)
}

test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
fill_na(test_data)
?predict

fit <- lm(mpg ~ hp + qsec, mtcars)
new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)
str(fit)

summary(lm(rating ~ complaints * critical, attitude))

mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
summary(lm(mpg ~ wt*am, mtcars))

library('ggplot2')
ggplot(mtcars, aes(wt, mpg, col = am))+
  geom_point()+
  geom_smooth(method = 'lm')

?attitude

model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)

ideal_model <- step(model_full, scope = list(lower = model_null, upper = model_full), direction = 'backward')
anova(model_full, ideal_model)

str(LifeCycleSavings)
