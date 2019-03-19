?lm
library("ggplot2")
ggplot(mtcars, aes(hp, mpg))+
  geom_point()+
  geom_smooth(method = "lm")

fit  <- lm(mpg ~ hp, df)
fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

m <- read.table('dataset_11508_12.txt')
fitm <- fit <- lm(V1 ~ V2, m)
str(fitm)
fitm$coefficients

a <- c(1, 2, 3)
b <- c(4, 5, 6)
d <- c(7,  8, 9)
data.frame(a, b, d)

str(diamonds)
m2 <- subset(diamonds, cut == 'Ideal' & carat == 0.46)
fit_coef <- lm(price ~ depth, m2)$coefficients

regr.calc <- function(x)
{
  cf <- cor.test(x[[1]], x[[2]])
  if (cf$p.value >= 0.05)
  {
    return('There is no sense in prediction')
  }
  else
  {
    ft <- lm(x[[1]] ~ x[[2]], x)
    x$fit <- ft$fitted.values
    return(x)
  }
}

regr.calc(iris[,c(1,4)])

ggplot(iris, aes(Sepal.Width, Petal.Width, col = Species))+
  geom_point()+
  geom_smooth(method = "lm")