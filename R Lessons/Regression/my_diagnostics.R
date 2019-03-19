my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 
               0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 
               0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 
               0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 
               0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(sqrt(my_vector))
library('ggplot2')
df <- data.frame()
df$my_vector <- my_vector
hist(sqrt(my_vector))
hist(1/ my_vector)
shapiro.test(1 / my_vector)
hist(log(my_vector))
shapiro.test(log(my_vector))

?scale
sd(scale(my_vector))

beta.coef <- function(x)
{
  a <- scale(x[[1]])
  b <- scale(x[[2]])
  fit <- lm(a ~ b)
  return(fit$coefficients)
}
beta.coef(mtcars[,c(1,3)])

shapiro.test(mtcars$mpg)$p.value
vector <- sapply(iris, is.numeric) 
normality.test  <- function(x)
{
  res <- NULL
  for (i in 1:ncol(x))
  {
    res <- c(res, shapiro.test(x[[i]])$p.value)
  }
  names(res) <- names(x)
  return(res)
}

normality.test(mtcars[,1:6])
library('gvlma')
df <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
lm <- lm(DV ~ IV, df)
summary(lm)
x <- gvlma(lm)
summary(x)

fd <- lm$residuals
fdd <- as.data.frame(lm$residuals)

resid.norm  <- function(fit)
{
  fit.residuals <- fit$residuals
  df <- as.data.frame(fit.residuals)
  st <- shapiro.test(fit.residuals)$p.value
  if (st >= 0.05)
  {
    my_plot <- ggplot(df, aes(x = fit.residuals))+
      geom_histogram(binwidth = 0.4, fill = 'green')
  }
  else
  {
    my_plot <- ggplot(df, aes(x = fit.residuals))+
      geom_histogram(binwidth = 0.4, fill = 'red')
  }
  return(my_plot)
}
library('ggplot2')

fit <- lm(mpg ~ disp, mtcars)
df <- as.data.frame(fit$residuals)
shapiro.test(fit$residuals)$p.value
ggplot(df, aes(fit$residuals))+geom_histogram(binwidth = 0.4, fill = 'red')
resid.norm(lm(mpg ~ wt, mtcars))

df <- iris[, sapply(iris, is.numeric)]
?pairs
?cor
t <- table(pairs(df))
high.corr <- function(x)
{
  res <- 0
  ans <- NULL
  for (i in 1:ncol(x))
  {
    for (j in 1:ncol(x))
    {
      if (i != j & abs(cor(x[i], x[j])) > res)
      {
        res <- abs(cor(x[i], x[j]))
        ans <- c(colnames(x[i]), colnames(x[j]))
      }
    }
  }
  return(ans)
}

high.corr <- function(x)
{    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))
}
