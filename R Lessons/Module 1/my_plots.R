df  <- mtcars
df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

hist(df$mpg, breaks = 30, xlab = 'MPG')

boxplot(mpg ~ am, df, ylab = "MPG")

library(ggplot2)

ggplot(airquality, aes(x = factor(airquality$Month), y = airquality$Ozone))+
  geom_boxplot()

plot1 <- ggplot(mtcars, aes(x = mtcars$mpg, y = mtcars$disp, col = mtcars$hp))+
  xlab('mpg')+ylab('disp')+
  geom_point()

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point(aes(size = Petal.Length))
