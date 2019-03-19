View(mtcars)
?mtcars

df$vs <- factor(df$vs, label = c('V', 'S'))
df$am <- factor(df$am, label = c('Auto', 'Manual'))
df <- mtcars
hp_vsam <- aggregate(hp ~ vs + am, df, mean)

library(psych)
description <- describe(x = df[-c(8, 9)])

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)

descr3 <- describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, 
           fast = T)

?length
?airquality

subs <- subset(airquality, Month%in%c(7, 8, 9))
length(subs)
nrow(subs)
result <- aggregate(Ozone ~ Month, subs, length)

task11 <- describeBy(airquality, group = airquality$Month, mat = T, digits = 1)

data(iris)
describe((iris))
task14 <- describeBy(iris, group = iris$Species, mat = T, digits = 1)

?replace
my_vector <- c(23, 10, 16, 19, 23, 22, 16, 21, 24, 20, 22, 21, 19, 25, 22, 14, 
               22, 14, 16, 15, NA, 24, NA, NA, NA, 23, 15, 21, 24, NA, NA, NA,
               18, 21, 18, NA, 17, 20, 17, NA)
fixed_vector <- ifelse(is.na(my_vector), mean(my_vector, na.rm = T), my_vector)
fixed_vector
is.na(my_vector)
