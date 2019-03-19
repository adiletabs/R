data <- read.csv('evals.csv')
head(data, 3)
View(data)
str(data)
summary(data)
names(data)
nrow(data)

b <- data$score
mean(b)
min(b)
b * 10

data$number <- 1:nrow(data)
ncol(data)

data[1, 2]
data[c(34, 67, 105), c(1, 2, 3)]

data[5:10,] #all columns of 5-10 rows

data$gender == 'female'
data$age[data$gender == 'female']
summary(data[data$gender == 'female', 1])


subset(data, gender = 'female')
subset(data, score >= 4.5)

data2 <- subset(data, gender == 'male')
data3 <- subset(data, gender == 'female')
data_gender_sorted <- rbind(data2, data3)

library(help = 'datasets')
help("mtcars")
data(mtcars)
mtcars$even_gear <- 1 * (mtcars$gear %% 2 == 0)
