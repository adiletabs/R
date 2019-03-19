45 + 232 + 43534
formula1 <- 3 + 4 + 5
class(formula1)
class('er')
class(TRUE)
c("unit", "ree", 4645)
names <- c("jose", "marcelo")
class(names)
ves <- c(65, 56, 40, 50)
rost <- c(160, 178, 190, 185)
cor.test(ves, rost)

t <- 56
class(t)
t == formula1
x <- 5 * (t == formula1)

v <- c(42, 654, 1, 4)
v[c(4, 2)]
v <- v * 5
v2 <- v[v >= 5]
v[v > 4]
sum(v)
name <- c('Mesut', 'Cristiano', 'Tomas', 'Santi')

data <- list(v, ves)
v5 <- data[[2]]

df <- data.frame(Name = name, Heght = rost, Weight = ves)
typeof(df)

my_v <- c(21, 18, 21, 19, 25, 20, 17, 17, 18, 22, 17, 18, 18, 19, 19, 27,
          21, 20, 24, 17, 15, 24, 24, 29, 19, 14, 21, 17, 19, 18, 18, 20,
          21, 21, 19, 19, 17, 21, 13, 17, 13, 23, 15, 23, 24, 16, 17, 25,
          24, 22)
mean <- mean(my_v)
st <- sd(my_v)
mv <- my_v[abs(my_v - mean(my_v)) < sd(my_v)]
mv2 <- my_v[abs(my_v - sd(my_v)) < mean(my_v)]
mv2
