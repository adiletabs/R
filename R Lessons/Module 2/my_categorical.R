df <- read.csv('grants.csv')

df$status <- factor(df$status, labels = c('Not funded', 'Funded'))

t1 <- table(df$status)
t1
dim(t1)
tdf <- table(df)
dim(tdf)

t2 <- table(df$status, df$field)
t2
dim(t2)

t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

hec <- HairEyeColor
dimnames(HairEyeColor)
HairEyeColor[, 'Blue', 'Male']
red_men <- prop.table(HairEyeColor[, 'Blue', 'Male'])[3]
red_men
greeneyed_girls <- sum(HairEyeColor[, 'Green', 'Female'])

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(subset(mydata, Sex == 'Female'), aes(x = Hair, y = Freq, 
                                                fill = Eye))+
  geom_bar(stat="identity", position = 'dodge')+
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

binom.test(t1)

chi <- chisq.test(t2)
t2
chi$exp
chi$obs           

fish <- fisher.test(t2)

brownhaired_girls <- HairEyeColor['Brown', , 'Female']
chisq.test(brownhaired_girls)

?diamonds
main_stat <- chisq.test(table(diamonds$cut, diamonds$color))[[1]][1]

factor_price <- c(ifelse(diamonds$price < mean(diamonds$price), 0, 1))
factor_carat <- c(ifelse(diamonds$carat < mean(diamonds$carat), 0, 1))
main_stat <- chisq.test(factor_price, factor_carat)$statistic

fisher_test <- fisher.test(table(mtcars$am, mtcars$vs))$p.value