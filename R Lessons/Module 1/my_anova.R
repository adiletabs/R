df <- read.csv('shops.csv')

# One-way ANOVA

ggplot(mydata, aes(x = origin, y = price)) + 
  geom_boxplot()

fit <- aov(price ~ origin, data=mydata)
summary(fit)

# p-value = 0.0189, цены российских и импортных не равны, то есть по стране
# производства можно спрогнозировать цену продукта

?npk
df1 <- npk
fit <- aov(yield ~ N + P + K, df1)
summary(fit)

fit_iris <- aov(Sepal.Width ~ Species, iris)
summary(fit_iris)
TukeyHSD(fit_iris)

td <- read.csv('therapy_data.csv')
str(td)
td$subject <- as.factor(td$subject)

tmp <- read.csv('https://stepik.org/media/attachments/lesson/11505/Pillulkin.csv')
tmp$patient <- as.factor(tmp$patient)
str(tmp)
fit_tmp <- aov(temperature ~ pill + Error(patient/pill), tmp)
summary(fit_tmp)

fit_tmp2 <- aov(temperature ~ pill * doctor + Error(patient/(pill * doctor)), tmp)
summary(fit_tmp2)

library(ggplot2)
library(Hmisc)
ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, color = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1)+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3)+
  stat_summary(fun.data = mean_cl_boot, geom = 'line')
