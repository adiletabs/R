glm(am ~ disp + vs + mpg, mtcars, family = "binomial")$coefficients
library('ggplot2')
ggplot(ToothGrowth, aes(supp, len), fill = dose)+geom_boxplot(show.legend = T)
ggplot(data = ToothGrowth, aes(supp, len))+geom_boxplot(fill = as.factor(dose))
str(ToothGrowth)
