df <- iris
df1  <- subset(df, Species != "setosa")

?shapiro.test
?bartlett.test
?t.test

data(ToothGrowth)

t_stat <- t.test(ToothGrowth$len[ToothGrowth$supp == 'OJ' & ToothGrowth$dose == 0.5],
       ToothGrowth$len[ToothGrowth$supp == 'VC' & ToothGrowth$dose == 2])$statictic


lv <- read.csv('lekarstva.csv')
t.test(lv$Pressure_before, lv$Pressure_after, paired = T)

?wilcox.test

m1 <- read.table("dataset_11504_15 (2).txt")
str(m1)
bartlett.test(V1 ~ V2, m1)
wilcox.test(V1 ~ V2, m1)

m2 <- read.table("dataset_11504_16.txt")
str(m2)
t.test(m2$V1, m2$V2)