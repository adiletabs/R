train <- read.csv('train.csv')
train$Id <- NULL
test <- read.csv('test.csv')
test$Id <- NULL

train$MSZoning <- as.numeric(train$MSZoning)
train$MSZoning <- as.factor(train$MSZoning)
test$MSZoning <- as.numeric(test$MSZoning)
test$MSZoning <- ifelse(is.na(test$MSZoning), 4, test$MSZoning)
test$MSZoning <- as.factor(test$MSZoning)
train$LotFrontage <- ifelse(is.na(train$LotFrontage), 0, train$LotFrontage)
test$LotFrontage <- ifelse(is.na(test$LotFrontage), 0, test$LotFrontage)
train$Alley <- as.factor(ifelse(is.na(train$Alley), '3', train$Alley))
test$Alley <- as.factor(ifelse(is.na(test$Alley), '3', test$Alley))
train$Utilities <- NULL
test$Utilities <- NULL
train$LandSlope <- NULL
test$LandSlope <- NULL
train <- train[train$Condition2 != 'RRAe' & train$Condition2 != 'RRAn' & train$Condition2 != 'RRNn',]
train$Condition2 <- factor(train$Condition2)
train <- train[train$HouseStyle != '2.5Fin',]
train$HouseStyle <- factor(train$HouseStyle)
train$OverallQual <- as.factor(train$OverallQual)
test$OverallQual <- as.factor(test$OverallQual)
train$OverallCond <- as.factor(train$OverallCond)
test$OverallCond <- as.factor(test$OverallCond)
train$RoofMatl <- NULL
test$RoofMatl <- NULL
train <- train[train$Exterior1st != 'ImStucc' & train$Exterior1st != 'Stone',]
train$Exterior1st <- factor(train$Exterior1st)
train$Exterior1st <- as.numeric(train$Exterior1st)
test$Exterior1st <- as.numeric(test$Exterior1st)
test$Exterior1st <- ifelse(is.na(test$Exterior1st), 11, test$Exterior1st)
train$Exterior1st <- as.factor(train$Exterior1st)
test$Exterior1st <- as.factor(test$Exterior1st)
train <- train[train$Exterior2nd != 'Other',]
train$Exterior2nd <- factor(train$Exterior2nd)
train$Exterior2nd <- as.numeric(train$Exterior2nd)
test$Exterior2nd <- as.numeric(test$Exterior2nd)
test$Exterior2nd <- ifelse(is.na(test$Exterior2nd), 13, test$Exterior2nd)
train$Exterior2nd <- as.factor(train$Exterior2nd)
test$Exterior2nd <- as.factor(test$Exterior2nd)
train$MasVnrType <- as.numeric(train$MasVnrType)
test$MasVnrType <- as.numeric(test$MasVnrType)
train$MasVnrType <- ifelse(is.na(train$MasVnrType), 3, train$MasVnrType)
test$MasVnrType <- ifelse(is.na(test$MasVnrType), 3, test$MasVnrType)
train$MasVnrType <- as.factor(train$MasVnrType)
test$MasVnrType <- as.factor(test$MasVnrType)
train$MasVnrArea <- ifelse(is.na(train$MasVnrArea), 0, train$MasVnrArea)
test$MasVnrArea <- ifelse(is.na(test$MasVnrArea), 0, test$MasVnrArea)
train$BsmtQual <- as.numeric(train$BsmtQual)
test$BsmtQual <- as.numeric(test$BsmtQual)
train$BsmtQual <- ifelse(is.na(train$BsmtQual), 5, train$BsmtQual)
test$BsmtQual <- ifelse(is.na(test$BsmtQual), 5, test$BsmtQual)
train$BsmtQual <- as.factor(train$BsmtQual)
test$BsmtQual <- as.factor(test$BsmtQual)
train$BsmtCond <- NULL
test$BsmtCond <- NULL
train$BsmtExposure <- as.numeric(train$BsmtExposure)
test$BsmtExposure <- as.numeric(test$BsmtExposure)
train$BsmtExposure <- ifelse(is.na(train$BsmtExposure), 5, train$BsmtExposure)
test$BsmtExposure <- ifelse(is.na(test$BsmtExposure), 5, test$BsmtExposure)
train$BsmtExposure <- as.factor(train$BsmtExposure)
test$BsmtExposure <- as.factor(test$BsmtExposure)
train$BsmtFinType1 <- as.numeric(train$BsmtFinType1)
test$BsmtFinType1 <- as.numeric(test$BsmtFinType1)
train$BsmtFinType1 <- ifelse(is.na(train$BsmtFinType1), 7, train$BsmtFinType1)
test$BsmtFinType1 <- ifelse(is.na(test$BsmtFinType1), 7, test$BsmtFinType1)
train$BsmtFinType1 <- as.factor(train$BsmtFinType1)
test$BsmtFinType1 <- as.factor(test$BsmtFinType1)
test$BsmtFinSF1 <- ifelse(is.na(test$BsmtFinSF1), 351, test$BsmtFinSF1)
train$BsmtFinType2 <- as.numeric(train$BsmtFinType2)
test$BsmtFinType2 <- as.numeric(test$BsmtFinType2)
train$BsmtFinType2 <- ifelse(is.na(train$BsmtFinType2), 7, train$BsmtFinType2)
test$BsmtFinType2 <- ifelse(is.na(test$BsmtFinType2), 7, test$BsmtFinType2)
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)
test$BsmtFinType2 <- as.factor(test$BsmtFinType2)
test$BsmtFinSF2 <- ifelse(is.na(test$BsmtFinSF2), 0, test$BsmtFinSF2)
test$BsmtUnfSF <- ifelse(is.na(test$BsmtUnfSF), 460, test$BsmtUnfSF)
test$TotalBsmtSF <- ifelse(is.na(test$TotalBsmtSF), 988, test$TotalBsmtSF)
train$Heating <- NULL
test$Heating <- NULL
train$CentralAir <- NULL
test$CentralAir <- NULL
train$Electrical <- NULL
test$Electrical <- NULL
train$X1stFlrSF <- ifelse(is.na(train$X1stFlrSF), 1086, train$X1stFlrSF)
train$X2ndFlrSF <- ifelse(is.na(train$X2ndFlrSF), 0, train$X2ndFlrSF)
train$LowQualFinSF <- ifelse(is.na(train$LowQualFinSF), 0, train$LowQualFinSF)
train$GrLivArea <- ifelse(is.na(train$GrLivArea), 1456, train$GrLivArea)
train$BsmtFullBath <- ifelse(is.na(train$BsmtFullBath), 0, train$BsmtFullBath)
train$BsmtFullBath <- as.factor(train$BsmtFullBath)
test$BsmtFullBath <- ifelse(is.na(test$BsmtFullBath), 0, test$BsmtFullBath)
test$BsmtFullBath <- as.factor(test$BsmtFullBath)
train$BsmtHalfBath <- ifelse(is.na(train$BsmtHalfBath), 0, train$BsmtHalfBath)
train$BsmtHalfBath <- as.factor(train$BsmtHalfBath)
test$BsmtHalfBath <- ifelse(is.na(test$BsmtHalfBath), 0, test$BsmtHalfBath)
test$BsmtHalfBath <- as.factor(test$BsmtHalfBath)
train$FullBath <- ifelse(is.na(train$FullBath), 2, train$FullBath)
test$FullBath <- ifelse(is.na(test$FullBath), 2, test$FullBath)
train$HalfBath <- ifelse(is.na(train$HalfBath), 0, train$HalfBath)
train$HalfBath <- as.factor(train$HalfBath)
test$HalfBath <- ifelse(is.na(test$HalfBath), 0, test$HalfBath)
test$HalfBath <- as.factor(test$HalfBath)
train$Bedroom <- ifelse(is.na(train$Bedroom), 3, train$Bedroom)
train$Bedroom <- as.factor(train$Bedroom)
test$Bedroom <- as.factor(test$Bedroom)
train$BedroomAbvGr <- ifelse(is.na(train$BedroomAbvGr), 3, train$BedroomAbvGr)
train$BedroomAbvGr <- as.factor(train$BedroomAbvGr)
test$BedroomAbvGr <- as.factor(test$BedroomAbvGr)
train$KitchenQual <- as.numeric(train$KitchenQual)
test$KitchenQual <- as.numeric(test$KitchenQual)
train$KitchenQual <- ifelse(is.na(train$KitchenQual), 4, train$KitchenQual)
test$KitchenQual <- ifelse(is.na(test$KitchenQual), 4, test$KitchenQual)
train$KitchenQual <- as.factor(train$KitchenQual)
test$KitchenQual <- as.factor(test$KitchenQual)
train$TotRmsAbvGrd <- ifelse(is.na(train$TotRmsAbvGrd), 6, train$TotRmsAbvGrd)
train$Functional <- NULL
test$Functional <- NULL
train$FireplaceQu <- as.numeric(train$FireplaceQu)
test$FireplaceQu <- as.numeric(test$FireplaceQu)
train$FireplaceQu <- ifelse(is.na(train$FireplaceQu), 6, train$FireplaceQu)
test$FireplaceQu <- ifelse(is.na(test$FireplaceQu), 6, test$FireplaceQu)
train$FireplaceQu <- as.factor(train$FireplaceQu)
test$FireplaceQu <- as.factor(test$FireplaceQu)
train$GarageType <- as.numeric(train$GarageType)
test$GarageType <- as.numeric(test$GarageType)
train$GarageType <- ifelse(is.na(train$GarageType), 7, train$GarageType)
test$GarageType <- ifelse(is.na(test$GarageType), 7, test$GarageType)
train$GarageType <- as.factor(train$GarageType)
test$GarageType <- as.factor(test$GarageType)
train$GarageYrBlt <- ifelse(is.na(train$GarageYrBlt), 1980, train$GarageYrBlt)
test$GarageYrBlt <- ifelse(is.na(test$GarageYrBlt), 1979, test$GarageYrBlt)
train$GarageFinish <- as.numeric(train$GarageFinish)
test$GarageFinish <- as.numeric(test$GarageFinish)
train$GarageFinish <- ifelse(is.na(train$GarageFinish), 4, train$GarageFinish)
test$GarageFinish <- ifelse(is.na(test$GarageFinish), 4, test$GarageFinish)
train$GarageFinish <- as.factor(train$GarageFinish)
test$GarageFinish <- as.factor(test$GarageFinish)
test$GarageCars <- ifelse(is.na(test$GarageCars), 2, test$GarageCars)
test$GarageArea <- ifelse(is.na(test$GarageArea), 0, test$GarageArea)
train$GarageQual <- NULL
test$GarageQual <- NULL
train$GarageCond <- NULL
test$GarageCond <- NULL
train$PavedDrive <- NULL
test$PavedDrive <- NULL
train$X3SsnPorch <- NULL
test$X3SsnPorch <- NULL
train$PoolArea <- NULL
test$PoolArea <- NULL
train$PoolQC <- NULL
test$PoolQC <- NULL
train$Fence <- as.numeric(train$Fence)
test$Fence <- as.numeric(test$Fence)
train$Fence <- ifelse(is.na(train$Fence), 5, train$Fence)
test$Fence <- ifelse(is.na(test$Fence), 5, test$Fence)
train$Fence <- as.factor(train$Fence)
test$Fence <- as.factor(test$Fence)
train$MiscFeature <- NULL
test$MiscFeature <- NULL
train$MiscVal <- NULL
test$MiscVal <- NULL
train$MoSold <- as.factor(train$MoSold)
test$MoSold <- as.factor(test$MoSold)
train$YrSold <- as.factor(train$YrSold)
test$YrSold <- as.factor(test$YrSold)
train$SaleType <- as.numeric(train$SaleType)
test$SaleType <- as.numeric(test$SaleType)
test$SaleType <- ifelse(is.na(test$SaleType), 9, test$SaleType)
train$SaleType <- as.factor(train$SaleType)
test$SaleType <- as.factor(test$SaleType)

train$Condition2 <- as.integer(train$Condition2)
test$Condition2 <- as.integer(test$Condition2)
train$OverallQual <- as.integer(train$OverallQual)
test$OverallQual <- as.integer(test$OverallQual)
train$OverallCond <- as.integer(train$OverallCond)
test$OverallCond <- as.integer(test$OverallCond)
train$RoofStyle <- as.integer(train$RoofStyle)
test$RoofStyle <- as.integer(test$RoofStyle)
train$Exterior1st <- as.integer(train$Exterior1st)
test$Exterior1st <- as.integer(test$Exterior1st)
train$ExterCond <- as.integer(train$ExterCond)
test$ExterCond <- as.integer(test$ExterCond)
train$HeatingQC <- as.integer(train$HeatingQC)
test$HeatingQC <- as.integer(test$HeatingQC)
mn <- mean(train$SalePrice)
sn <- sd(train$SalePrice)
train <- train[train$SalePrice <= mn + sn & train$SalePrice >= mn - sn,]

fit <- lm(SalePrice ~ ., train)
summary(fit)
fit_best <- step(fit)
summary(fit_best)

###################################################################################

prices <- predict(fit_best, test)
answer <- data.frame(1461:2919, prices)

colnames(answer) <- c('Id', 'SalePrice')
write.csv(answer, 'ans_best4.csv', row.names = F)

###################################################################################
facs <- train[,sapply(train, is.factor)]
str(train)

train$Condition2 <- NULL
test$Condition2 <- NULL
train$HouseStyle <- NULL
test$HouseStyle <- NULL
train$OverallQual <- as.factor(train$OverallQual)
train$OverallCond <- as.factor(train$OverallCond)

head(train$Alley, 30)
levels(train$LotFrontage)
sum(is.na(train$LotFrontage))
range(train$LotArea)
