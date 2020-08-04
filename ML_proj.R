library(readr)
library(tidyverse)
data <- read_csv("./train.csv")
colnames(data)
sum(is.na(data))
apply(is.na(data), 2, which)
colnames(data)[colSums(is.na(data)) > 0]
na_colnames <- colnames(data)[colSums(is.na(data)) > 0]
na_colnames
data$PoolQC[is.na(data$PoolQC)] <- "None"
unique(data$PoolQC)
data$Fence[is.na(data$Fence)] <- "None"
data$Alley[is.na(data$Alley)] <- "None"
data$BsmtQual[is.na(data$BsmtQual)] <- "None"
data$BsmtCond[is.na(data$BsmtCond)] <- "None"
data$BsmtExposure[is.na(data$BsmtExposure)] <- "None"
data$BsmtFinType1[is.na(data$BsmtFinType1)] <- "None"
data$BsmtFinType2[is.na(data$BsmtFinType2)] <- "None"
data$FireplaceQu[is.na(data$FireplaceQu)] <- "None"
data$GarageType[is.na(data$GarageType)] <- "None"
data$GarageFinish[is.na(data$GarageFinish)] <- "None" 
data$GarageQual[is.na(data$GarageQual)] <- "None"
data$GarageCond[is.na(data$GarageCond)] <- "None"
data$MiscFeature[is.na(data$MiscFeature)] <- "None"

sum(is.na(data$GarageYrBlt))
sum(data$GarageType == 'None')

data$LotFrontage[is.na(data$LotFrontage)] <- 0

which(is.na(data$GarageYrBlt))
which(data$GarageType == "None")

head(data$MasVnrArea)
which(is.na(data$MasVnrArea))

data$MasVnrType[is.na(data$MasVnrType)] <- "None"
data$MasVnrArea[is.na(data$MasVnrArea)] <- 0

data$GarageYrBlt[is.na(data$GarageYrBlt)] <- 0
which(is.na(data$Electrical))

data <- na.omit(data)




