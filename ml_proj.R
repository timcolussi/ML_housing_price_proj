setwd("~/NYCDSA/ML_project/ML_housing_price_proj")
library(readr)
housing_data_raw <- read_csv("./train.csv")



library(tidyverse)
library(ggplot2)

perc_missingness <- function(x) {
  data.frame("Percent.Missing" = floor(apply(is.na(x), 2, sum)/nrow(x)*100))
}

missing <- perc_missingness(housing_data_raw)
missing$Column.Names <- row.names(missing)
missing <- missing[missing$Percent.Missing > 0, ]

# plotting the missingness

ggplot(missing, aes(x = reorder(Column.Names, Percent.Missing), y = Percent.Missing)) +
  geom_bar(stat="identity") + 
  coord_flip(ylim = c(4,100)) + 
  labs(title = "Percent of Data Missing for each Feature",
       x = "Feature",
       y = "Percent Missing") +
  theme_bw()

# Three features did not show up because % was less than 1

housing_data <- housing_data_raw

housing_data$TotPorchSF <- housing_data$`3SsnPorch` + housing_data$OpenPorchSF + 
  housing_data$ScreenPorch + housing_data$EnclosedPorch

# adding a column for percent of basement that is finished
housing_data$BsmtPercFin <- (housing_data$BsmtFinSF1 +
                               housing_data$BsmtFinSF2)/housing_data$TotalBsmtSF

# added TotSF and TotSFAbvGrd columns 
housing_data <- housing_data %>% 
  rename(FrstFlrSF = `1stFlrSF`,
         ScndFlrSF = `2ndFlrSF`)
housing_data$TotSF <- housing_data$GrLivArea  + housing_data$TotalBsmtSF

# adding columns for TotFullBath and TotHalfBath
housing_data$TotFullBath <- housing_data$FullBath + housing_data$BsmtFullBath
housing_data$TotHalfBath <- housing_data$HalfBath + housing_data$BsmtHalfBath

# removing some columns
housing_data <- housing_data %>% 
  rename(ThrScn = `3SsnPorch`)
columns_to_drop <- c("PoolQC", "MiscFeature", "MiscValue", "GarageYrBlt",
                     "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "BsmtFinSF2", "FrstFlrSF",
                     "ScndFlrSF", "BsmtUnfSF", "FullBath", "HalfBath", "BsmtFullBath",
                     "BsmtHalfBath", "OpenPorchSF", "ThrScn", "ScreenPorch", "EnclosedPorch")
housing_data[, columns_to_drop] <- list(NULL)

# imputing NAs with "None" or 0
library(data.table)

columns_to_convert_na_none <- c("Fence", "FireplaceQu", "GarageType", "GarageFinish", "GarageCond",
                                "MasVnrType", "Alley", "BsmtQual", "BsmtCond", "BsmtExposure", 
                                "GarageQual", "Garage")

columns_to_convert_na_zero <- c("MasVnrArea", "BsmtPercFin", "PoolArea")

for(k in names(housing_data)){
  if(k %in% columns_to_convert_na_none){
    set(x = housing_data, which(is.na(housing_data[[k]])), k, "None")
  }
  else if(k %in% columns_to_convert_na_zero){
    set(x = housing_data, which(is.na(housing_data[[k]])), k, 0)
  }
}

# imputing LotFrontage
housing_data <- housing_data %>% 
  group_by(Neighborhood, LotConfig) %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), mean(LotFrontage, na.rm = TRUE), LotFrontage))
nwames <- housing_data %>% 
  filter(Neighborhood == 'NWAmes')
mean_lot_nwames <- mean(nwames$LotFrontage, na.rm = TRUE)
crawfor <- housing_data %>% 
  filter(Neighborhood == 'Crawfor')
mean_lot_crawfor <- mean(crawfor$LotFrontage, na.rm = TRUE)
nwames_na_idx <- c(365, 422, 612)
crawfor_na_idx <- c(530, 1162)
housing_data[nwames_na_idx, "LotFrontage"] <- mean_lot_nwames
housing_data[crawfor_na_idx, "LotFrontage"] <- mean_lot_crawfor

# imputing Electrical
which(is.na(housing_data$Electrical))
housing_data[1380, c('Neighborhood', 'Electrical')]
timber <- housing_data %>% 
  filter(Neighborhood == 'Timber')
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_elec_timber <- getmode(timber$Electrical)
housing_data[1380, "Electrical"] <- mode_elec_timber

names(housing_data)

more_columns_to_drop <- c("LotShape", "Id", "LandContour", "BsmtQual", "GarageArea")
housing_data[, more_columns_to_drop] <- list(NULL)


ggplot(housing_data, aes(x = SalePrice)) + geom_histogram(bins = 100) +
  scale_x_continuous(labels=scales::dollar_format()) +
  labs(title = "Frequency of Sale Price",
       x = "Sale Price",
       y = "Freqeuncy")

target <- log(housing_data$SalePrice)
target_df <- data.frame(target)

ggplot(target_df, aes(x = target)) + geom_histogram(bins = 100) +
  labs(title = "Frequency of Sale Price",
       x = "Log of Sale Price",
       y = "Freqeuncy")

ggplot(housing_data, aes(x = SalePrice)) + geom_boxplot() + 
  scale_x_continuous(labels=scales::dollar_format())

## figuring out categoricals with low instances
which(sapply(housing_data, is.character))
table(housing_data$SaleCondition)
## combining FR2 and FR3 to FR
housing_data$LotConfig <- ifelse(housing_data$LotConfig == "FR2" | housing_data$LotConfig == "FR3",
                                 "FR", housing_data$LotConfig)
## can't remove neighborhood so split on this
## altering RR and Pos categories in Cond1 
housing_data$Condition1 <- ifelse(housing_data$Condition1 == "RRAe" | 
                                    housing_data$Condition1 == "RRAn" |
                                    housing_data$Condition1 == "RRNe" |
                                    housing_data$Condition1 == "RRNn", "RR", 
                                  housing_data$Condition1)
housing_data$Condition1 <- ifelse(housing_data$Condition1 == "PosA" |
                                    housing_data$Condition1 == "PosN", "Pos",
                                  housing_data$Condition1)
## dropping Cond2, mostly norm
## grouping rare roofstyle as other
housing_data$RoofStyle <- ifelse(housing_data$RoofStyle == "Flat" |
                                   housing_data$RoofStyle == "Gambrel" |
                                   housing_data$RoofStyle == "Mansard" |
                                   housing_data$RoofStyle == "Shed", "Other",
                                 housing_data$RoofStyle)
## changing roofmatl to either CompShg or not
housing_data$RoofMatl <- ifelse(housing_data$RoofMatl != "CompShg", "Other", housing_data$RoofMatl)
## Exterior1 to Shingles, Siding, Brick, or Other
#housing_data$Exterior1st <- housing_data_raw$Exterior1st
housing_data$Exterior1st <- ifelse(housing_data$Exterior1st == "AsbShng" |
                                     housing_data$Exterior1st == "AsphShn" |
                                     housing_data$Exterior1st == "WdShing", "Shingles",
                                   housing_data$Exterior1st)
housing_data$Exterior1st <- ifelse(housing_data$Exterior1st == "MetalSd" |
                                     housing_data$Exterior1st == "VinylSd" |
                                     housing_data$Exterior1st == "Wd Sdng", "Siding",
                                   housing_data$Exterior1st)
housing_data$Exterior1st <- ifelse(housing_data$Exterior1st == "BrkComm" |
                                     housing_data$Exterior1st == "BrkFace", "Brick",
                                   housing_data$Exterior1st)
housing_data$Exterior1st <- ifelse(housing_data$Exterior1st == "Brick" |
                                     housing_data$Exterior1st == "Shingles" |
                                     housing_data$Exterior1st == "Siding", 
                                   housing_data$Exterior1st, "Other")
## same for Exterior 2
housing_data$Exterior2nd <- ifelse(housing_data$Exterior2nd == "AsbShng" |
                                     housing_data$Exterior2nd == "AsphShn" |
                                     housing_data$Exterior2nd == "WdShing", "Shingles",
                                   housing_data$Exterior2nd)
housing_data$Exterior2nd <- ifelse(housing_data$Exterior2nd == "MetalSd" |
                                     housing_data$Exterior2nd == "VinylSd" |
                                     housing_data$Exterior2nd == "Wd Sdng", "Siding",
                                   housing_data$Exterior2nd)
housing_data$Exterior2nd <- ifelse(housing_data$Exterior2nd == "BrkComm" |
                                     housing_data$Exterior2nd == "BrkFace", "Brick",
                                   housing_data$Exterior2nd)
housing_data$Exterior2nd <- ifelse(housing_data$Exterior2nd == "Brick" |
                                     housing_data$Exterior2nd == "Shingles" |
                                     housing_data$Exterior2nd == "Siding", 
                                   housing_data$Exterior2nd, "Other")
## changing ExterCond variables to AbTA, TA, and BlTA
housing_data$ExterCond <- ifelse(housing_data$ExterCond == "Ex" |
                                   housing_data$ExterCond == "Gd", "AbTA",
                                 housing_data$ExterCond)
housing_data$ExterCond <- ifelse(housing_data$ExterCond == "Fa" |
                                   housing_data$ExterCond == "Po", "BlTA",
                                 housing_data$ExterCond)
## changed uncommon foundations to other
housing_data$Foundation <- ifelse(housing_data$Foundation == "Slab" |
                                    housing_data$Foundation == "Stone" |
                                    housing_data$Foundation == "Wood", "Other",
                                  housing_data$Foundation)
## grouped Fa and Po to BlTA for BsmtCond
housing_data$BsmtCond <- ifelse(housing_data$BsmtCond == "Fa" |
                                  housing_data$BsmtCond == "Po", "BlTA",
                                housing_data$BsmtCond)
## changed heatingQC to AbTA, TA, or BlTA
housing_data$HeatingQC <- ifelse(housing_data$HeatingQC == "Ex" | 
                                   housing_data$HeatingQC == "Gd", "AbTA",
                                 housing_data$HeatingQC)
housing_data$HeatingQC <- ifelse(housing_data$HeatingQC == "Fa" |
                                   housing_data$HeatingQC == "Po", "BlTA",
                                 housing_data$HeatingQC)
## change Functional to either Typ or Deduct
housing_data$Functional <- ifelse(housing_data$Functional == "Typ",
                                  housing_data$Functional, "Deduct")
## changing fireplaceQu to AbTA, TA, BlTA, and None
housing_data$FireplaceQu <- ifelse(housing_data$FireplaceQu == "Ex" |
                                     housing_data$FireplaceQu == "Gd", "AbTA",
                                   housing_data$FireplaceQu)
housing_data$FireplaceQu <- ifelse(housing_data$FireplaceQu == "Fa" |
                                     housing_data$FireplaceQu == "Po", "BlTA",
                                   housing_data$FireplaceQu)
## changing GarageQual to AbTA, TA, BlTA
housing_data$GarageQual <- ifelse(housing_data$GarageQual == "Ex" |
                                    housing_data$GarageQual == "Gd", "AbTA",
                                  housing_data$GarageQual)
housing_data$GarageQual <- ifelse(housing_data$GarageQual == "Fa" |
                                    housing_data$GarageQual == "Po", "BlTA",
                                  housing_data$GarageQual)
## changing GarageCond to AbTA, TA, BlTA
housing_data$GarageCond <- ifelse(housing_data$GarageCond == "Ex" |
                                    housing_data$GarageCond == "Gd", "AbTA",
                                  housing_data$GarageCond)
housing_data$GarageCond <- ifelse(housing_data$GarageCond == "Fa" |
                                    housing_data$GarageCond == "Po", "BlTA",
                                  housing_data$GarageCond)
## grouping SaleType into Contract, WarrDeed, New, Other
housing_data$SaleType <- ifelse(housing_data$SaleType == "Con" |
                                  housing_data$SaleType == "ConLD" |
                                  housing_data$SaleType == "ConLI" |
                                  housing_data$SaleType == "ConLw", "Contract",
                                housing_data$SaleType)
housing_data$SaleType <- ifelse(housing_data$SaleType == "CWD" |
                                  housing_data$SaleType == "WD", "WarrDeed",
                                housing_data$SaleType)
housing_data$SaleType <- ifelse(housing_data$SaleType == "COD" |
                                  housing_data$SaleType == "Oth", "Other",
                                housing_data$SaleType)
## grouped AdjLand, Alloca, Family as other for SaleCondition
housing_data$SaleCondition <- ifelse(housing_data$SaleCondition == "AdjLand" |
                                       housing_data$SaleCondition == "Alloca" |
                                       housing_data$SaleCondition == "Family", "Other",
                                     housing_data$SaleCondition)

to_drop <- c("Street", "Condition2", "Heating")
housing_data[, to_drop] <- NULL





modeling_data <- housing_data
modeling_data$LogSalePrice <- log(modeling_data$SalePrice)
modeling_data$SalePrice <- NULL

which(housing_data$SalePrice > 700000)
housing_data[692, ]
(max(housing_data$GrLivArea))

modeling_data <- modeling_data[-c(692,1183), ]

glimpse(modeling_data)

modeling_data[] <- lapply(modeling_data, function(x) if(is.character(x)) as.factor(x) else x)
modeling_data$MSSubClass <- as.factor(modeling_data$MSSubClass)

modeling_data$TotalBsmtSF <- NULL
modeling_data$GrLivArea <- NULL

# drop Utilities because they are all the same
modeling_data$Utilities <- NULL

## Multiple Linear Regression

library(caret)
set.seed(2187)
train.index <- createDataPartition(modeling_data$Neighborhood, p = 0.7, list = FALSE)

train2 <- modeling_data[train.index, ]
test2 <- modeling_data[-train.index, ]

mlr_model2 <- lm(LogSalePrice ~ ., data = train2)
summary(mlr_model2)
vif(mlr_model2)
ld.vars2 <- attributes(alias(mlr_model2)$Complete)$dimnames[[1]]
ld.vars2


test_target2 <- test2
test_target2$LogSalePrice <- NULL
mlr_model_pred2 <- predict(mlr_model2, test_target2, interval = "prediction")

plot(mlr_model2)

plot(mlr_model_pred2[,1], test2$LogSalePrice)
mlr_empty2 <- lm(LogSalePrice ~ 1, data = train2)
scope = list(lower = formula(mlr_empty2), upper = formula(mlr_model2))

library(MASS)
backwardAIC2 = step(mlr_model2, scope, direction = "backward", k = 2)
forwardAIC2 = step(mlr_empty2, scope, direction = "forward", k = 2)

anova(forwardAIC2, mlr_model2)

summary(mlr_model2)
summary(backwardAIC2)
pred_back2 <- predict(backwardAIC2, test_target2, interval = "prediction")
plot(pred_back2[,1], test2$LogSalePrice)

summary(backwardAIC2)$r.squared
summary(mlr_model2)$r.squared
summary(forwardAIC2)$r.squared
cor(mlr_model_pred2[,1], test2$LogSalePrice)^2
cor(pred_back2[,1], test2$LogSalePrice)^2

backwardAIC2$model

# full_data_pred <- predict(backwardAIC2, modeling_data, interval = "prediction")
# plot(full_data_pred[,1], modeling_data$LogSalePrice)
# cor(full_data_pred[,1], modeling_data$LogSalePrice)^2

summ <- summary(backwardAIC2)
best_model_formula <- summ[[1]]
best_form <- as.formula(best_model_formula)
best_mlr <- lm(best_form, data = modeling_data)
summary(best_mlr)$r.squared
plot(best_mlr$fitted.values, modeling_data$LogSalePrice)
cor(best_mlr$fitted.values, modeling_data$LogSalePrice)^2
summary(best_mlr)
best_form

##############################################################################################
## processing test.csv #######################################################################
##############################################################################################
unseen_data_raw <- read_csv("./test.csv") 
unseen_data <- unseen_data_raw

unseen_data$TotPorchSF <- unseen_data$`3SsnPorch` + unseen_data$OpenPorchSF + 
  unseen_data$ScreenPorch + unseen_data$EnclosedPorch

# adding a column for percent of basement that is finished
unseen_data$BsmtPercFin <- (unseen_data$BsmtFinSF1 +
                               unseen_data$BsmtFinSF2)/unseen_data$TotalBsmtSF

# added TotSF and TotSFAbvGrd columns 
unseen_data <- unseen_data %>% 
  rename(FrstFlrSF = `1stFlrSF`,
         ScndFlrSF = `2ndFlrSF`)
unseen_data$TotSF <- unseen_data$GrLivArea  + unseen_data$TotalBsmtSF

# adding columns for TotFullBath and TotHalfBath
unseen_data$TotFullBath <- unseen_data$FullBath + unseen_data$BsmtFullBath
unseen_data$TotHalfBath <- unseen_data$HalfBath + unseen_data$BsmtHalfBath

# removing some columns
unseen_data <- unseen_data %>% 
  rename(ThrScn = `3SsnPorch`)
columns_to_drop <- c("PoolQC", "MiscFeature", "MiscValue", "GarageYrBlt",
                     "BsmtFinType1", "BsmtFinSF1", "BsmtFinType2", "BsmtFinSF2", "FrstFlrSF",
                     "ScndFlrSF", "BsmtUnfSF", "FullBath", "HalfBath", "BsmtFullBath",
                     "BsmtHalfBath", "OpenPorchSF", "ThrScn", "ScreenPorch", "EnclosedPorch")
unseen_data[, columns_to_drop] <- list(NULL)

# imputing NAs with "None" or 0
library(data.table)

columns_to_convert_na_none <- c("Fence", "FireplaceQu", "GarageType", "GarageFinish", "GarageCond",
                                "MasVnrType", "Alley", "BsmtQual", "BsmtCond", "BsmtExposure", 
                                "GarageQual", "Garage")

columns_to_convert_na_zero <- c("MasVnrArea", "BsmtPercFin", "PoolArea")

for(k in names(unseen_data)){
  if(k %in% columns_to_convert_na_none){
    set(x = unseen_data, which(is.na(unseen_data[[k]])), k, "None")
  }
  else if(k %in% columns_to_convert_na_zero){
    set(x = unseen_data, which(is.na(unseen_data[[k]])), k, 0)
  }
}
##
unseen_missing <- perc_missingness(unseen_data)
unseen_missing$Column.Names <- row.names(unseen_missing)
unseen_missing <- unseen_missing[unseen_missing$Percent.Missing > 0, ]
unseen_missing
sum(is.na(unseen_data$LotFrontage))

unseen_data <- unseen_data %>% 
  group_by(Neighborhood, LotConfig) %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage), mean(LotFrontage, na.rm = TRUE), LotFrontage))
na_LF <- which(is.na(unseen_data$LotFrontage)) 
unseen_data[na_LF, c("LotFrontage", "LotConfig", "Neighborhood")] 

gilbert <- unseen_data %>% 
  filter(Neighborhood == "Gilbert")
mn_lf_gilbert <- round(mean(gilbert$LotFrontage, na.rm = TRUE), digits = 2)
noridge <- unseen_data %>% 
  filter(Neighborhood == "NoRidge")
mn_lf_noridge <- round(mean(noridge$LotFrontage, na.rm = TRUE), digits = 2)
na_LF
unseen_data[1218, "Neighborhood"]
idx_gilbert <- c(482, 1218)
idx_nwames <- c(498, 1175)
idx_noridge <- c(537)
unseen_data[idx_gilbert, "LotFrontage"] <- mn_lf_gilbert
unseen_data[idx_nwames, "LotFrontage"] <- mean_lot_nwames
unseen_data[idx_noridge, "LotFrontage"] <- mn_lf_noridge
sum(is.na(unseen_data$LotFrontage))
sum(is.na(unseen_data))

summary(unseen_data)
num_missing <- function(x) {
  data.frame("Number.Missing" = apply(is.na(x), 2, sum))
}
unseen_missing2 <- num_missing(unseen_data)
unseen_missing2$Column.names <- row.names(unseen_missing2)
unseen_missing2 <- unseen_missing2[unseen_missing2$Number.Missing > 0, ]
unseen_missing2
which(!complete.cases(unseen_data))

which(is.na(unseen_data$TotSF))

unseen_data[661, c("GrLivArea", "TotalBsmtSF", "TotFullBath", "TotHalfBath")]
unseen_data_raw[661, c("FullBath", "HalfBath", "BsmtFullBath", "BsmtHalfBath")]
unseen_data[661, "TotSF"] <- unseen_data_raw[661, "GrLivArea"]
unseen_data[661, "TotFullBath"] <- unseen_data_raw[661, "FullBath"]
unseen_data[661, "TotHalfBath"] <- unseen_data_raw[661, "HalfBath"]
mszon_na <- which(is.na(unseen_data$MSZoning))
unseen_data[mszon_na, c("Neighborhood", "SaleType")]

idotrr <- unseen_data %>% 
  filter(Neighborhood == "IDOTRR")
id_mode_zone <- getmode(idotrr$MSZoning)
mitchel <- unseen_data %>% 
  filter(Neighborhood == "Mitchel")
mitch_mode_zone <- getmode(mitchel$MSZoning)
unseen_data[791, "Neighborhood"]
idx_mitchel <- c(456, 757, 791)
unseen_data[idx_mitchel, "MSZoning"] <- id_mode_zone
unseen_data[1445, "MSZoning"] <- mitch_mode_zone  

which(is.na(unseen_data$TotHalfBath))
unseen_data_raw[729, c("FullBath", "HalfBath", "BsmtFullBath", "BsmtHalfBath")]
unseen_data[729, "TotFullBath"] <- unseen_data_raw[729, "FullBath"]
unseen_data[729, "TotHalfBath"] <- unseen_data_raw[729, "HalfBath"]

getmode(unseen_data$Exterior1st)

which(is.na(unseen_data$GarageCars))
unseen_data[1117, c("GarageArea", "GarageCars", "GarageType")]

unseen_data2 <- unseen_data
column_to_mode <- c("Exterior1st")
which(is.na(unseen_data$Exterior1st))

columns_to_mode <- c("Exterior1st", "Exterior2nd", "KitchenQual", "Functional", "SaleType")

for(k in names(unseen_data)){
  if(k %in% columns_to_mode){
    set(x = unseen_data, which(is.na(unseen_data[[k]])), k, getmode(unseen_data[[k]]))
  }
}

unseen_data %>% 
  group_by(GarageType) %>% 
  summarise(ave_cars = mean(GarageCars, na.rm = TRUE),
            ave_area = mean(GarageArea, na.rm = TRUE))
which(is.na(unseen_data$GarageCars))
unseen_data[1117, "GarageCars"] <- 1.5
unseen_data[1117, "GarageArea"] <- 412

to_drop <- c("Street", "Condition2", "Heating", "Utilities", "TotalBsmtSF", "GrLivArea",
             "LotShape", "Id", "LandContour", "BsmtQual", "GarageArea")

unseen_data[, to_drop] <- NULL



unique(modeling_data$MSSubClass)
unique(unseen_data$MSSubClass)
setdiff(modeling_data$MSSubClass, unseen_data$MSSubClass)

which(unseen_data$MSSubClass == 150)
unseen_data[1359, "MSSubClass"] <- as.factor(160)
class(unseen_data$MSSubClass)

## combining FR2 and FR3 to FR
unseen_data$LotConfig <- ifelse(unseen_data$LotConfig == "FR2" | unseen_data$LotConfig == "FR3",
                                 "FR", unseen_data$LotConfig)
## can't remove neighborhood so split on this
## altering RR and Pos categories in Cond1 
unseen_data$Condition1 <- ifelse(unseen_data$Condition1 == "RRAe" | 
                                    unseen_data$Condition1 == "RRAn" |
                                    unseen_data$Condition1 == "RRNe" |
                                    unseen_data$Condition1 == "RRNn", "RR", 
                                  unseen_data$Condition1)
unseen_data$Condition1 <- ifelse(unseen_data$Condition1 == "PosA" |
                                    unseen_data$Condition1 == "PosN", "Pos",
                                  unseen_data$Condition1)
## dropping Cond2, mostly norm
## grouping rare roofstyle as other
unseen_data$RoofStyle <- ifelse(unseen_data$RoofStyle == "Flat" |
                                   unseen_data$RoofStyle == "Gambrel" |
                                   unseen_data$RoofStyle == "Mansard" |
                                   unseen_data$RoofStyle == "Shed", "Other",
                                 unseen_data$RoofStyle)
## changing roofmatl to either CompShg or not
unseen_data$RoofMatl <- ifelse(unseen_data$RoofMatl != "CompShg", "Other", unseen_data$RoofMatl)
## Exterior1 to Shingles, Siding, Brick, or Other
#housing_data$Exterior1st <- housing_data_raw$Exterior1st
unseen_data$Exterior1st <- ifelse(unseen_data$Exterior1st == "AsbShng" |
                                     unseen_data$Exterior1st == "AsphShn" |
                                     unseen_data$Exterior1st == "WdShing", "Shingles",
                                   unseen_data$Exterior1st)
unseen_data$Exterior1st <- ifelse(unseen_data$Exterior1st == "MetalSd" |
                                     unseen_data$Exterior1st == "VinylSd" |
                                     unseen_data$Exterior1st == "Wd Sdng", "Siding",
                                   unseen_data$Exterior1st)
unseen_data$Exterior1st <- ifelse(unseen_data$Exterior1st == "BrkComm" |
                                     unseen_data$Exterior1st == "BrkFace", "Brick",
                                   unseen_data$Exterior1st)
unseen_data$Exterior1st <- ifelse(unseen_data$Exterior1st == "Brick" |
                                     unseen_data$Exterior1st == "Shingles" |
                                     unseen_data$Exterior1st == "Siding", 
                                   unseen_data$Exterior1st, "Other")
## same for Exterior 2
unseen_data$Exterior2nd <- ifelse(unseen_data$Exterior2nd == "AsbShng" |
                                     unseen_data$Exterior2nd == "AsphShn" |
                                     unseen_data$Exterior2nd == "WdShing", "Shingles",
                                   unseen_data$Exterior2nd)
unseen_data$Exterior2nd <- ifelse(unseen_data$Exterior2nd == "MetalSd" |
                                     unseen_data$Exterior2nd == "VinylSd" |
                                     unseen_data$Exterior2nd == "Wd Sdng", "Siding",
                                   unseen_data$Exterior2nd)
unseen_data$Exterior2nd <- ifelse(unseen_data$Exterior2nd == "BrkComm" |
                                     unseen_data$Exterior2nd == "BrkFace", "Brick",
                                   unseen_data$Exterior2nd)
unseen_data$Exterior2nd <- ifelse(unseen_data$Exterior2nd == "Brick" |
                                     unseen_data$Exterior2nd == "Shingles" |
                                     unseen_data$Exterior2nd == "Siding", 
                                   unseen_data$Exterior2nd, "Other")
## changing ExterCond variables to AbTA, TA, and BlTA
unseen_data$ExterCond <- ifelse(unseen_data$ExterCond == "Ex" |
                                   unseen_data$ExterCond == "Gd", "AbTA",
                                 unseen_data$ExterCond)
unseen_data$ExterCond <- ifelse(unseen_data$ExterCond == "Fa" |
                                   unseen_data$ExterCond == "Po", "BlTA",
                                 unseen_data$ExterCond)
## changed uncommon foundations to other
unseen_data$Foundation <- ifelse(unseen_data$Foundation == "Slab" |
                                    unseen_data$Foundation == "Stone" |
                                    unseen_data$Foundation == "Wood", "Other",
                                  unseen_data$Foundation)
## grouped Fa and Po to BlTA for BsmtCond
unseen_data$BsmtCond <- ifelse(unseen_data$BsmtCond == "Fa" |
                                  unseen_data$BsmtCond == "Po", "BlTA",
                                unseen_data$BsmtCond)
## changed heatingQC to AbTA, TA, or BlTA
unseen_data$HeatingQC <- ifelse(unseen_data$HeatingQC == "Ex" | 
                                   unseen_data$HeatingQC == "Gd", "AbTA",
                                 unseen_data$HeatingQC)
unseen_data$HeatingQC <- ifelse(unseen_data$HeatingQC == "Fa" |
                                   unseen_data$HeatingQC == "Po", "BlTA",
                                 unseen_data$HeatingQC)
## change Functional to either Typ or Deduct
unseen_data$Functional <- ifelse(unseen_data$Functional == "Typ",
                                  unseen_data$Functional, "Deduct")
## changing fireplaceQu to AbTA, TA, BlTA, and None
unseen_data$FireplaceQu <- ifelse(unseen_data$FireplaceQu == "Ex" |
                                     unseen_data$FireplaceQu == "Gd", "AbTA",
                                   unseen_data$FireplaceQu)
unseen_data$FireplaceQu <- ifelse(unseen_data$FireplaceQu == "Fa" |
                                     unseen_data$FireplaceQu == "Po", "BlTA",
                                   unseen_data$FireplaceQu)
## changing GarageQual to AbTA, TA, BlTA
unseen_data$GarageQual <- ifelse(unseen_data$GarageQual == "Ex" |
                                    unseen_data$GarageQual == "Gd", "AbTA",
                                  unseen_data$GarageQual)
unseen_data$GarageQual <- ifelse(unseen_data$GarageQual == "Fa" |
                                    unseen_data$GarageQual == "Po", "BlTA",
                                  unseen_data$GarageQual)
## changing GarageCond to AbTA, TA, BlTA
unseen_data$GarageCond <- ifelse(unseen_data$GarageCond == "Ex" |
                                    unseen_data$GarageCond == "Gd", "AbTA",
                                  unseen_data$GarageCond)
unseen_data$GarageCond <- ifelse(unseen_data$GarageCond == "Fa" |
                                    unseen_data$GarageCond == "Po", "BlTA",
                                  unseen_data$GarageCond)
## grouping SaleType into Contract, WarrDeed, New, Other
unseen_data$SaleType <- ifelse(unseen_data$SaleType == "Con" |
                                  unseen_data$SaleType == "ConLD" |
                                  unseen_data$SaleType == "ConLI" |
                                  unseen_data$SaleType == "ConLw", "Contract",
                                unseen_data$SaleType)
unseen_data$SaleType <- ifelse(unseen_data$SaleType == "CWD" |
                                  unseen_data$SaleType == "WD", "WarrDeed",
                                unseen_data$SaleType)
unseen_data$SaleType <- ifelse(unseen_data$SaleType == "COD" |
                                  unseen_data$SaleType == "Oth", "Other",
                                unseen_data$SaleType)
## grouped AdjLand, Alloca, Family as other for SaleCondition
unseen_data$SaleCondition <- ifelse(unseen_data$SaleCondition == "AdjLand" |
                                       unseen_data$SaleCondition == "Alloca" |
                                       unseen_data$SaleCondition == "Family", "Other",
                                     unseen_data$SaleCondition)

unseen_data[] <- lapply(unseen_data, function(x) if(is.character(x)) as.factor(x) else x)
unseen_data$MSSubClass <- as.factor(unseen_data$MSSubClass)


mlr_unseen_pred <- predict(best_mlr, unseen_data, interval = "prediction")
summary(mlr_unseen_pred)
mlr_unseen_pred[,1]

mlr_pred <- data.frame("Id" = unseen_data_raw$Id, "SalePrice" = exp(mlr_unseen_pred[,1]))
mlr_pred$SalePrice  <- round(mlr_pred$SalePrice, digits = 0)
head(mlr_pred)

write.csv(mlr_pred, "mlr_pred.csv", row.names = FALSE)
mlr_kaggle_score <- 0.13701

#############################################################################################
################ Ridge Regression ###########################################################
#############################################################################################
# train2, test2, target2, best_mlr
grid = 10^seq(5, -2, length = 100)
library(glmnet)
library(caret)

x = model.matrix(LogSalePrice ~ ., train2)[, -1] #Dropping the intercept column.
y = train2$LogSalePrice


set.seed(2187)
cv_ridge = cv.glmnet(x, y, lambda = grid, alpha = 0, nfolds = 5)
plot(cv_ridge, main = "Ridge Regression\n")
best_lambda <- cv_ridge$lambda.min
log(cv_ridge$lambda.min)
best_ridge_fit <- cv_ridge$glmnet.fit
head(best_ridge_fit)
best_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_ridge)

train2_features <- train2
train2_features$LogSalePrice <- NULL
pred_ridge_train <- predict(best_ridge, s = best_lambda, newx = x)
actual_ridge_train <- train2$LogSalePrice
preds_ridge_train <- pred_ridge_train
rss_ridge_train <- sum((preds_ridge_train - actual_ridge_train) ^ 2)
tss_ridge_train <- sum((actual_ridge_train - mean(actual_ridge_train)) ^ 2)
rsq_ridge_train <- 1 - rss_ridge_train/tss_ridge_train
rsq_ridge_train
x_test <- model.matrix(LogSalePrice ~ ., test2)[, -1]
names(test2)
preds_ridge_test <- predict(best_ridge, s = best_lambda, newx = x_test)
actual_ridge_test <- test2$LogSalePrice
rss_ridge_test <- sum((preds_ridge_test - actual_ridge_test) ^ 2)
tss_ridge_test <- sum((actual_ridge_test - mean(actual_ridge_test)) ^ 2)
rsq_ridge_test <- 1 - rss_ridge_test/tss_ridge_test
rsq_ridge_test


library(fastDummies)
unseen_data_dummy <- dummy_cols(unseen_data)
newx_unseen <- unseen_data
newx_unseen$LogSalePrice <- 0
newx_unseen <- model.matrix(LogSalePrice ~ ., newx_unseen)[, -1]
pred_ridge_unseen <- predict(best_ridge, s = best_lambda, newx = newx_unseen)

length(levels(unseen_data$MSSubClass))
housing_data_dummy <- dummy_cols(housing_data)

glimpse(unseen_data)
sum(is.na(unseen_data))

unseen_fac <- lapply(unseen_data, function(x) if(is.factor(x)) length(levels(x)))
model_fac <- lapply(modeling_data, function(x) if(is.factor(x)) length(levels(x)))
cbind(unseen_fac, model_fac)
fac_level
levels(unseen_data$MSSubClass)
table(unseen_data$MSSubClass)

unseen_data2 <- unseen_data
unseen_fac2 <- lapply(unseen_data2, function(x) if(is.factor(x)) length(levels(x)))
droplevels(unseen_data2)
unseen_fac3 <- lapply(unseen_data2, function(x) if(is.factor(x)) length(levels(x)))
fac_level <- cbind(fac_level, unseen_fac2, unseen_fac3)
fac_level

levels(modeling_data$HouseStyle)
levels(unseen_data$HouseStyle)
levels(unseen_data2$MSSubClass)

table(unseen_data2$MSSubClass)
droplevels(unseen_data2$MSSubClass)
levels(modeling_data$Electrical)
levels(unseen_data$Electrical)
unseen_data$Electrical <- factor(unseen_data$Electrical, 
                                  levels = c(levels(unseen_data$Electrical), "Mix"))
levels(unseen_data2$Electrical)
table(unseen_data2$Electrical)
names(unseen_data)
unseen_data$HouseStyle <- factor(unseen_data$HouseStyle, 
                                  levels = c(levels(unseen_data$HouseStyle), "2.5Fin"))
levels(unseen_data2$HouseStyle)
unseen_data <- droplevels(unseen_data)
levels(unseen_data$MSSubClass)

pred_ridge_unseen[,1]


ridge_pred <- data.frame("Id" = unseen_data_raw$Id, "SalePrice" = exp(pred_ridge_unseen[,1]))
ridge_pred$SalePrice  <- round(ridge_pred$SalePrice, digits = 0)
head(ridge_pred)

write.csv(ridge_pred, "ridge_pred.csv", row.names = FALSE)
mlr_kaggle_score <- 0.14332


# exp(mlr_unseen_pred[, 1])

# unseen_data$Condition1 <- unseen_data_raw$Condition1
# table(unseen_data$Condition1)
# class(unseen_data$Condition1)
# unseen_data$Condition1 <- as.factor(unseen_data$Condition1)

#############################################################################################
############### LASSO Regression ############################################################
#############################################################################################
# x, y, x_test, newx_unseen

set.seed(2187)
cv_lasso = cv.glmnet(x, y, lambda = grid, alpha = 1, nfolds = 5)
plot(cv_ridge, main = "LASSO Regression\n")
best_lambda_lasso <- cv_lasso$lambda.min
best_lambda_lasso
best_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda_lasso)
coef(best_lasso)

pred_lasso_train <- predict(best_lasso, s = best_lambda_lasso, newx = x)
actual_lasso_train <- train2$LogSalePrice
preds_lasso_train <- pred_lasso_train
rss_lasso_train <- sum((preds_lasso_train - actual_lasso_train) ^ 2)
tss_lasso_train <- sum((actual_lasso_train - mean(actual_lasso_train)) ^ 2)
rsq_lasso_train <- 1 - rss_lasso_train/tss_lasso_train
rsq_lasso_train

preds_lasso_test <- predict(best_lasso, s = best_lambda_lasso, newx = x_test)
actual_lasso_test <- test2$LogSalePrice
rss_lasso_test <- sum((preds_lasso_test - actual_lasso_test) ^ 2)
tss_lasso_test <- sum((actual_lasso_test - mean(actual_lasso_test)) ^ 2)
rsq_lasso_test <- 1 - rss_lasso_test/tss_lasso_test
rsq_lasso_test

pred_lasso_unseen <- predict(best_lasso, s = best_lambda_lasso, newx = newx_unseen)

lasso_pred <- data.frame("Id" = unseen_data_raw$Id, "SalePrice" = exp(pred_lasso_unseen[,1]))
lasso_pred$SalePrice  <- round(lasso_pred$SalePrice, digits = 0)
head(lasso_pred)

write.csv(lasso_pred, "lasso_pred.csv", row.names = FALSE)
mlr_kaggle_score <- 0.14403

#############################################################################################
#################### ElasticNet #############################################################
#############################################################################################

library(caret)
set.seed(2187)
train_control <- trainControl(method = 'cv', number = 5)
en_tune_grid <- expand.grid(lambda = grid, alpha = seq(0,1,by=0.1))
elastic_net_model <- train(x, y, method = 'glmnet', trControl = train_control,
                           tuneGrid = en_tune_grid)
elastic_net_model$bestTune
best_en_alpha <- elastic_net_model$bestTune[[1]]
best_en_lambda <- elastic_net_model$bestTune[[2]]
class(best_en_alpha)

best_elastic_net <- glmnet(x, y, alpha = best_en_alpha, lambda = best_en_lambda)
coef(best_elastic_net)

pred_en_train <- predict(best_elastic_net, newx = x)
actual_en_train <- train2$LogSalePrice
preds_en_train <- pred_en_train
rss_en_train <- sum((preds_en_train - actual_en_train) ^ 2)
tss_en_train <- sum((actual_en_train - mean(actual_en_train)) ^ 2)
rsq_en_train <- 1 - rss_en_train/tss_en_train
rsq_en_train

preds_en_test <- predict(best_elastic_net, newx = x_test)
actual_en_test <- test2$LogSalePrice
rss_en_test <- sum((preds_en_test - actual_en_test) ^ 2)
tss_en_test <- sum((actual_en_test - mean(actual_en_test)) ^ 2)
rsq_en_test <- 1 - rss_en_test/tss_en_test
rsq_en_test

x_mod <- model.matrix(LogSalePrice ~ ., modeling_data)[,-1]
y_mod <- modeling_data$LogSalePrice
best_en_alldata <- glmnet(x_mod, y_mod, alpha = best_en_alpha, lambda = best_en_lambda)

pred_en_unseen <- predict(best_en_alldata, newx = newx_unseen)

en_pred <- data.frame("Id" = unseen_data_raw$Id, "SalePrice" = exp(pred_en_unseen[,1]))
en_pred$SalePrice  <- round(en_pred$SalePrice, digits = 0)
head(en_pred)
class(pred_en_unseen)
write.csv(en_pred, "en_pred.csv", row.names = FALSE)
mlr_kaggle_score <- 0.13705

#############################################################################################
########### Random Forest ###################################################################
#############################################################################################
library(randomForest)
set.seed(2187)
oob.err = numeric(25)
for (mtry in 1:25) {
  fit = randomForest(LogSalePrice ~ ., data = train2, mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:25, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")
min(oob.err)
oob_errors1 <- data.frame(oob.err)
oob_errors1
best_mtry_rf <- 17

best_rf <- randomForest(LogSalePrice ~ ., train2, importance = TRUE, mtry = best_mtry_rf)
best_rf
best_rf$importance
importance(best_rf)
varImpPlot(best_rf)

preds_rf_train <- best_rf$predicted
actual_rf_train <- train2$LogSalePrice
rss_rf_train <- sum((preds_rf_train - actual_rf_train) ^ 2)
tss_rf_train <- sum((actual_rf_train - mean(actual_rf_train)) ^ 2)
rsq_rf_train <- 1 - rss_rf_train/tss_rf_train
rsq_rf_train

pred_rf_test <- predict(best_rf, test2, type = "response")
actual_rf_test <- test2$LogSalePrice
rss_rf_test <- sum((pred_rf_test - actual_rf_test) ^ 2)
tss_rf_test <- sum((actual_rf_test - mean(actual_rf_test)) ^ 2)
rsq_rf_test <- 1 - rss_rf_test/tss_rf_test
rsq_rf_test

best_rf_alldata <- randomForest(LogSalePrice ~ ., modeling_data,
                                importance = TRUE, mtry = best_mtry_rf)

pred_rf_unseen <- predict(best_rf_alldata, unseen_data, type = "response")
class(pred_rf_unseen)

pred_rf_unseen <- as.matrix(pred_rf_unseen)

rf_pred <- data.frame("Id" = unseen_data_raw$Id, "SalePrice" = exp(pred_rf_unseen[,1]))
rf_pred$SalePrice  <- round(rf_pred$SalePrice, digits = 0)
head(rf_pred)

write.csv(rf_pred, "rf_pred.csv", row.names = FALSE)
mlr_kaggle_score <- 0.13762


#############################################################################################
######### Ignore for now ####################################################################
#############################################################################################


# vif(backwardAIC2)
# ld.vars2 <- attributes(alias(backwardAIC2)$Complete)$dimnames[[1]]
# ld.vars2

# ## trying iterative significant selection found online
# all_vars <- names(mlr_model2[[1]])[-1]
# summ <- summary(mlr_model2)
# pvals <- summ[[4]][, 4]
# not_significant <- character()
# not_significant <- names(which(pvals > 0.1))
# not_significant <- not_significant[!not_significant %in% "(Intercept)"]
# 
# while(length(not_significant) > 0){
#   all_vars <- all_vars[!all_vars %in% not_significant[1]]
#   myForm <- as.formula(paste("LogSalePrice ~ ", paste (all_vars, collapse=" + "), sep=""))  # new formula
#   selectedMod <- lm(myForm, data=train2)  # re-build model with new formula
#   
#   # Get the non-significant vars.
#   summ <- summary(selectedMod)
#   pvals <- summ[[4]][, 4]
#   not_significant <- character()
#   not_significant <- names(which(pvals > 0.1))
#   not_significant <- not_significant[!not_significant %in% "(Intercept)"]
# }


##

# formula.new <- as.formula(
#   paste(
#     paste(deparse(formula), collapse=""), 
#     paste(ld.vars, collapse="-"),
#     sep="-"
#   )
# )
# 
# 
# fit.new <-lm(formula.new)
# vif(fit.new)

# fixing contraat error
# glimpse(train)
# 
# debug_contr_error <- function (dat, subset_vec = NULL) {
#   if (!is.null(subset_vec)) {
#     ## step 0
#     if (mode(subset_vec) == "logical") {
#       if (length(subset_vec) != nrow(dat)) {
#         stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
#       }
#       subset_log_vec <- subset_vec
#     } else if (mode(subset_vec) == "numeric") {
#       ## check range
#       ran <- range(subset_vec)
#       if (ran[1] < 1 || ran[2] > nrow(dat)) {
#         stop("'numeric' `subset_vec` provided but values are out of bound")
#       } else {
#         subset_log_vec <- logical(nrow(dat))
#         subset_log_vec[as.integer(subset_vec)] <- TRUE
#       } 
#     } else {
#       stop("`subset_vec` must be either 'logical' or 'numeric'")
#     }
#     dat <- base::subset(dat, subset = subset_log_vec)
#   } else {
#     ## step 1
#     dat <- stats::na.omit(dat)
#   }
#   if (nrow(dat) == 0L) warning("no complete cases")
#   ## step 2
#   var_mode <- sapply(dat, mode)
#   if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
#   var_class <- sapply(dat, class)
#   if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
#     stop("matrix variables with 'AsIs' class must be 'numeric'")
#   }
#   ind1 <- which(var_mode %in% c("logical", "character"))
#   dat[ind1] <- lapply(dat[ind1], as.factor)
#   ## step 3
#   fctr <- which(sapply(dat, is.factor))
#   if (length(fctr) == 0L) warning("no factor variables to summary")
#   ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
#   dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
#   ## step 4
#   lev <- lapply(dat[fctr], base::levels.default)
#   nl <- lengths(lev)
#   ## return
#   list(nlevels = nl, levels = lev)
# }
# debug_contr_error2 <- function (form, dat, subset_vec = NULL) {
#   ## step 0
#   if (!is.null(subset_vec)) {
#     if (mode(subset_vec) == "logical") {
#       if (length(subset_vec) != nrow(dat)) {
#         stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
#       }
#       subset_log_vec <- subset_vec
#     } else if (mode(subset_vec) == "numeric") {
#       ## check range
#       ran <- range(subset_vec)
#       if (ran[1] < 1 || ran[2] > nrow(dat)) {
#         stop("'numeric' `subset_vec` provided but values are out of bound")
#       } else {
#         subset_log_vec <- logical(nrow(dat))
#         subset_log_vec[as.integer(subset_vec)] <- TRUE
#       } 
#     } else {
#       stop("`subset_vec` must be either 'logical' or 'numeric'")
#     }
#     dat <- base::subset(dat, subset = subset_log_vec)
#   }
#   ## step 0 and 1
#   dat_internal <- stats::lm(form, data = dat, method = "model.frame")
#   attr(dat_internal, "terms") <- NULL
#   ## rely on `debug_contr_error` for steps 2 to 4
#   c(list(mf = dat_internal), debug_contr_error(dat_internal, NULL))
# }
# debug_contr_error2(LogSalePrice ~ ., train)
# 
# table(housing_data_raw$Utilities)
