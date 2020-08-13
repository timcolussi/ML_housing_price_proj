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

# library(lubridate)
# 
# year_blt <- strptime(modeling_data$YearBuilt, "%Y")
# mo_blt <- strptime(modeling_data$MoSold, "%M")
# year(year_blt)
# month(mo_blt)
# 
# head(modeling_data$MoSold, 15)

# drop Utilities because they are all the same
modeling_data$Utilities <- NULL

# table(housing_data$SaleCondition)
# 
# other_roofmatl <- c("ClyTile", "Membran", "Metal", "Roll")
# housing_data$RoofMatl <- ifelse(housing_data$RoofMatl %in% other_roofmatl, "Other", housing_data$RoofMatl)
# housing_data$HeatingQC <- ifelse(housing_data$HeatingQC == "Po", "Fa", housing_data$HeatingQC)
# 
# even_more_columns_to_drop <- c("Condition2", "Exterior1st", "Exterior2nd", "ExterCond", "Heating",
#                                "Electrical")
# which(housing_data$Functional == "Sev")
# housing_data <- housing_data[-c(667), ]
# housing_data[, even_more_columns_to_drop] <- NULL



# set.seed(2187)
# index <- sample(1:nrow(modeling_data), size=nrow(modeling_data)*0.8)
# train <- modeling_data[index, ]
# test <- modeling_data[-index, ]
# 
# mlr_model <- lm(LogSalePrice ~ ., train)
# 
# summary(mlr_model)
# library(car)
# vif(mlr_model)
# 
# ld.vars <- attributes(alias(mlr_model)$Complete)$dimnames[[1]]
# ld.vars
# 
# mlr.empty <- lm(LogSalePrice ~ 1, data = train)
# mlr.saturated <- lm(LogSalePrice ~ ., data = train)
# scope = list(lower = formula(mlr.empty), upper = formula(mlr.saturated))
# 
# library(MASS)
# backwardAIC = step(mlr.saturated, scope, direction = "backward", k = 2)
# forwardAIC = step(mlr.empty, scope, direction = "forward", k = 2)
# 
# summary(backwardAIC)
# summary(forwardAIC)
# vif(backwardAIC)
# vif(forwardAIC)
# 
# modeling_data2 <- modeling_data
# modeling_data2$LogSalePrice <- NULL
# cor(modeling_data)
# 
# test_target <- test
# test_target$LogSalePrice <- NULL
# 
# mlr_model_pred <- predict(mlr_model, test_target, interval = "prediction")
# 
# table(housing_data_raw$Neighborhood)
# table(train$Neighborhood)
# table(test$Neighborhood)

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

a <- table(housing_data_raw$BsmtExposure)
a[[1]]

test_target2 <- test2
test_target2$LogSalePrice <- NULL
mlr_model_pred2 <- predict(mlr_model2, test_target2, interval = "prediction")

test_tab1 <- table(housing_data_raw$Exterior2nd)
test_tab1
dim(test_tab1)

table(truth = train2$LogSalePrice, prediction = mlr_model2$fitted.values)
which(sapply(modeling_data, is.factor))

anova(mlr_model2)

summary(mlr_model2)



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
