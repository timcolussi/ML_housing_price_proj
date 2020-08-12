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

set.seed(2187)
index <- sample(1:nrow(modeling_data), size=nrow(modeling_data)*0.8)
train <- modeling_data[index, ]
test <- modeling_data[-index, ]

mlr_model <- lm(LogSalePrice ~ ., train)

summary(mlr_model)
library(car)
vif(mlr_model)

ld.vars <- attributes(alias(mlr_model)$Complete)$dimnames[[1]]
ld.vars

mlr.empty <- lm(LogSalePrice ~ 1, data = train)
mlr.saturated <- lm(LogSalePrice ~ ., data = train)
scope = list(lower = formula(mlr.empty), upper = formula(mlr.saturated))

library(MASS)
backwardAIC = step(mlr.saturated, scope, direction = "backward", k = 2)
forwardAIC = step(mlr.empty, scope, direction = "forward", k = 2)

summary(backwardAIC)
summary(forwardAIC)
vif(backwardAIC)
vif(forwardAIC)

modeling_data2 <- modeling_data
modeling_data2$LogSalePrice <- NULL
cor(modeling_data)


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
