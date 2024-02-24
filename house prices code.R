### Before proceeding, please install and load the packages below :


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(naniar)) install.packages("naniar", repos = "http://cran.us.r-project.org")
if(!require(missMDA)) install.packages("missMDA", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(paran)) install.packages("paran", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(mgcv)) install.packages("mgcv", repos = "http://cran.us.r-project.org")
if(!require(nlme)) install.packages("nlme", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(vtreat)) install.packages("vtreat", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")


### Downloading repository and files ####

# Downloading the GitHub repository with the train and test sets from GitHub to Rstudio environment

setwd("~")

url_zip_gitrepo <- "https://github.com/a-raulet/house-prices-prediction/archive/master.zip"

download.file(url_zip_gitrepo, "house.zip")

unzip("house.zip", exdir = "~")

# Train

setwd("~/house-prices-prediction-master")
train <- read.csv("train.csv", stringsAsFactors = FALSE)

# test

test <- read.csv("test.csv", stringsAsFactors = FALSE)


### Data Exploration ####

# Structure of train set
str(train)


# Structure of test set
str(test)

# Number of categorical variables
categ_data <- select_if(train, is.character)
length(categ_data)

# Number of numerical variables
num_data <- select_if(train, is.numeric)
length(num_data)


### Cleaning and Missing Data ####

library(naniar)
# Checking missing value
sum(is.na(train))

miss_var_summary(train) %>% filter(n_miss > 0)

# Barplot of variables with missing values
gg_miss_var(train)


# According to data description most NAs mean actually "None"
# Imputing "None" for 15 variables.
# For ordinal variables :
train$Fence[is.na(train$Fence)] <- "None"
train$BsmtQual[is.na(train$BsmtQual)] <- "None"
train$BsmtCond[is.na(train$BsmtCond)] <- "None"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "None"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "None"
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "None"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "None"
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
train$GarageQual[is.na(train$GarageQual)] <- "None"
train$GarageCond[is.na(train$GarageCond)] <- "None"
train$PoolQC[is.na(train$PoolQC)] <- "None"

# For categorical variables :
train$MiscFeature[is.na(train$MiscFeature)] <- "None"
train$Alley[is.na(train$Alley)] <- "None"
train$GarageType[is.na(train$GarageType)] <- "None"
train$MasVnrType[is.na(train$MasVnrType)] <- "None"

# For numerical variables :
# If MasVnrType missing data are "None", then area must be 0.
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0

# We replace GarageYrBlt by the YearBuilt value
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- train$YearBuilt[is.na(train$GarageYrBlt)]

# We replace the only NA in Electrical variable with the most common value : "SBrkr"
train %>% ggplot(aes(Electrical)) +
  geom_bar()

train$Electrical[is.na(train$Electrical)] <- "SBrkr"

# Changing 'CentralAir' variable with '0' and '1' instead of 'N' and 'Y' :
train <- train %>% mutate(CentralAir = ifelse(CentralAir == "Y", 1, 0))

# Converting numerical variable 'MSSubClass' to factor variable :
train$MSSubClass <- factor(train$MSSubClass)

# Changing character ordinal variables to numerical ordinal variables
train <- train %>% mutate(LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), ordered = TRUE, labels = c(1, 2, 3, 4)),  
                          LandContour = factor(LandContour, levels = c("Low", "HLS", "Bnk", "Lvl"), ordered = TRUE, labels = c(1, 2, 3, 4)), 
                          LandSlope = factor(LandSlope, levels = c("Gtl", "Mod", "Sev"), ordered = TRUE, labels = c(1, 2, 3)),  
                          ExterQual = factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)), 
                          ExterCond = factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)),
                          BsmtQual = factor(BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)), 
                          BsmtCond = factor(BsmtCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)), 
                          BsmtExposure = factor(BsmtExposure, levels = c("None", "No", "Mn", "Av", "Gd"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)), 
                          BsmtFinType1 = factor(BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "BLQ", "ALQ", "GLQ"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5, 6, 7)), 
                          BsmtFinType2 = factor(BsmtFinType2, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "BLQ", "ALQ", "GLQ"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5, 6, 7)), 
                          HeatingQC = factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)),
                          KitchenQual = factor(KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)), 
                          Functional = factor(Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = TRUE, labels = c(1, 2, 3, 4, 5, 6, 7, 8)), 
                          FireplaceQu = factor(FireplaceQu, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)), 
                          GarageFinish = factor(GarageFinish, levels = c("None", "Unf", "RFn", "Fin"), ordered = TRUE, labels = c(0, 1, 2, 3)), 
                          GarageQual = factor(GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)), 
                          GarageCond = factor(GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)),
                          PoolQC = factor(PoolQC, levels = c("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)), 
                          Fence = factor(Fence, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)),
                          Utilities = factor(Utilities, levels = c("ELO", "NoSeWa", "NoSewr", "AllPub"), ordered = TRUE, labels = c(1, 2, 3, 4)),
                          Electrical = factor(Electrical, levels = c("Mix", "FuseP", "FuseF", "FuseA", "SBrkr"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)),
                          PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), ordered = TRUE, labels = c(0, 1, 2)))


# Number of remaining character variables :
train %>% select_if(is.character) %>% length()

# Changing remaining character variables to factor variables
train <- train %>% mutate_if(is.character, factor)


glimpse(train)

miss_var_summary(train)

# Train - Dev Split
n = floor(0.8 * nrow(train))
set.seed(69)
index = sample(seq_len(nrow(train)), size = n)

train = train[index,]
dev = train[-index,]


### Recettes
library(recipes)

base_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
#  step_date(DateSold, features = c("month", "year")) %>%
#  step_holiday(DateSold, holidays = timeDate::listHolidays("US")) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, -MoSold)) %>%
  step_dummy(all_nominal_predictors())
  
base_prep <- base_recipe %>% prep()

print(base_prep)

transformed_base_train <- base_prep %>% bake(new_data = NULL)

print(transformed_base_train)

miss_var_summary(transformed_base_train)

pairs(transformed_base_train[,1:10])

# Simple linear regression model with lm() function

lm_mod <- lm(SalePrice ~ . -Id, data = transformed_base_train)
summary(lm_mod)

ggplot(transformed_base_train, aes(fitted(lm_mod), SalePrice)) + geom_point() + geom_abline()

RMSE(fitted(lm_mod), transformed_base_train$SalePrice)

baked_dev <- base_prep |> bake(new_data = dev)
pred_lm <- predict(lm_mod, newdata = baked_dev)

RMSE(pred_lm, baked_dev$SalePrice)

# Detecting multicollinearity between predictors
library(plm)
detect.lindep(transformed_base_train)

library(corrplot)
train_matrix <- select_if(transformed_base_train, is.numeric)
corr_matrix <- cor(as.matrix(train_matrix))

corrplot(corr_matrix, method = "circle", sig.level = 0.05)

#suspicious_feat <- transformed_base_train[, c(16, 18, 19, 20, 23, 24, 25, 26, 66, 123, 145, 148, 162, 53)]
#suspicious_feat <- transformed_base_train[, c(69, 119, 122, 125, 150, 164, 187, 198)]
suspicious_feat <- transformed_base_train[, c(17, 19, 20, 21, 25, 26, 27, 28, 69, 119, 122, 125, 150, 164, 187, 198)]
susp_corr_matrix <- cor(as.matrix(suspicious_feat))

corrplot(susp_corr_matrix, method = "number")

library(ggcorrplot)
ggcorrplot(susp_corr_matrix, method = "circle")

# All the following predictors have only zeros in every row.
sum(transformed_base_train$Condition2_PosA)
sum(transformed_base_train$Condition2_RRAn)
sum(transformed_base_train$Exterior1st_CBlock)
sum(transformed_base_train$Heating_OthW)
sum(transformed_base_train$MiscFeature_TenC)

# Let's remove the collinear variables 'BsmtUnfSF', 'BsmtFinSF1', 'X1stFlrSF', 
# 'X2ndFlrSF', 'BldgType_Duplex', 'Exterior2nd_CBlock', 'Condition2_PosA', 'Condition2_RRAn', 'Exterior1st_CBlock', 
# 'Heating_OthW', 'MiscFeature_TenC', 'Heating_Wal' (The last one is because it displays NA in the summary) and 'Exterior1st_AsphShn'.

clean_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, -MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BsmtFinSF1, BsmtUnfSF, X1stFlrSF, X2ndFlrSF, BldgType_Duplex, 
                 Exterior2nd_CBlock, Condition2_PosA, Condition2_RRAn, Exterior1st_CBlock, 
                 Heating_OthW, MiscFeature_TenC, Heating_Wall, Exterior1st_AsphShn))

clean_prep <- clean_recipe %>% prep()

print(clean_prep)

clean_train <- clean_prep %>% bake(new_data = NULL)
clean_dev <- clean_prep %>% bake(new_data = dev)

clean_lm_mod <- lm(SalePrice ~ . -Id, data = clean_train)
summary(clean_lm_mod) %>% print()

pred_clean_lm <- predict(clean_lm_mod, newdata = clean_dev)
RMSE(pred_clean_lm, clean_dev$SalePrice)



# By removing multicollinearity, we get an even better RMSE (0.085 now, against 0.093 before)
# on the dev set.

# While we removed multicollinearity, our RMSE slightly went up. (0.0912, against 0.089 before).
# Something must be wrong. Let's check multicollinearity again, but with other functions.

# Checking again multicollinearity, but with the vif() and kappa() function.
detect.lindep(clean_train)
# Everything seems to be clean for the detect.lindep() function.

library(car)

vif(clean_lm_mod)
vif_df = as.data.frame(vif(clean_lm_mod))
names(vif_df) = "vif"
vif_df <- vif_df |> rownames_to_column(var = "variable")

vif_df <- vif_df %>%
  arrange(desc(vif))

print(vif_df)


# Create a ggplot barplot with sorted variables
vif_df |> filter(vif >= 30) |>
ggplot(aes(x = reorder(variable, vif), y = vif)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text = element_text(size = 6))

# Condition number of (X.T X)
kappa(clean_lm_mod, exact = TRUE)

# Both the vif function and the condition number kappa indicate there is still multicollinearity.
# We can remove more variables, or build a ridge-lasso regression model or a PCA.
# Let's first remove even more variables and see what happens.

reduced_clean_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, -MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BsmtFinSF1, BsmtUnfSF, X1stFlrSF, X2ndFlrSF, BldgType_Duplex, 
                 Exterior2nd_CBlock, Condition2_PosA, Condition2_RRAn, Exterior1st_CBlock, 
                 Heating_OthW, MiscFeature_TenC, Heating_Wall)) %>%
  step_select(-c(MiscFeature_None, Exterior2nd_VinylSd, RoofStyle_Gable))

reduced_clean_prep <- reduced_clean_recipe %>% prep()

print(reduced_clean_prep)

reduced_clean_train <- reduced_clean_prep %>% bake(new_data = NULL)
reduced_clean_dev <- reduced_clean_prep %>% bake(new_data = dev)

reduced_clean_lm_mod <- lm(SalePrice ~ . -Id, data = reduced_clean_train)
summary(reduced_clean_lm_mod) %>% print()

pred_reduced_clean_lm <- predict(reduced_clean_lm_mod, newdata = reduced_clean_dev)
RMSE(pred_reduced_clean_lm, reduced_clean_dev$SalePrice)

# Condition number of (X.T X)
kappa(reduced_clean_lm_mod, exact = TRUE)

# If we remove too many variables. The RMSE goes up to 0.12.
# If we only remove the 3 variables with the highest 'vif' score, that is 'MiscFeature_None', 
# MiscFeature_Shed, and RoofStyle_Gable, the RMSE slightly decreases to 0.0910,
# but is almost equal to the first clean model 'clean_lm' (0.0912).
# We can also try to select the most important variables.

# Most important features with 'vip' package.
library(vip)
vip(reduced_clean_lm_mod, n = 50)
imp_feat <- vi(reduced_clean_lm_mod)

tail(imp_feat)

feat100 <- imp_feat$Variable[1:100]

clean_tr100 <- reduced_clean_train[, feat100]
clean_tr100$SalePrice <- reduced_clean_train$SalePrice

clean_dev100 <- reduced_clean_dev[, feat100]
clean_dev100$SalePrice <- reduced_clean_dev$SalePrice

lm100 <- lm(SalePrice ~ ., data = clean_tr100)

summary(lm100) %>% print()

pred_lm100 <- predict(lm100, newdata = clean_dev100)
RMSE(pred_lm100, clean_dev100$SalePrice)

# With a simple hypothesis of selecting the 100 most important features,
# the RMSE slightly goes up to 0.093.
# We can also select the 100 most important features from the summary of
# the clean model 'clean_lm_mod'.

summary_data <- summary(reduced_clean_lm_mod)
# Sort by p-values
sorted_summary_data <- summary_data$coefficients[order(summary_data$coefficients[, "Pr(>|t|)"]), ]

# Set a significance level
significance_level <- 0.05

# Filter for significant variables
significant_vars <- sorted_summary_data[sorted_summary_data[, "Pr(>|t|)"] < significance_level, ]
rownames(significant_vars)
# View the most significant variables
print(significant_vars)
tail(significant_vars)

feat100 <- rownames(significant_vars)[1:52]

print(feat100)

feat100 <- feat100[-1]
clean_tr100 <- reduced_clean_train[, feat100]
clean_tr100$SalePrice <- reduced_clean_train$SalePrice

clean_dev100 <- reduced_clean_dev[, feat100]
clean_dev100$SalePrice <- reduced_clean_dev$SalePrice

lm100 <- lm(SalePrice ~ ., data = clean_tr100)

summary(lm100) %>% print()

pred_lm100 <- predict(lm100, newdata = clean_dev100)
RMSE(pred_lm100, clean_dev100$SalePrice)

# The RMSE is still higher (0.101), but with 57 predictors only this time. 
# (NAs found from the 56th to the 100th column)

# Feature selection with forward method
library(leaps)
reg = regsubsets(SalePrice ~ . -Id, data = reduced_clean_train, method = "forward", nvmax = 194)
reg_summ = summary(reg)

reg_summ$which
length(reg_summ$bic)
n = dim(clean_train)[1]

AIC = 2 * (2:195) + n * log(reg_summ$rss/n)
which.min(AIC)

# According to AIC, the forward method selects 108 predictors
plot(AIC ~ I(1:194), xlab = "nb of predictors", ylab = "AIC")
points(x = which.min(AIC), pch = 19, col = "blue")

# According to Adjusted R-squared, the forward method selects 124 predictors
plot(1:194, reg_summ$adjr2, xlab = "number of predictors", ylab = "Adj. R-squared")
which.max(reg_summ$adjr2)

BIC = log(n) * (2:195) + n * log(reg_summ$rss / n)
which.min(reg_summ$bic)
which.min(BIC)

# According to BIC, the forward method selects 62 predictors
plot(BIC ~ I(1:194), xlab = "nb of predictors", ylab = "BIC")
points(x = which.min(BIC), pch = 19, col = "blue")

plot(reg_summ$bic ~ I(1:194), xlab = "nb of predictors", ylab = "BIC")



# Feature selection with backward selection
reg = regsubsets(SalePrice ~ . -Id, data = reduced_clean_train, method = "backward", nvmax = 194)
reg_summ = summary(reg)

reg_summ$which

n = dim(clean_train)[1]

AIC = 2 * (2:195) + n * log(reg_summ$rss/n)
min_aic_index = which.min(AIC)

# According to AIC, the backward method selects 92 predictors
plot(AIC ~ I(1:194), xlab = "nb of predictors", ylab = "AIC")
points(x = min_aic_index, y = min(AIC), pch = 19, col = "blue")
abline(v = min_aic_index, col = "red", lty = 2)
text(x = min_aic_index, y = min(AIC) + 60, labels = paste("Predictors =", min_aic_index), col = "black")

# According to Adjusted R-squared, the backward method selects 128 predictors
plot(1:194, reg_summ$adjr2, xlab = "number of predictors", ylab = "Adj. R-squared")
which.max(reg_summ$adjr2)

BIC = log(n) * (2:195) + n * log(reg_summ$rss / n)
which.min(BIC)
which.min(reg_summ$bic)

# According to BIC, the backward method selects 48 predictors
plot(BIC ~ I(1:194), xlab = "nb of predictors", ylab = "BIC")
points(x = which.min(BIC), pch = 19, col = "blue")

plot(reg_summ$bic ~ I(1:194), xlab = "nb of predictors", ylab = "BIC")

# To resume,
# For forward selection, we have :
# AIC = 108 predictors
# Adj.R2 = 124 predictors
# BIC = 62 predictors
#
# For backward selection, we have :
# AIC = 92 predictors
# Adj.R2 = 128 predictors
# BIC = 48 predictors

# Let's create linear models for each hypotheses.
# H1 = 108 predictors
lm_aic108 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:110])
summary(lm_aic108)
pred_aic108 <- predict(lm_aic108, newdata = reduced_clean_dev[, 2:110])
RMSE(pred_aic108, reduced_clean_dev$SalePrice)

# H2 = 124 predictors
lm_adjr2_124 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:126])
summary(lm_adjr2_124)
pred_adjr2_124 <- predict(lm_adjr2_124, newdata = reduced_clean_dev[, 2:126])
RMSE(pred_adjr2_124, reduced_clean_dev$SalePrice)

# H3 = 62 predictors
lm_bic62 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:64])
summary(lm_bic62)
pred_bic62 <- predict(lm_bic62, newdata = reduced_clean_dev[, 2:64])
RMSE(pred_bic62, reduced_clean_dev$SalePrice)

# H4 = 92 predictors
lm_aic92 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:94])
summary(lm_aic92)
pred_aic92 <- predict(lm_aic92, newdata = reduced_clean_dev[, 2:94])
RMSE(pred_aic92, reduced_clean_dev$SalePrice)

# H5 = 128 predictors
lm_adjr2_128 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:130])
summary(lm_adjr2_128)
pred_adjr2_128 <- predict(lm_adjr2_128, newdata = reduced_clean_dev[, 2:130])
RMSE(pred_adjr2_128, reduced_clean_dev$SalePrice)

# H6 = 48 predictors
lm_bic48 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:52])
summary(lm_bic48)
pred_bic48 <- predict(lm_bic48, newdata = reduced_clean_dev[, 2:52])
RMSE(pred_bic48, reduced_clean_dev$SalePrice)

# It seems the RMSE gets better with more predictors and gets worse with less predictors.
# A ridge regression seems to be more adapted to keep more variables. 
# We can also add some L1 regularization (lasso) as well, to remove useless predictors.

# GLMNet model
# Creating a cross-validation plan
cv_plan <- trainControl(method = "cv", number = 10)
# Training a GLMnet model
set.seed(69, sample.kind = "Rounding")

model_glmnet <- train(SalePrice ~ ., 
                      reduced_clean_train,
                      tuneGrid = expand.grid(alpha = seq(0, 1, 0.1),
                                             lambda = seq(0.0001, 10^3, length = 100)),
                      method = "glmnet",
                      trControl = cv_plan)

# Plot of ridge and lasso parameters
plot(model_glmnet)

model_glmnet$bestTune
# Predictions of GLMnet model :
pred_glmnet <- predict(model_glmnet, reduced_clean_dev)

(rmse_glmnet <- RMSE(pred_glmnet, reduced_clean_dev$SalePrice))

# Ridge regression only
x_train <- model.matrix(SalePrice ~ ., reduced_clean_train)[, -49]
y_train <- reduced_clean_train$SalePrice

x_dev <- model.matrix(SalePrice ~ ., reduced_clean_dev)[, -49]
y_dev <- reduced_clean_dev$SalePrice

grid <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x_train, y_train, alpha = 0, lambda = grid, standardize = FALSE)

plot(ridge_mod)

set.seed(69)
cv_out <- cv.glmnet(x_train, y_train, alpha = 0)
plot(cv_out)

best_lambda <- cv_out$lambda.min
best_lambda

ridge_pred <- predict(ridge_mod, s = best_lambda, newx = x_dev)
RMSE(ridge_pred, y_dev)

# Lasso regression only
lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = grid, standardize = FALSE)

plot(lasso_mod)

set.seed(69)
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv_out_lasso)

best_lambda_lasso <- cv_out_lasso$lambda.min
best_lambda_lasso

lasso_pred <- predict(lasso_mod, s = best_lambda_lasso, newx = x_dev)
RMSE(lasso_pred, y_dev)

# Elasticnet model
# Let's build an Elasticnet model with different values for alpha.

cv_elnet_model <- cv.glmnet(x_train, y_train, alpha = 0.4, lambda = grid)
plot(cv_elnet_model)

best_elnet_lambda = cv_elnet_model$lambda.min

elnet_model <- glmnet(x_train, y_train, lambda =best_elnet_lambda, standardize = FALSE)
elnet_pred <- predict(elnet_model, x_dev)

RMSE(elnet_pred, y_dev)
#It looks like we cannot go under 0.132 for now...

# PCA
pca <- prcomp(reduced_clean_train[, -c(1, 49)])

summary(pca)

fviz_pca_var(pca, select.var = list(contrib = 5), repel = TRUE)


# Checking contribution of variables for PC1 and PC2 :
# PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

# PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)

# Screeplot to visualize the "elbow" to determine the number of components (2 or 3 components)
fviz_screeplot(pca)

# Applying Kaiser-Guttman rule to determine the number of components (eigenvalue > 1) : (17 components)
get_eigenvalue(pca) %>% filter(eigenvalue > 1)

# Parallel analysis to determine the number of components to retain 
# Parallel analysis :
paran_output <- paran(reduced_clean_train[,-c(1, 49)], seed = 69, graph = TRUE)

# Number of components to retain : (46 components)
paran_output$Retained

# PCR model with 17 components
library(pls)
pcr_mod <- pcr(SalePrice ~ . -Id, data = reduced_clean_train, scale = TRUE, validation = "CV")
validationplot(pcr_mod)

pcr_pred <- predict(pcr_mod, reduced_clean_dev, ncomp = 192)
RMSE(pcr_pred, reduced_clean_dev$SalePrice)


# Let's create a recipe by adding a PCA and retaining 16 components
pca17_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, -MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BsmtFinSF1, BsmtUnfSF, X1stFlrSF, X2ndFlrSF, BldgType_Duplex, 
                 Exterior2nd_CBlock, Condition2_PosA, Condition2_RRAn, Exterior1st_CBlock, 
                 Heating_OthW, MiscFeature_TenC, Heating_Wall)) %>%
  step_select(-c(MiscFeature_None, Exterior2nd_VinylSd, RoofStyle_Gable)) %>%
  step_pca(all_predictors(), num_comp = 17)

pca17_prep <- pca17_recipe %>% prep()

print(pca17_prep)

pca17_train <- pca17_prep %>% bake(new_data = NULL)
pca17_dev <- pca17_prep %>% bake(new_data = dev)

pca17_lm_mod <- lm(SalePrice ~ . -Id, data = pca17_train)
summary(pca17_lm_mod) %>% print()

pred_pca17_lm <- predict(pca17_lm_mod, newdata = pca17_dev)
RMSE(pred_pca17_lm, pca17_dev$SalePrice)

# Let's create a recipe by adding a PCA and retaining 46 components
pca46_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, -MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BsmtFinSF1, BsmtUnfSF, X1stFlrSF, X2ndFlrSF, BldgType_Duplex, 
                 Exterior2nd_CBlock, Condition2_PosA, Condition2_RRAn, Exterior1st_CBlock, 
                 Heating_OthW, MiscFeature_TenC, Heating_Wall)) %>%
  step_select(-c(MiscFeature_None, Exterior2nd_VinylSd, RoofStyle_Gable)) %>%
  step_pca(all_predictors(), num_comp = 46)

pca46_prep <- pca46_recipe %>% prep()

print(pca46_prep)

pca46_train <- pca46_prep %>% bake(new_data = NULL)
pca46_dev <- pca46_prep %>% bake(new_data = dev)

pca46_lm_mod <- lm(SalePrice ~ . -Id, data = pca46_train)
summary(pca46_lm_mod) %>% print()

pred_pca46_lm <- predict(pca46_lm_mod, newdata = pca46_dev)
RMSE(pred_pca46_lm, pca46_dev$SalePrice)

# PCA is surprisingly disappointing, whether it is with 17 or 46 components.

# Let's try something else.
# Training a randomForest model
model_rf <- train(SalePrice ~ . -Id, 
                  tuneLength = 10, 
                  data = reduced_clean_train, 
                  method = "ranger",
                  trControl = cv_plan)

# Plot of the tuning of 10 randomForest models
plot(model_rf)

# Best tune for the randomForest model :
model_rf$bestTune

# Predictions of the randomForest model :
pred_rf <- predict(model_rf, reduced_clean_dev)

(rmse_rf <- RMSE(pred_rf, reduced_clean_dev$SalePrice))

# The 'ranger' model gets the best performance for now : 0.062.

# XGBoost model
# Tuning hyperparmaters for XGBoost model :
xgb_grid <- expand.grid(nrounds = c(200, 500, 800, 1000),
                        max_depth = c(2,3,4,5,6,7,8), # default = 6
                        eta = c(0.05,0.1,0.2,0.3,0.4),# default = 0.3
                        gamma = 1,
                        colsample_bytree = 1, # default = 1
                        min_child_weight = 1, # default = 1
                        subsample = 1) # default = 1


set.seed(69, sample.kind = "Rounding") # We must set the seed to get the same results

# Be careful ! Running time for this code is around 15 minutes !
xgb_tune <-train(SalePrice ~ . -Id,
                 data = reduced_clean_train,
                 method = "xgbTree",
                 trControl = cv_plan, # 10-fold cross-validation plan
                 tuneGrid = xgb_grid, # hyperparameters we want to tune
                 verbose = FALSE,
                 metric = "RMSE",
                 nthread =3)

# Plot of the XGBoost tuning
plot(xgb_tune)

# XGBoost best tune
xgb_tune$bestTune

# Training an XGBoost model
model_xgb <- xgboost(data = as.matrix(reduced_clean_train[, -c(1, 52)]), # training data as matrix without the 'SalePrice' outcome
                     label = reduced_clean_train$SalePrice,  # we must use the original column of train_set
                     nrounds = 200,       # number of trees to build
                     objective = "reg:squarederror", # for regression
                     eta = 0.2,
                     max_depth = 3,
                     verbose = 0)  # silent

# Predictions of the XGBoost model
pred_xgb <- predict(model_xgb, as.matrix(reduced_clean_dev[, -c(1, 52)]))

(rmse_xgb <- RMSE(pred_xgb, reduced_clean_dev$SalePrice))

# XGBoost performs even better with a James Bond score : 0.007 !

# Training a GAM
# Let's check again the most important predictors and see if their non-linear relationships
vip(reduced_clean_lm_mod, n = 20)
print(imp_feat, n = 20)

imp_feat20 <- imp_feat$Variable[1:20]
imp_feat20

train20 <- reduced_clean_train[, imp_feat20]
train20$SalePrice <- reduced_clean_train$SalePrice

pairs(SalePrice ~ ., data = train20)

model_gam <- gam(SalePrice ~ s(OverallQual) + s(GrLivArea) + s(TotalBsmtSF), 
                 family = gaussian, # Important !
                 data = reduced_clean_train)

summary(model_gam)
plot(model_gam)
# Predictions of GAM :
pred_gam <- predict(model_gam, reduced_clean_dev)

# RMSE for GAM :
(rmse_gam <- RMSE(pred_gam, reduced_clean_dev$SalePrice))

# GAM predictions VS actual values plot :
reduced_clean_dev %>% cbind(pred_gam) %>% 
  ggplot(., aes(pred_gam, SalePrice)) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("GAM predictions vs actual values")


### Tensorflow Model ###
library(reticulate)

#install_miniconda(update = TRUE)
#install_python(version = "3.11:latest")
#conda_create(envname = "r-tflow", python_version = "3.11.6")

#tensorflow::install_tensorflow(version = "nightly", envname = "r-tflow")
#keras::install_keras(method = "conda", version = "nightly", envname = "r-tflow")

conda_version()
conda_list()

use_condaenv("r-tflow")

library(tensorflow)
library(keras)

tf$constant("Hello TensorFlow!")

tensorflow::tf_version()

tensorflow::tf_config()
py_config()

# Fractionner les données en ensembles d'entraînement et de dev
set.seed(69)

X_train <- reduced_clean_train[, -c(1, 52)]
y_train <- reduced_clean_train$SalePrice
X_dev <- reduced_clean_dev[, -c(1, 52)]
y_dev <- reduced_clean_dev$SalePrice

# Créer le modèle
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

# Compiler le modèle
model %>% compile(optimizer = "adam", loss = "rmse")

# Entraîner le modèle
model %>% keras::fit(
  x = X_train,
  y = y_train,
  epochs = 10,
  batch_size = 32,
  test_data = list(X_dev, y_dev)
)


colnames(X_dev) <- c("input")
# Évaluer le modèle sur l'ensemble de test
loss <- model %>% evaluate(X_dev, y_dev)
print(paste("Loss:", loss))



# Simple linear regression model

library(parsnip)
lm_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

library(workflows)
base_wkfl <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(base_recipe)

lm_fit <- base_wkfl %>%
  fit(data = train)

library(vip)
library(yardstick)
vip(lm_fit, num_features = 60)


# Cross-Validation
library(rsample)
library(tune)
cv_metrics <- metric_set(rmse)

set.seed(69)
train_folds <- vfold_cv(train, v = 10, strata = SalePrice)



# Random Forest with basic recipe
randfo_model <- rand_forest(mtry = 3, trees = tune(), min_n = tune()) %>%
  set_engine("randomForest") %>%
  set_mode(mode = "regression")

library(dials)
tree_grid <- grid_regular(trees(), min_n(), levels = 5)


randfo_wkf <- workflow() %>%
  add_model(randfo_model) %>%
  add_recipe(base_recipe)

rand_resamp <- randfo_wkf %>%
  tune_grid(resamples = train_folds,
            grid = tree_grid)

rand_resamp |> collect_metrics() |>
  mutate(min_n = factor(min_n)) |>
  ggplot(aes(trees, mean, color = min_n)) + geom_line() + geom_point()

rand_resamp |> show_best("rmse")

best_rand <- rand_resamp |> select_best("rmse")


final_randfo_wkf <- randfo_wkf |> finalize_workflow(best_rand)


final_rando_fit <- final_randfo_wkf |> fit(train)

final_rando_fit

# Simple random forest fit
randfo_fit <- randfo_wkf %>% fit(train)

randfo_fit


# CV fit
rand_cv_fit <- randfo_wkf %>%
  fit_resamples(resamples = train_folds,
                metrics = cv_metrics)

rand_cv_fit %>%
  collect_metrics()


# GLMNet

ridge_mod <- linear_reg(penalty = 1, mixture = 0) |>
  set_engine("glmnet")

ridge_wkfl <- workflow() |>
  add_model(ridge_mod) |>
  add_recipe(base_recipe)

ridge_fit <- ridge_wkfl |>
  fit(train)

ridge_fit

# New recipe with significant features
signif_feat <- lm_fit %>% extract_fit_parsnip() |> tidy() |> filter(p.value < 0.05)
signif_terms <- signif_feat[2:nrow(signif_feat), 1] |> as_vector()

unsignif_feat <- lm_fit %>% extract_fit_parsnip() |> tidy() |> filter(p.value >= 0.05)
unsignif_terms <- unsignif_feat[2:nrow(unsignif_feat), 1] |> as_vector()

signif_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, -MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_corr(all_numeric_predictors(), use = "pairwise.complete.obs", threshold = 0.9) %>%
  step_zv(all_numeric_predictors())

signif_prep <- signif_recipe %>% prep()

signif_train <- signif_prep %>% bake(new_data = NULL)

signif_lm <- lm(SalePrice ~ . -Id, data = signif_train)
summary(signif_lm)


ggplot(signif_train, aes(fitted(signif_lm), SalePrice)) + geom_point() + geom_abline()

RMSE(fitted(signif_lm), signif_train$SalePrice)

anova(lm_mod, signif_lm)











library(car)
vif(lm_mod)




# New recipe with reduced number of features

reduced_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0)) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(all_predictors(), neighbors = 5) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.75, method = "pearson") %>%
  step_corr(all_ordered_predictors(), threshold = 0.75, method = "spearman") %>%
  step_nzv(all_predictors())

reduced_prep <- reduced_recipe %>% prep()

reduced_train <- reduced_prep %>% bake(new_data = NULL)

reduced_wkfl <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(reduced_recipe)

lm_fit_reduced <- reduced_wkfl %>%
  fit(data = train)

lm_fit_reduced %>% extract_fit_parsnip() |> tidy() |> print()

# Reduced number of features with lm() function

lm_reduced <- lm(SalePrice ~ . -Id, data = reduced_train)
summary(lm_reduced)


house_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_impute_knn(all_predictors(), neighbors = 3) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              DateSold = make_date(year = YrSold, month = MoSold)) %>%
  step_holiday(DateSold, holidays = timeDate::listHolidays("US"),
               keep_original_cols = FALSE) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold)) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.75, method = "pearson") %>%
  step_corr(all_ordered_predictors(), threshold = 0.75, method = "spearman") %>%
  step_nzv(all_predictors())


house_prep <- house_recipe %>% prep()
print(house_prep)

library(tidymodels)
lin_reg <- linear_reg()

library(workflows)
house_wkfl <- workflow() %>%
  add_model(lin_reg) %>%
  add_recipe(house_recipe)

folds <- vfold_cv(train, v = 10)

lin_fit <- house_wkfl %>% fit(train)
lin_fit_cv <- house_wkfl %>% fit_resamples(folds)

lin_results <- tidy(lin_fit) %>% arrange(desc(estimate))
lin_fit %>% extract_fit_parsnip() %>% vip()

lin_aug <- lin_fit %>% augment(test)

library(vip)
collect_metrics(lin_fit_cv)


pred <- lin_fit_cv %>% collect_predictions()


### Importance des variables avec randomForest ou RFE (caret)

library(tidymodels)
library(randomForest)

# Créer un modèle de pipeline avec recipes et randomForest
rf_recipe <- recipe(SalePrice ~ ., data = train_data) %>%
  step_normalize(all_numeric()) %>%
  step_dummy(all_nominal())

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(randomForest)

# Ajuster le modèle
rf_fit <- rf_workflow %>%
  fit(data = train_data)

# Extraire l'importance des variables
variable_importance <- rf_fit %>%
  pull_workflow_fit() %>%
  randomForest::importance()

# Afficher l'importance des variables
print(variable_importance)


### Learning Vector Quantization pour sélectionner variables (principalement numériques

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# ou autre méthode
# Ajuster le modèle de régression quantile linéaire
quantreg_model <- rq(recipe_obj, tau = 0.5)  # Choisissez le quantile désiré (ici 0.5 pour la médiane)
# Extraire les coefficients du modèle
coefficients <- coef(quantreg_model)

# Afficher les coefficients et leur significativité
print(coefficients)




### Cleaning test set ####


miss_var_summary(test) %>% filter(n_miss > 0)

# Barplot of variables with missing values
gg_miss_var(test)



# Imputing "None" for 15 variables.
# For ordinal variables :
#test$PoolQC[is.na(test$PoolQC)] <- "None"
test$Fence[is.na(test$Fence)] <- "None"
test$BsmtQual[is.na(test$BsmtQual)] <- "None"
test$BsmtCond[is.na(test$BsmtCond)] <- "None"
test$BsmtExposure[is.na(test$BsmtExposure)] <- "None"
test$BsmtFinType1[is.na(test$BsmtFinType1)] <- "None"
test$BsmtFinType2[is.na(test$BsmtFinType2)] <- "None"
test$FireplaceQu[is.na(test$FireplaceQu)] <- "None"
test$GarageFinish[is.na(test$GarageFinish)] <- "None"
test$GarageQual[is.na(test$GarageQual)] <- "None"
test$GarageCond[is.na(test$GarageCond)] <- "None"

# For categorical variables :
test$MiscFeature[is.na(test$MiscFeature)] <- "None"
test$Alley[is.na(test$Alley)] <- "None"
test$GarageType[is.na(test$GarageType)] <- "None"
test$MasVnrType[is.na(test$MasVnrType)] <- "None"

# For numerical variables :
# If MasVnrType missing data are "None", then area must be 0.
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0

# We replace GarageYrBlt by the YearBuilt value
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- test$YearBuilt[is.na(test$GarageYrBlt)]




### Exterior variables
# NA of Exterior1st and 2nd :
which(is.na(test$Exterior1st))

# We replace the only NA in the 692th row in Exterior1st and 2nd with the most common value
# after considering Exterior Quality : 
test %>% filter(ExterQual == "TA") %>% ggplot(aes(Exterior1st)) +
  geom_bar()

test$Exterior1st[is.na(test$Exterior1st)] <- "HdBoard"

test$Exterior2nd[is.na(test$Exterior2nd)] <- "MetalSd"

### MSZoning variable
# Where are NAs for MSZoning ?
which(is.na(test$MSZoning))

test %>% 
  ggplot(aes(factor(Neighborhood), OverallQual, col = factor(MSZoning))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For NAs located in "IDOTRR" :
test$MSZoning[is.na(test$MSZoning)] <- "RM"

# For NA located in "Mitchel" :
test$MSZoning[is.na(test$MSZoning)] <- "RL"

### KitchenQual variable
# Where is the NA for KitchenQual ?
which(is.na(test$KitchenQual))

test %>% 
  ggplot(aes(factor(OverallCond), OverallQual, col = factor(KitchenQual))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test$KitchenQual[is.na(test$KitchenQual)] <- "TA"

### Garage variables
# Where is the NA for GarageCars ?
which(is.na(test$GarageCars))

test %>% 
  ggplot(aes(factor(GarageType), GarageQual, col = factor(GarageCars))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

test %>% 
  ggplot(aes(factor(GarageCond), GarageQual, col = factor(GarageCars))) +
  geom_jitter()

# 1 seems to be the best value according to the plot
test$GarageCars[is.na(test$GarageCars)] <- 1

# Where is the NA for GarageArea ?
which(is.na(test$GarageArea))

# The Garage Type is "Detchd"
test$GarageType[1117]

test %>% filter(GarageArea > 0) %>% group_by(GarageType) %>% 
  summarize(min = min(GarageArea), median = median(GarageArea))

test %>% 
  ggplot(aes(GarageType, GarageArea)) +
  geom_boxplot()

# We impute the median area for "Detchd" garage type
test$GarageArea[is.na(test$GarageArea)] <- 384


# NAs of Functional :
which(is.na(test$Functional))

test %>% ggplot(aes(Functional)) +
  geom_bar()

# "Typ" seems to be the less risky value according to the plot
test$Functional[is.na(test$Functional)] <- "Typ"

# NAs of Utilities :
which(is.na(test$Utilities))

test %>% ggplot(aes(Utilities)) +
  geom_bar()

# Every obsersation is either "AllPub" or "NA". We therefore impute "AllPub".
test$Utilities[is.na(test$Utilities)] <- "AllPub"


### Basement variables
# NAs of "Basement" variables (2 rows). No basement, so all areas equal to 0. 
# So is the number of baths.
which(is.na(test$BsmtFinSF1)) 
which(is.na(test$BsmtFinSF2))
which(is.na(test$BsmtUnfSF))
which(is.na(test$TotalBsmtSF))
which(is.na(test$BsmtFullBath))
which(is.na(test$BsmtHalfBath))


test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <- 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <- 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <- 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <- 0
test$BsmtFullBath[is.na(test$BsmtFullBath)] <- 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <- 0

### SaleType variable
# NA of "SaleType" : most common value "WD" is input.
which(is.na(test$SaleType))

test %>% ggplot(aes(SaleType)) +
  geom_bar()

test$SaleType[is.na(test$SaleType)] <- "WD"

# Changing 'CentralAir' variable with '0' and '1' :
test <- test %>% mutate(CentralAir = ifelse(CentralAir == "Y", 1, 0))

# Converting numerical variable 'MSSubClass' to factor variable :
test$MSSubClass <- factor(test$MSSubClass)

# Changing character ordinal variables to numerical ordinal variables
test <- test %>% 
  mutate(LotShape = as.integer(as.character(factor(LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), ordered = TRUE, labels = c(1, 2, 3, 4)))),
         LandContour = as.integer(as.character(factor(LandContour, levels = c("Low", "HLS", "Bnk", "Lvl"), ordered = TRUE, labels = c(1, 2, 3, 4)))),
         LandSlope = as.integer(as.character(factor(LandSlope, levels = c("Gtl", "Mod", "Sev"), ordered = TRUE, labels = c(1, 2, 3)))),
         ExterQual = as.integer(as.character(factor(ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)))),
         ExterCond = as.integer(as.character(factor(ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)))),
         BsmtQual = as.integer(as.character(factor(BsmtQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)))),
         BsmtCond = as.integer(as.character(factor(BsmtCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)))),
         BsmtExposure = as.integer(as.character(factor(BsmtExposure, levels = c("None", "No", "Mn", "Av", "Gd"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)))),
         BsmtFinType1 = as.integer(as.character(factor(BsmtFinType1, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "BLQ", "ALQ", "GLQ"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5, 6, 7)))),
         BsmtFinType2 = as.integer(as.character(factor(BsmtFinType2, levels = c("None", "Unf", "LwQ", "Rec", "BLQ", "BLQ", "ALQ", "GLQ"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5, 6, 7)))),
         HeatingQC = as.integer(as.character(factor(HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)))),
         KitchenQual = as.integer(as.character(factor(KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(1, 2, 3, 4, 5)))),
         Functional = as.integer(as.character(factor(Functional, levels = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"), ordered = TRUE, labels = c(1, 2, 3, 4, 5, 6, 7, 8)))),
         FireplaceQu = as.integer(as.character(factor(FireplaceQu, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)))),
         GarageFinish = as.integer(as.character(factor(GarageFinish, levels = c("None", "Unf", "RFn", "Fin"), ordered = TRUE, labels = c(0, 1, 2, 3)))),
         GarageQual = as.integer(as.character(factor(GarageQual, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)))),
         GarageCond = as.integer(as.character(factor(GarageCond, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4, 5)))),
         PoolQC = as.integer(as.character(factor(PoolQC, levels = c("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)))),
         Fence = as.integer(as.character(factor(Fence, levels = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"), ordered = TRUE, labels = c(0, 1, 2, 3, 4)))))

# Number of remaining character variables :
test %>% select_if(is.character) %>% length()

# Changing remaining character variables to factor variables
test <- test %>% mutate_if(is.character, as.factor)

# Converting "Year" variables to date variables :
test <- test %>% mutate(YearBuilt = parse_date_time(YearBuilt, orders = "Y"),
                                    YearRemodAdd = parse_date_time(YearRemodAdd, orders = "Y"),
                                    GarageYrBlt = parse_date_time(GarageYrBlt, orders = "Y"),
                                    DateSold = make_date(year = YrSold, month = MoSold))

# Adding duration variables :
test <- test %>% mutate(durationBltSold = difftime(DateSold, YearBuilt, units = "weeks"),
                                    durationRemodSold = difftime(DateSold, YearRemodAdd, units = "weeks"))





### Checking factor levels in train and test sets
# We must be sure we have the same number of levels of in both sets.
# Checking the levels of factors in train and test sets :
a <- train %>% summarise_if(is.factor, nlevels)

b <- test %>% summarise_if(is.factor, nlevels)

a == b

# To be sure we have the same number of levels in both sets, we bind the sets by row and reordering the levels.
temp_df <- rbind(train[, -80], test)

temp_df <- temp_df %>% mutate_if(is.factor, as.factor)

# Getting the train set reordered
train_reordered <- temp_df[1:1460, ]

# We add the 'SalePrice' column from the previous train set :
train_reordered$SalePrice <- train$SalePrice

test <- temp_df[1461:nrow(temp_df), ]


# Now we can see that all levels are the same :
c <- train_reordered %>% summarise_if(is.factor, nlevels)

d <- test %>% summarise_if(is.factor, nlevels)

c == d

# Everything is clean. We rename 'train_reordered' with a shorter name and reuse the name 'train' :
train <- train_reordered

# Our dataset is clean. We have no more NAs, and the same number of levels in train and test sets.
# We can start our analysis.



### Analysis of the sale price variable ####


# Distribution of Sale Prices
ggplot(train, aes(SalePrice)) +
  geom_histogram() +
  ggtitle("Distribution of Sale Prices")

ggplot(train, aes(log(SalePrice))) +
  geom_histogram() +
  ggtitle("Distribution of LOG Sale Prices")

# Summary of Sale Prices Distribution
summary(train$SalePrice)

# Boxplot of Sale Prices by location
ggplot(train, aes(reorder(Neighborhood, SalePrice), SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = mean(SalePrice), color = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Sale Prices by location",
       col = "Overall Mean")

# Plot of Sale Prices by Total Basement Area
ggplot(train, aes(TotalBsmtSF, SalePrice, color = Neighborhood)) +
  geom_point(position = "jitter") +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("Sale Prices by Total Basement Area")

# Plot of Sale Prices according to year of construction. We also check which houses have been remodeled.
train %>% mutate(Remodel_diff = YearRemodAdd - YearBuilt,
                 Remodel_true = ifelse(Remodel_diff > 0, TRUE, FALSE)) %>%
  ggplot(aes(factor(YearBuilt), SalePrice, color = Remodel_true)) +
  geom_boxplot() +
  theme(axis.text.x = element_blank()) +
  ggtitle("Sale price according to year of construction")


### Bivariate normal distribution between 'SalePrice' and predictors ####

# Selecting only numerical variables
num_data <- select_if(train, is.numeric)
length(num_data)

# Creating a grid plot with all numerical variables against the 'SalePrice' :
grid_num <- lapply(1:ncol(num_data[, -52]),
                   function(col) ggplot2::qplot(x = num_data[[col]],
                                                y = train$SalePrice,
                                                geom = "point",
                                                xlab = names(num_data)[[col]],
                                                ylab = ""))

cowplot::plot_grid(plotlist = grid_num)

# Numerical variables with bivariate normal distribution (selection for better visualization)
selected_num <- num_data %>% dplyr::select(OverallQual, OverallCond, ExterQual, BsmtFinSF1, TotalBsmtSF, X1stFlrSF, X2ndFlrSF,
                                           GrLivArea, KitchenQual, TotRmsAbvGrd, GarageFinish, GarageCars, GarageArea, 
                                           FullBath, HeatingQC) 

grid_num_selected <-lapply(1:ncol(selected_num),
                           function(col) ggplot2::qplot(x = selected_num[[col]],
                                                        y = train$SalePrice,
                                                        geom = "point",
                                                        xlab = names(selected_num)[[col]]))

cowplot::plot_grid(plotlist = grid_num_selected) 

# Selecting only categorical variables
categ_data <- select_if(train, is.factor)
length(categ_data)

# Creating a grid plot with all categorical variables against the 'SalePrice' :
list_factors <-lapply(1:ncol(categ_data),
                      function(col) ggplot2::qplot(reorder(categ_data[[col]], train$SalePrice), train$SalePrice,
                                                   geom = "boxplot",
                                                   xlab = names(categ_data)[[col]]))

cowplot::plot_grid(plotlist = list_factors)


### Correlation matrix and PCA ####

# Changing date data as numeric
train <- train %>% mutate(YearBuilt = as.numeric(YearBuilt),
                          YearRemodAdd = as.numeric(YearRemodAdd),
                          GarageYrBlt = as.numeric(GarageYrBlt),
                          DateSold = as.numeric(DateSold),
                          durationBltSold = as.numeric(durationBltSold),
                          durationRemodSold = as.numeric(durationRemodSold))

test <- test %>% mutate(YearBuilt = as.numeric(YearBuilt),
                          YearRemodAdd = as.numeric(YearRemodAdd),
                          GarageYrBlt = as.numeric(GarageYrBlt),
                          DateSold = as.numeric(DateSold),
                          durationBltSold = as.numeric(durationBltSold),
                          durationRemodSold = as.numeric(durationRemodSold))

# Getting correlation matrix for all numerical variables
train_num <- train %>% select_if(is.numeric)
length(train_num)

train_num_correl <- cor(train_num[, -1])# The 'Id' variable (the 1st column) will not be useful

# Creating a matrix of p-value to get rid of insignificant values on the correlation matrix
library(ggcorrplot)
p_mat <- cor_pmat(train_num[, -1])

# Plot of the correlations
ggcorrplot(as.matrix(train_num_correl), 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.5, 
           p.mat = p_mat, 
           insig = "blank")

# Find highly correlated variables
high_corr <- findCorrelation(as.matrix(train_num_correl), cutoff = 0.75, names = TRUE)
print(high_corr)

# Feature Importance
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(SalePrice~., data = train_num, method = "leapForward", preProcess = "scale", trControl = control)
importance <- varImp(model, scale = FALSE)
plot(importance)

roc_imp <- filterVarImp(x = train_num[, -train_num$SalePrice], y = train_num$SalePrice)
print(roc_imp)


# Removing multicollinearity with recipes
library(recipes)
train_recipe <- recipe(SalePrice ~ ., data = train) %>%
  step_corr(all_numeric(), -Id, -all_outcomes(), threshold = 0.8) %>%
  step_normalize(all_numeric(), -Id, -all_outcomes()) %>%
  step_dummy(all_nominal())

train_recipe_prep <- train_recipe %>% prep(training = train)

train_recipe_baked <- train_recipe_prep %>% bake(new_data = NULL)
# PCA with numerical variables
pca_num <- prcomp(train_recipe_prep[, -c(1, 49)]) # We put out 'SalePrice' (outcome).

summary(pca_num)

# Plotting PCA results
library(factoextra)

fviz_pca_var(pca_num, select.var = list(contrib = 5), repel = TRUE)


# Checking contribution of variables for PC1 and PC2 :
# PC1
fviz_contrib(pca_num, choice = "var", axes = 1, top = 20)

# PC2
fviz_contrib(pca_num, choice = "var", axes = 2, top = 20)

# Screeplot to visualize the "elbow" to determine the number of components (2 components)
fviz_screeplot(pca_num)

# Applying Kaiser-Guttman rule to determine the number of components (eigenvalue > 1) : (16 components)
get_eigenvalue(pca_num) %>% filter(eigenvalue > 1)

# Parallel analysis to determine the number of components to retain 
# Parallel analysis :
paran_output <- paran(train_recipe_prep[,-c(1, 49)], seed = 69, graph = TRUE)

# Number of components to retain : (9 components)
paran_output$Retained


# Extracting principal components :
train_pc <- pca_num$x[, 1:16] %>% as.data.frame()

# Adding the outcome :
train_pc <- train_pc %>% cbind(train$SalePrice) %>% rename(SalePrice = `train$SalePrice` )


### Training the models ####

### Data partition and cross-validation plan

# Splitting in train and dev sets

set.seed(69, sample.kind = "Rounding")

dev_index <- createDataPartition(train$SalePrice, times = 1, p = 0.8, list = FALSE)

train_set <- train[dev_index,]
dev_set <-  train[-dev_index,]


# Train and test sets with 9 components :
train_9 <- train_pc[, 1:9] %>% cbind(train$SalePrice) %>% rename(SalePrice = `train$SalePrice`)
dev_9 <- train_pc[, 1:9] %>% cbind(train$SalePrice) %>% rename(SalePrice = `train$SalePrice`)

train_9 <- train_9[dev_index,]
dev_9 <- dev_9[-dev_index,]
# Train and test sets with 17 components :
train_16 <- train_pc[dev_index,]
dev_16 <- train_pc[-dev_index,]

# Creating a cross-validation plan
cv_plan <- trainControl(method = "cv", number = 10)


### Transformation of the outcome 'SalePrice' with log function

# Distribution of 'SalePrice' with mean and median
train %>% ggplot(aes(SalePrice)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(SalePrice), color = "mean")) +
  geom_vline(aes(xintercept = median(SalePrice), color = "median")) +
  scale_color_manual(name = "Legend", values = c(mean = "black", median = "red")) +
  ggtitle("Right-skewed distribution")


# Log transformation of 'SalePrice' for better predictions
train %>% ggplot(aes(log(SalePrice))) +
  geom_density() +
  geom_vline(aes(xintercept = mean(log(SalePrice)), color = "mean")) +
  geom_vline(aes(xintercept = median(log(SalePrice)), color = "median")) +
  scale_color_manual(name = "Legend", values = c(mean = "black", median = "red")) +
  ggtitle("Normal distribution with log transformation of the outcome")


# We compare predictions of linear models with and without log transformation of the outcome
# Linear model with 'SalePrice' as outcome to predict :
model_lm <- lm(SalePrice ~ GrLivArea * OverallQual,
               train)

pred_lm <- predict(model_lm, test_set)
RMSE(log(pred_lm), log(test_set$SalePrice))

# Linear model with 'log(SalePrice)' as outcome to predict :
model_lm_log <- lm(log(SalePrice) ~ GrLivArea * OverallQual,
                   train)

pred_lm_log <- predict(model_lm_log, test_set)
RMSE(pred_lm_log, log(test_set$SalePrice))

# The RMSE is better when we transform the outcome in our model with the `log` function 
# rather than transforming our predictions afterwards.



### First model : Linear regression ####

# Training a linear model
model_lm <- train(log(SalePrice) ~ ., 
                  method = "lm", 
                  trControl = cv_plan, 
                  preProcess = c("nzv", "center", "scale", "pca"),
                  data = train_set)

# Predicting results
pred_lm <- predict(model_lm, dev_set)

(rmse_lm <- RMSE(log(dev_set$SalePrice), pred_lm))

# Plot of predictions with linear model
dev_set %>% cbind(pred_lm) %>% 
  ggplot(aes(pred_lm, log(SalePrice))) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("Linear regression model predictions vs actual values")


### Second model : GLMnet ####

# Training a GLMnet model
set.seed(69, sample.kind = "Rounding")

model_glmnet <- train(log(SalePrice) ~ ., 
                      train_set,
                      tuneGrid = expand.grid(alpha = 0:1,
                                             lambda = seq(0.0001, 1, length = 20)),
                      method = "glmnet",
                      trControl = cv_plan,
                      preProcess = c("nzv", "center", "scale"))

# Plot of ridge and lasso parameters
plot(model_glmnet)

# Predictions of GLMnet model :
pred_glmnet <- predict(model_glmnet, dev_set)

(rmse_glmnet <- RMSE(log(dev_set$SalePrice), pred_glmnet))


# Plot of predictions of linear model
test_set %>% cbind(pred_glmnet) %>% 
  ggplot(aes(pred_glmnet, log(SalePrice))) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("GLMnet model predictions vs actual values")

### Third model : randomForest ####

# Training a randomForest model
model_rf <- train(log(SalePrice) ~ ., 
                  tuneLength = 10, 
                  data = train_set, 
                  method = "ranger",
                  trControl = cv_plan)

# Plot of the tuning of 10 randomForest models
plot(model_rf)

# Best tune for the randomForest model :
model_rf$bestTune

# Predictions of the randomForest model :
pred_rf <- predict(model_rf, dev_set)

(rmse_rf <- RMSE(log(dev_set$SalePrice), pred_rf))


# Plot of the randomForest predictions
test_set %>% cbind(pred_rf) %>% 
  ggplot(aes(pred_rf, log(SalePrice))) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("randomForest model predictions vs actual values")


### Fourth model : Generalized Additive Model (GAM) ####

# Transformation of predictor : an example with 'GrLivArea' against 'SalePrice'
# Transforming predictor : squared and cubic
fmla_sqr <- SalePrice ~ I(GrLivArea^2)

fmla_cub <- SalePrice ~ I(GrLivArea^3)

# Fitting a model of price as a function of squared area and cubic area
model_sqr <- lm(fmla_sqr, train)

model_cub <- lm(fmla_cub, train)

# Fitting a model of price as a linear function of 'GrLivArea'
model_lin <- lm(SalePrice ~ GrLivArea, train)


# Making predictions and comparing
train %>% mutate(linear = predict(model_lin), # predictions from linear model
                 squared = predict(model_sqr),        # predictions from quadratic model
                 cubic = predict(model_cub)) %>%      # predictions from cubic model
  gather(key = modeltype, value = pred, linear, squared, cubic) %>% # gather the predictions
  ggplot(aes(x = GrLivArea)) + 
  geom_point(aes(y = SalePrice)) +  # actual prices
  geom_line(aes(y = pred, color = modeltype)) + # the predictions
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Predictor transformation : Comparing models")


# Comparing RMSE of the three models :
train %>% 
  mutate(linear = predict(model_lin),   # predictions from linear model
         squared = predict(model_sqr),  # predictions from quadratic model
         cubic = predict(model_cub)) %>%    # predictions from cubic model
  gather(key = modeltype, value = pred, linear, squared, cubic) %>%
  group_by(modeltype) %>%
  summarize(rmse = RMSE(log(SalePrice), log(pred)))


# Training a GAM
model_gam <- gam(log(SalePrice) ~ Neighborhood + OverallQual + OverallCond + 
                   GrLivArea + GarageCars + s(GarageArea) + ExterQual + s(TotalBsmtSF) +
                   KitchenQual + FullBath + s(X1stFlrSF) + MSSubClass + MSZoning + 
                   TotRmsAbvGrd + RoofStyle + SaleType + SaleCondition + Condition1, 
                 family = gaussian, # Important !
                 train_set)

# Predictions of GAM :
pred_gam <- predict(model_gam, dev_set)

# RMSE for GAM :
(rmse_gam <- RMSE(log(dev_set$SalePrice), pred_gam))

# GAM predictions VS actual values plot :
dev_set %>% cbind(pred_gam) %>% 
  ggplot(aes(pred_gam, log(SalePrice))) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("GAM predictions vs actual values")



### Fifth model : XGBoost ####

# Dummifying categorical variables of the train set (One-hot encoding)
library(vtreat)

# Defining categorical predictors to dummify
vars <- names(categ_data)

# Creating the treatment plan
treatplan <- designTreatmentsZ(train_set, vars)

# Checking the scoreFrame
scoreFrame <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%
  dplyr::select(varName, origName, code)

# We only want the rows with code "lev"
newvars <- scoreFrame %>%
  filter(code == "lev") %>%
  magrittr::use_series(varName)

# Creating the treated training data
df_treat <- vtreat::prepare(treatplan, train_set, varRestriction = newvars)

# Finally, we add the new binary variables with the numerical variables.
train_treat <- train_set[, -c(1, 82)] %>% select_if(is.numeric) %>% cbind(df_treat)

# Preparing categorical variables of the test set and converting them into binary variables
df_dev_treat <- vtreat::prepare(treatplan, dev_set, varRestriction = newvars)

dev_treat <- dev_set[, -c(1, 82)] %>% select_if(is.numeric) %>% cbind(df_dev_treat)


# Tuning hyperparmaters for XGBoost model :
xgb_grid <- expand.grid(nrounds = c(200, 500, 800, 1000),
                        max_depth = c(2,3,4,5,6,7,8), # default = 6
                        eta = c(0.05,0.1,0.2,0.3,0.4),# default = 0.3
                        gamma = 1,
                        colsample_bytree = 1, # default = 1
                        min_child_weight = 1, # default = 1
                        subsample = 1) # default = 1


set.seed(69, sample.kind = "Rounding") # We must set the seed to get the same results

# For tuning with the 'caret' package, we add the 'SalePrice' column to the 'train_treat' dataset.
# Remember that 'SalePrice' outcome must not be part of the dataset when training with 'xgboost' function or it
# will give overoptimistic results.

train_treat_tune <- train_treat  %>% cbind(train_set$SalePrice) %>% rename(SalePrice = `train_set$SalePrice`)

# Be careful ! Running time for this code is around 40 minutes !
xgb_tune <-train(log(SalePrice) ~ .,
                 data = train_treat_tune,
                 method = "xgbTree",
                 trControl = cv_plan, # 10-fold cross-validation plan
                 tuneGrid = xgb_grid, # hyperparameters we want to tune
                 verbose = FALSE,
                 metric = "RMSE",
                 nthread =3)

# Plot of the XGBoost tuning
plot(xgb_tune)

# XGBoost best tune
xgb_tune$bestTune

# Training an XGBoost model
model_xgb <- xgboost(data = as.matrix(train_treat), # training data as matrix without the 'SalePrice' outcome
                     label = log(train_set$SalePrice),  # we must use the original column of train_set
                     nrounds = 200,       # number of trees to build
                     objective = "reg:squarederror", # for regression
                     eta = 0.1,
                     max_depth = 3,
                     verbose = 0)  # silent

# Predictions of the XGBoost model
pred_xgb <- predict(model_xgb, as.matrix(test_treat))

(rmse_xgb <- RMSE(log(test_set$SalePrice), pred_xgb))

# XGBoost predictions versus actual values
test_treat %>% cbind(pred_xgb) %>% ggplot(aes(pred_xgb, log(test_set$SalePrice))) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("XGBoost predictions vs actual values")


### Training models with 11 principal components ####

# Linear model
lm_9 <- lm(log(SalePrice) ~., train_9)

pred_lm_9 <- predict(lm_9, dev_9)

(rmse_lm_9 <- RMSE(log(dev_9$SalePrice), pred_lm_9))

# GLMnet model
model_glmnet_9 <- train(log(SalePrice) ~ ., 
                         train_9,
                         tuneGrid = expand.grid(alpha = 0:1,
                                                lambda = seq(0.0001, 1, length = 20)),
                         method = "glmnet",
                         trControl = cv_plan)

pred_glmnet_9 <- predict(model_glmnet_9, dev_9)

(rmse_glmnet_9 <- RMSE(log(dev_9$SalePrice), pred_glmnet_9))

# RandomForest model
rf_9 <- randomForest(log(SalePrice) ~ .,
                      train_9)

pred_rf_9 <- predict(rf_9, dev_9)

(rmse_rf_9 <- RMSE(log(test_9$SalePrice), pred_rf_9))

# GAM
model_gam_9 <- train(log(SalePrice) ~ ., 
                      method = "gam", 
                      trControl = cv_plan,
                      train_9)

pred_gam_9 <- predict(model_gam_9, dev_9)

(rmse_gam_9 <- RMSE(log(dev_9$SalePrice), pred_gam_9))

# XGBoost
# Tuning model
xgb_tune_9 <-train(log(SalePrice) ~ .,
                 data = train_9,
                 method = "xgbTree",
                 trControl = cv_plan, # 10-fold cross-validation plan
                 tuneGrid = xgb_grid, # hyperparameters we want to tune
                 verbose = FALSE,
                 metric = "RMSE",
                 nthread =3)

# Plot of the XGBoost tuning
plot(xgb_tune_9)

# XGBoost best tune
xgb_tune_9$bestTune


model_xgb_9 <- xgboost(data = as.matrix(train_9[, -10]), 
                        label = log(train_set$SalePrice),
                        nrounds = 200, 
                        objective = "reg:squarederror",
                        eta = 0.4,
                        max_depth = 2,
                        verbose = 0)

pred_xgb_9 <- predict(model_xgb_9, as.matrix(dev_9[, -10]))

(rmse_xgb_9 <- RMSE(log(test_set$SalePrice), pred_xgb_9))


### Training models with 16 principal components ####

# Linear model
lm_16 <- lm(log(SalePrice) ~., train_16)

pred_lm_16 <- predict(lm_16, dev_16)

(rmse_lm_16 <- RMSE(log(dev_16$SalePrice), pred_lm_16))


# GLMnet model
model_glmnet_16 <- train(log(SalePrice) ~ ., 
                         train_16,
                         tuneGrid = expand.grid(alpha = 0:1,
                                                lambda = seq(0.0001, 1, length = 20)),
                         method = "glmnet",
                         trControl = cv_plan)

pred_glmnet_16 <- predict(model_glmnet_16, dev_16)

(rmse_glmnet_16 <- RMSE(log(dev_16$SalePrice), pred_glmnet_16))


# RandomForest model
rf_16 <- randomForest(log(SalePrice) ~ .,
                      train_16)

pred_rf_16 <- predict(rf_16, dev_16)

(rmse_rf_16 <- RMSE(log(dev_16$SalePrice), pred_rf_16))

# GAM
model_gam_16 <- train(log(SalePrice) ~ ., 
                      method = "gam", 
                      trControl = cv_plan,
                      train_16)

pred_gam_16 <- predict(model_gam_16, dev_16)

(rmse_gam_16 <- RMSE(log(dev_16$SalePrice), pred_gam_16))

# XGBoost
# Tuning hyperparameters
xgb_tune_16 <-train(log(SalePrice) ~ .,
                 data = train_16,
                 method = "xgbTree",
                 trControl = cv_plan, # 10-fold cross-validation plan
                 tuneGrid = xgb_grid, # hyperparameters we want to tune
                 verbose = FALSE,
                 metric = "RMSE",
                 nthread =3)

# Plot of the XGBoost tuning
plot(xgb_tune_16)

# XGBoost best tune
xgb_tune_16$bestTune


model_xgb_16 <- xgboost(data = as.matrix(train_16[, -17]), 
                        label = log(train_set$SalePrice), 
                        nrounds = 200, 
                        objective = "reg:squarederror", 
                        eta = 0.4,
                        max_depth = 4,
                        verbose = 0)

pred_xgb_16 <- predict(model_xgb_16, as.matrix(dev_16[, -17]))

(rmse_xgb_16 <- RMSE(log(dev_16$SalePrice), pred_xgb_16))


### Sum up of results of the different train sets (original variables, 9 PCs, and 16 PCs) ####

data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "GAM", "XGBoost"),
           RMSE_original_train = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_gam, rmse_xgb),
           RMSE_9_components_train = c(rmse_lm_9, rmse_glmnet_9, rmse_rf_9, rmse_gam_9, rmse_xgb_9),
           RMSE_16_components_train = c(rmse_lm_16, rmse_glmnet_16, rmse_rf_16, rmse_gam_16, rmse_xgb_16))


### Ensemble ####
# Creating an ensemble of the 3 best models : GLMnet, randomForest and XGBoost
ensemble <- (pred_glmnet + pred_rf + pred_xgb) / 3

# RMSE of the ensemble
(rmse_ensemble <- RMSE(log(dev_set$SalePrice), ensemble))

### Tensorflow Model ###

library(tensorflow)
library(keras)
library(caret)


# Diviser les données en features (X) et en labels (y)
X <- train_9[, -10]
y <- train_9$SalePrice

# Fractionner les données en ensembles d'entraînement et de dev
set.seed(69)
split <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
X_train <- X[split, ]
y_train <- y[split]
X_dev <- X[-split, ]
y_dev <- y[-split]


# Normaliser les données
preproc <- preProcess(X_train, method = c("center", "scale"))
X_train <- predict(preproc, X_train)
X_dev <- predict(preproc, X_dev)

# Créer le modèle
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)


# Compiler le modèle
model %>% compile(optimizer = "adam", loss = "rmse")

# Entraîner le modèle
model %>% keras::fit(
  x = X_train,
  y = y_train,
  epochs = 10,
  batch_size = 32,
  test_data = list(X_dev, y_dev)
)


colnames(X_dev) <- c("input")
# Évaluer le modèle sur l'ensemble de test
loss <- model %>% evaluate(X_test, y_test)
print(paste("Loss:", loss))

# Faire des prédictions sur de nouvelles données
new_data <- read.csv("chemin/vers/vos/nouvelles/donnees.csv")
new_data <- predict(preproc, new_data)
predictions <- model %>% predict(new_data)


library(keras)

# Définir les noms de colonnes pour les entrées
colnames <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9")

# Créer les entrées du modèle
inputs <- layer_input(shape = dim(train_16[, -17])[2], name = "input")
x <- inputs

# Ajouter les couches cachées
x <- layer_dense(x, units = 1024, activation = "relu")
x <- layer_dense(x, units = 1024, activation = "relu")
x <- layer_dense(x, units = 1024, activation = "relu")

# Ajouter la couche de sortie
outputs <- layer_dense(x, units = 1)

# Créer le modèle
model <- keras_model(inputs, outputs)

# Compiler le modèle
model %>% compile(optimizer = "adam", loss = "mean_squared_error")

# Entraîner le modèle
history <- model %>% fit(
  x = as.matrix(train_16[, -17]),
  y = log(train_16$SalePrice),
  epochs = 50,
  batch_size = 16,
  validation_split = 0.2
)

# Effectuer des prédictions sur le jeu de données de test
predictions <- model %>% predict(as.matrix(dev_16[, -17]))

# Comparer les prédictions avec les valeurs réelles
actual_values <- log(dev_16$SalePrice)

# Calculer l'erreur de prédiction (RMSE)
rmse <- sqrt(mean((predictions - actual_values)^2))
cat("RMSE:", rmse, "\n")

# Vous pouvez également afficher d'autres métriques d'évaluation, par exemple, l'erreur absolue moyenne (MAE)
mae <- mean(abs(predictions - actual_values))
cat("MAE:", mae, "\n")


### test ####

test_prep <- train_recipe_prep %>% bake(new_data = test)
## Predictions of GLMnet

pred_test_glmnet <- predict(model_glmnet, test)

## Predicting 'SalePrice' with the randomForest model 

pred_test_rf <- predict(model_rf, test)

## Predictions of XGBoost model

# Creating the treated test data
df_test_treat <- vtreat::prepare(treatplan, test, varRestriction = newvars)

# Finally, we add the new binary variables with the numerical variables
test_treat <- test[, -1] %>% select_if(is.numeric) %>% 
  cbind(df_test_treat)

# Predictions of the XGBoost model
pred_test_xgb <- predict(model_xgb, as.matrix(test_treat))



## Creating the ensemble

ensemble_val <- (pred_test_glmnet + pred_test_rf + pred_test_xgb) / 3


# Selecting Id and predictions for submission. We must not forget to use the exponential
# function to get the real values of sale prices.

submission_ensemble <- test %>% mutate(SalePrice = exp(ensemble_val)) %>% 
  dplyr::select(Id, SalePrice)

# Saving the submission
write.csv(submission_ensemble, "Submission Ensemble.csv", row.names = FALSE)


### Final result ####
# Comparison of RMSEs between train and test sets

data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "XGBoost", "Ensemble", "XGBoost 11 PC"),
           RMSE_original_train = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_xgb, rmse_ensemble, rmse_xgb_11),
           RMSE_test = c(0.15088, 0.14341, 0.14376, 0.13568, 0.13224, 0.76273))


comparison_rmse <- data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "XGBoost", "Ensemble", "XGBoost 11 PC", "Linear", "GLMnet", "randomForest", "XGBoost", "Ensemble", "XGBoost 11 PC"),
                              RMSE = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_xgb, rmse_ensemble, rmse_xgb_11, 0.15088, 0.14341, 0.14376, 0.13568, 0.13224, 0.76273),
                              dataset = c("train", "train", "train", "train", "train", "train", "test", "test", "test", "test", "test", "test"))


# Bar plot of RMSEs between train and test sets
comparison_rmse %>% ggplot(aes(Model_type, RMSE, fill = dataset)) +
  geom_col(position = "dodge") +
  ggtitle("Comparing RMSE between train and test sets")
