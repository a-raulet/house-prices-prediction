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

# Downloading the GitHub repository with the train and validation sets from GitHub to Rstudio environment

setwd("~")

url_zip_gitrepo <- "https://github.com/a-raulet/house-prices-prediction/archive/master.zip"

download.file(url_zip_gitrepo, "house.zip")

unzip("house.zip", exdir = "~")

# Train

setwd("~/house-prices-prediction-master")
train <- read.csv("train.csv", stringsAsFactors = FALSE)

# Validation

validation <- read.csv("test.csv", stringsAsFactors = FALSE)


### Data Exploration ####

# Structure of train set
str(train)


# Structure of validation set
str(validation)

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
train$PoolQC[is.na(train$PoolQC)] <- "None"
train$Fence[is.na(train$Fence)] <- "None"
train$BsmtQual[is.na(train$BsmtQual)] <- "None"
train$BsmtCond[is.na(train$BsmtCond)] <- "None"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "None"
train$BsmtFinType1[is.na(train$BsmtFinType2)] <- "None"
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "None"
train$FireplaceQu[is.na(train$FireplaceQu)] <- "None"
train$GarageFinish[is.na(train$GarageFinish)] <- "None"
train$GarageQual[is.na(train$GarageQual)] <- "None"
train$GarageCond[is.na(train$GarageCond)] <- "None"

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

### The 'LotFrontage' variable
# LotFrontage variable : Where are the NAs ?
train %>% ggplot(aes(BldgType, OverallQual, color = is.na(LotFrontage))) +
  geom_jitter()

library(missMDA)
# Creating matrix with numerical variables
num_matrix <- select_if(train, is.numeric) %>% as.matrix()

# Estimation of the optimal number of dimensions
number_cp <- estim_ncpPCA(num_matrix)

# Imputing values instead of NAs
complete_matrix <- imputePCA(num_matrix, ncp = number_cp$ncp, scale = TRUE)

# Extracting the complete matrix
clean_matrix <- complete_matrix$completeObs

# Changing the complete matrix as a data frame
clean_num <- as.data.frame(clean_matrix)

# Replacing NAs in the train set for 'LotFrontage' variable :
train$LotFrontage[is.na(train$LotFrontage)] <- clean_num$LotFrontage[is.na(train$LotFrontage)]


# We replace the only NA in Electrical variable with the most common value : "SBrkr"
train %>% ggplot(aes(Electrical)) +
  geom_bar()

train$Electrical[is.na(train$Electrical)] <- "SBrkr"

# Changing 'CentralAir' variable with '0' and '1' instead of 'N' and 'Y' :
train <- train %>% mutate(CentralAir = ifelse(CentralAir == "Y", 1, 0))

# Converting numerical variable 'MSSubClass' to factor variable :
train$MSSubClass <- factor(train$MSSubClass)

# Changing character ordinal variables to numerical ordinal variables
train <- train %>% mutate(LotShape = as.integer(as.character(factor(LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), ordered = TRUE, labels = c(1, 2, 3, 4)))),  
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
train %>% select_if(is.character) %>% length()

# Changing remaining character variables to factor variables
train <- train %>% mutate_if(is.character, as.factor)

# Converting "Year" variables to date variables :
train <- train %>% mutate(YearBuilt = parse_date_time(YearBuilt, orders = "Y"),
                          YearRemodAdd = parse_date_time(YearRemodAdd, orders = "Y"),
                          GarageYrBlt = parse_date_time(GarageYrBlt, orders = "Y"),
                          DateSold = make_date(year = YrSold, month = MoSold))

# Adding duration variables :
train <- train %>% mutate(durationBltSold = difftime(DateSold, YearBuilt, units = "weeks"),
                          durationRemodSold = difftime(DateSold, YearRemodAdd, units = "weeks"))



### Cleaning validation set ####


miss_var_summary(validation) %>% filter(n_miss > 0)

# Barplot of variables with missing values
gg_miss_var(validation)

# Imputing "None" for 15 variables.
# For ordinal variables :
validation$PoolQC[is.na(validation$PoolQC)] <- "None"
validation$Fence[is.na(validation$Fence)] <- "None"
validation$BsmtQual[is.na(validation$BsmtQual)] <- "None"
validation$BsmtCond[is.na(validation$BsmtCond)] <- "None"
validation$BsmtExposure[is.na(validation$BsmtExposure)] <- "None"
validation$BsmtFinType1[is.na(validation$BsmtFinType2)] <- "None"
validation$BsmtFinType2[is.na(validation$BsmtFinType2)] <- "None"
validation$FireplaceQu[is.na(validation$FireplaceQu)] <- "None"
validation$GarageFinish[is.na(validation$GarageFinish)] <- "None"
validation$GarageQual[is.na(validation$GarageQual)] <- "None"
validation$GarageCond[is.na(validation$GarageCond)] <- "None"

# For categorical variables :
validation$MiscFeature[is.na(validation$MiscFeature)] <- "None"
validation$Alley[is.na(validation$Alley)] <- "None"
validation$GarageType[is.na(validation$GarageType)] <- "None"
validation$MasVnrType[is.na(validation$MasVnrType)] <- "None"

# For numerical variables :
# If MasVnrType missing data are "None", then area must be 0.
validation$MasVnrArea[is.na(validation$MasVnrArea)] <- 0

# We replace GarageYrBlt by the YearBuilt value
validation$GarageYrBlt[is.na(validation$GarageYrBlt)] <- validation$YearBuilt[is.na(validation$GarageYrBlt)]

### Lot Frontage variable
# LotFrontage variable : Where are the NAs ?
validation %>% ggplot(aes(BldgType, OverallQual, color = is.na(LotFrontage))) +
  geom_jitter()

# Creating matrix with numerical variables
num_matrix_val <- select_if(validation, is.numeric) %>% as.matrix()

# Estimation of the optimal number of dimensions
number_cp_val <- estim_ncpPCA(num_matrix_val)

# Imputing values instead of NAs
complete_matrix_val <- imputePCA(num_matrix, ncp = number_cp$ncp, scale = TRUE)

# Extracting the complete matrix
clean_matrix_val <- complete_matrix_val$completeObs

# Changing the complete matrix as a data frame
clean_num_val <- as.data.frame(clean_matrix_val)

# Replacing NAs in the train set for 'LotFrontage' variable :
validation$LotFrontage[is.na(validation$LotFrontage)] <- clean_num_val$LotFrontage[is.na(validation$LotFrontage)]


### Exterior variables
# NA of Exterior1st and 2nd :
which(is.na(validation$Exterior1st))

# We replace the only NA in the 692th row in Exterior1st and 2nd with the most common value
# after considering Exterior Quality : 
validation %>% filter(ExterQual == "TA") %>% ggplot(aes(Exterior1st)) +
  geom_bar()

validation$Exterior1st[is.na(validation$Exterior1st)] <- "HdBoard"

validation$Exterior2nd[is.na(validation$Exterior2nd)] <- "MetalSd"

### MSZoning variable
# Where are NAs for MSZoning ?
which(is.na(validation$MSZoning))

validation %>% 
  ggplot(aes(factor(Neighborhood), OverallQual, col = factor(MSZoning))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For NAs located in "IDOTRR" :
validation$MSZoning[is.na(validation$MSZoning)] <- "RM"

# For NA located in "Mitchel" :
validation$MSZoning[is.na(validation$MSZoning)] <- "RL"

### KitchenQual variable
# Where is the NA for KitchenQual ?
which(is.na(validation$KitchenQual))

validation %>% 
  ggplot(aes(factor(OverallCond), OverallQual, col = factor(KitchenQual))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

validation$KitchenQual[is.na(validation$KitchenQual)] <- "TA"

### Garage variables
# Where is the NA for GarageCars ?
which(is.na(validation$GarageCars))

validation %>% 
  ggplot(aes(factor(GarageType), GarageQual, col = factor(GarageCars))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

validation %>% 
  ggplot(aes(factor(GarageCond), GarageQual, col = factor(GarageCars))) +
  geom_jitter()

validation$GarageCars[is.na(validation$GarageCars)] <- 1

# Where is the NA for GarageArea ?
which(is.na(validation$GarageArea))

validation %>% filter(GarageArea > 0) %>% group_by(GarageType) %>% 
  summarize(min = min(GarageArea), median = median(GarageArea))

validation %>% 
  ggplot(aes(GarageType, GarageArea)) +
  geom_boxplot()

validation$GarageArea[is.na(validation$GarageArea)] <- 384


# NAs of Functional :
which(is.na(validation$Functional))

validation %>% ggplot(aes(Functional)) +
  geom_bar()

validation$Functional[is.na(validation$Functional)] <- "Typ"

# NAs of Utilities :
which(is.na(validation$Utilities))

validation %>% ggplot(aes(Utilities)) +
  geom_bar()

validation$Utilities[is.na(validation$Utilities)] <- "AllPub"


### Basement variables
# NAs of "Basement" variables (2 rows). No basement, so all areas equal to 0. So is the number of baths.
which(is.na(validation$BsmtFinSF1)) 
which(is.na(validation$BsmtFinSF2))
which(is.na(validation$BsmtUnfSF))
which(is.na(validation$TotalBsmtSF))
which(is.na(validation$BsmtFullBath))
which(is.na(validation$BsmtHalfBath))


validation$BsmtFinSF1[is.na(validation$BsmtFinSF1)] <- 0
validation$BsmtFinSF2[is.na(validation$BsmtFinSF2)] <- 0
validation$BsmtUnfSF[is.na(validation$BsmtUnfSF)] <- 0
validation$TotalBsmtSF[is.na(validation$TotalBsmtSF)] <- 0
validation$BsmtFullBath[is.na(validation$BsmtFullBath)] <- 0
validation$BsmtHalfBath[is.na(validation$BsmtHalfBath)] <- 0

### SaleType variable
# NA of "SaleType" : most common value "WD" is input.
which(is.na(validation$SaleType))

validation %>% ggplot(aes(SaleType)) +
  geom_bar()

validation$SaleType[is.na(validation$SaleType)] <- "WD"

# Changing 'CentralAir' variable with '0' and '1' :
validation <- validation %>% mutate(CentralAir = ifelse(CentralAir == "Y", 1, 0))

# Converting numerical variable 'MSSubClass' to factor variable :
validation$MSSubClass <- factor(validation$MSSubClass)

# Changing character ordinal variables to numerical ordinal variables
validation <- validation %>% 
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
validation %>% select_if(is.character) %>% length()

# Changing remaining character variables to factor variables
validation <- validation %>% mutate_if(is.character, as.factor)

# Converting "Year" variables to date variables :
validation <- validation %>% mutate(YearBuilt = parse_date_time(YearBuilt, orders = "Y"),
                                    YearRemodAdd = parse_date_time(YearRemodAdd, orders = "Y"),
                                    GarageYrBlt = parse_date_time(GarageYrBlt, orders = "Y"),
                                    DateSold = make_date(year = YrSold, month = MoSold))

# Adding duration variables :
validation <- validation %>% mutate(durationBltSold = difftime(DateSold, YearBuilt, units = "weeks"),
                                    durationRemodSold = difftime(DateSold, YearRemodAdd, units = "weeks"))





### Checking factor levels in train and validation sets
# We must be sure we have the same number of levels of in both sets.
# Checking the levels of factors in train and validation sets :
a <- train %>% summarise_if(is.factor, nlevels)

b <- validation %>% summarise_if(is.factor, nlevels)

a == b

# To be sure we have the same number of levels in both sets, we bind the sets by row and reordering the levels.
temp_df <- rbind(train[, -81], validation)

temp_df <- temp_df %>% mutate_if(is.factor, as.factor)

# Getting the train set reordered
train_reordered <- temp_df[1:1460, ]

# We add the 'SalePrice' column from the previous train set :
train_reordered$SalePrice <- train$SalePrice

validation <- temp_df[1461:nrow(temp_df), ]


# Now we can see that all levels are the same :
c <- train_reordered %>% summarise_if(is.factor, nlevels)

d <- validation %>% summarise_if(is.factor, nlevels)

c == d

# Everything is clean. We rename 'train_reordered' with a shorter name and reuse the name 'train' :
train <- train_reordered

# Our dataset is clean. We have no more NAs, and the same number of levels in train and validation sets.
# We can start our analysis.



### Analysis of the sale price variable ####


# Distribution of Sale Prices
ggplot(train, aes(SalePrice)) +
  geom_histogram() +
  ggtitle("Distribution of Sale Prices")

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
grid_num <- lapply(2:ncol(num_data[, -54]),
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


# PCA with numerical variables
pca_num <- prcomp(train_num[, -c(1, 54)], center = TRUE, scale. = TRUE) # We put out 'Id' (useless) and 'SalePrice' (outcome).

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

# Applying Kaiser-Guttman rule to determine the number of components (eigenvalue > 1) : (17 components)
get_eigenvalue(pca_num) %>% filter(eigenvalue > 1)

# Parallel analysis to determine the number of components to retain 
# Parallel analysis :
paran_output <- paran(train_num[,-c(1, 54)], seed = 69, graph = TRUE)

# Number of components to retain : (11 components)
paran_output$Retained


# Extracting principal components :
train_pc <- pca_num$x[, 1:17] %>% as.data.frame()

# Adding the outcome :
train_pc <- train_pc %>% cbind(train$SalePrice) %>% rename(SalePrice = `train$SalePrice` )


### Training the models ####

### Data partition and cross-validation plan

# Splitting in train and test sets

set.seed(69, sample.kind = "Rounding")

test_index <- createDataPartition(train$SalePrice, times = 1, p = 0.8, list = FALSE)

train_set <- train[test_index,]
test_set <-  train[-test_index,]


# Train and test sets with 11 components :
train_11 <- train_pc[, 1:11] %>% cbind(train$SalePrice) %>% rename(SalePrice = `train$SalePrice`)
test_11 <- train_pc[, 1:11] %>% cbind(train$SalePrice) %>% rename(SalePrice = `train$SalePrice`)

train_11 <- train_11[test_index,]
test_11 <- test_11[-test_index,]
# Train and test sets with 17 components :
train_17 <- train_pc[test_index,]
test_17 <- train_pc[-test_index,]

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
pred_lm <- predict(model_lm, test_set)

(rmse_lm <- RMSE(log(test_set$SalePrice), pred_lm))

# Plot of predictions with linear model
test_set %>% cbind(pred_lm) %>% 
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
pred_glmnet <- predict(model_glmnet, test_set)

(rmse_glmnet <- RMSE(log(test_set$SalePrice), pred_glmnet))


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
pred_rf <- predict(model_rf, test_set)

(rmse_rf <- RMSE(log(test_set$SalePrice), pred_rf))


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
  geom_point(aes(y = SalePrice, label = Id)) +  # actual prices
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
pred_gam <- predict(model_gam, test_set)

# RMSE for GAM :
(rmse_gam <- RMSE(log(test_set$SalePrice), pred_gam))

# GAM predictions VS actual values plot :
test_set %>% cbind(pred_gam) %>% 
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
df_treat <- prepare(treatplan, train_set, varRestriction = newvars)

# Finally, we add the new binary variables with the numerical variables.
train_treat <- train_set[, -c(1, 84)] %>% select_if(is.numeric) %>% cbind(df_treat)

# Preparing categorical variables of the test set and converting them into binary variables
df_test_treat <- prepare(treatplan, test_set, varRestriction = newvars)

test_treat <- test_set[, -c(1, 84)] %>% select_if(is.numeric) %>% cbind(df_test_treat)


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
                     nrounds = 800,       # number of trees to build
                     objective = "reg:squarederror", # for regression
                     eta = 0.05,
                     max_depth = 4,
                     verbose = 0)  # silent

# Predictions of the XGBoost model
pred_xgb <- predict(model_xgb, as.matrix(test_treat))

(rmse_xgb <- RMSE(log(test_set$SalePrice), pred_xgb))

# XGBoost predictions versus actual values
test_treat %>% cbind(pred_xgb) %>% ggplot(aes(pred_xgb, log(SalePrice))) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("XGBoost predictions vs actual values")


### Training models with 11 principal components ####

# Linear model
lm_11 <- lm(log(SalePrice) ~., train_11)

pred_lm_11 <- predict(lm_11, test_11)

(rmse_lm_11 <- RMSE(log(test_11$SalePrice), pred_lm_11))

# GLMnet model
model_glmnet_11 <- train(log(SalePrice) ~ ., 
                         train_11,
                         tuneGrid = expand.grid(alpha = 0:1,
                                                lambda = seq(0.0001, 1, length = 20)),
                         method = "glmnet",
                         trControl = cv_plan)

pred_glmnet_11 <- predict(model_glmnet_11, test_11)

(rmse_glmnet_11 <- RMSE(log(test_11$SalePrice), pred_glmnet_11))

# RandomForest model
rf_11 <- randomForest(log(SalePrice) ~ .,
                      train_11)

pred_rf_11 <- predict(rf_11, test_11)

(rmse_rf_11 <- RMSE(log(test_11$SalePrice), pred_rf_11))

# GAM
model_gam_11 <- train(log(SalePrice) ~ ., 
                      method = "gam", 
                      trControl = cv_plan,
                      train_11)

pred_gam_11 <- predict(model_gam_11, test_11)

(rmse_gam_11 <- RMSE(log(test_11$SalePrice), pred_gam_11))

# XGBoost
model_xgb_11 <- xgboost(data = as.matrix(train_11[, -12]), 
                        label = log(train_set$SalePrice),
                        nrounds = 800, 
                        objective = "reg:squarederror",
                        eta = 0.05,
                        max_depth = 5,
                        verbose = 0)

pred_xgb_11 <- predict(model_xgb_11, as.matrix(test_11[, -12]))

(rmse_xgb_11 <- RMSE(log(test_set$SalePrice), pred_xgb_11))


### Training models with 17 principal components ####

# Linear model
lm_17 <- lm(log(SalePrice) ~., train_17)

pred_lm_17 <- predict(lm_17, test_17)

(rmse_lm_17 <- RMSE(log(test_11$SalePrice), pred_lm_17))


# GLMnet model
model_glmnet_17 <- train(log(SalePrice) ~ ., 
                         train_17,
                         tuneGrid = expand.grid(alpha = 0:1,
                                                lambda = seq(0.0001, 1, length = 20)),
                         method = "glmnet",
                         trControl = cv_plan)

pred_glmnet_17 <- predict(model_glmnet_17, test_17)

(rmse_glmnet_17 <- RMSE(log(test_17$SalePrice), pred_glmnet_17))


# RandomForest model
rf_17 <- randomForest(log(SalePrice) ~ .,
                      train_17)

pred_rf_17 <- predict(rf_17, test_17)

(rmse_rf_17 <- RMSE(log(test_17$SalePrice), pred_rf_17))

# GAM
model_gam_17 <- train(log(SalePrice) ~ ., 
                      method = "gam", 
                      trControl = cv_plan,
                      train_17)

pred_gam_17 <- predict(model_gam_17, test_17)

(rmse_gam_17 <- RMSE(log(test_17$SalePrice), pred_gam_17))

# XGBoost
model_xgb_17 <- xgboost(data = as.matrix(train_17[, -18]), 
                        label = log(train_set$SalePrice), 
                        nrounds = 800, 
                        objective = "reg:squarederror", 
                        eta = 0.05,
                        max_depth = 4,
                        verbose = 0)

pred_xgb_17 <- predict(model_xgb_17, as.matrix(test_17[, -18]))

(rmse_xgb_17 <- RMSE(log(test_set$SalePrice), pred_xgb_17))


### Sum up of results of the different train sets (original variables, 11 PCs, and 17 PCs) ####

data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "GAM", "XGBoost"),
           RMSE_original_train = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_gam, rmse_xgb),
           RMSE_11_components_train = c(rmse_lm_11, rmse_glmnet_11, rmse_rf_11, rmse_gam_11, rmse_xgb_11),
           RMSE_17_components_train = c(rmse_lm_17, rmse_glmnet_17, rmse_rf_17, rmse_gam_17, rmse_xgb_17))


### Ensemble ####
# Creating an ensemble of the 3 best models : GLMnet, randomForest and XGBoost
ensemble <- (pred_glmnet + pred_rf + pred_xgb) / 3

# RMSE of the ensemble
(rmse_ensemble <- RMSE(log(test_set$SalePrice), ensemble))


### Validation ####
## Predictions of GLMnet

pred_val_glmnet <- predict(model_glmnet, validation)

## Predicting 'SalePrice' with the randomForest model 

pred_val_rf <- predict(model_rf, validation)

## Predictions of XGBoost model

# Creating the treated validation data
df_val_treat <- prepare(treatplan, validation, varRestriction = newvars)

# Finally, we add the new binary variables with the numerical variables
validation_treat <- validation[, -1] %>% select_if(is.numeric) %>% 
  cbind(df_val_treat)

# Predictions of the XGBoost model
pred_val_xgb <- predict(model_xgb, as.matrix(validation_treat))



## Creating the ensemble

ensemble_val <- (pred_val_glmnet + pred_val_rf + pred_val_xgb) / 3


# Selecting Id and predictions for submission. We must not forget to use the exponential
# function to get the real values of sale prices.

submission_ensemble <- validation %>% mutate(SalePrice = exp(ensemble_val)) %>% 
  dplyr::select(Id, SalePrice)

# Saving the submission
write.csv(submission_ensemble, "Submission Ensemble.csv", row.names = FALSE)


### Final result ####
# Comparison of RMSEs between train and validation sets

data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "XGBoost", "Ensemble", "XGBoost 11 PC"),
           RMSE_original_train = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_xgb, rmse_ensemble, rmse_xgb_11),
           RMSE_validation = c(0.15088, 0.14341, 0.14376, 0.13568, 0.13224, 0.76273))


comparison_rmse <- data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "XGBoost", "Ensemble", "XGBoost 11 PC", "Linear", "GLMnet", "randomForest", "XGBoost", "Ensemble", "XGBoost 11 PC"),
                              RMSE = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_xgb, rmse_ensemble, rmse_xgb_11, 0.15088, 0.14341, 0.14376, 0.13568, 0.13224, 0.76273),
                              dataset = c("train", "train", "train", "train", "train", "train", "validation", "validation", "validation", "validation", "validation", "validation"))


# Bar plot of RMSEs between train and validation sets
comparison_rmse %>% ggplot(aes(Model_type, RMSE, fill = dataset)) +
  geom_col(position = "dodge") +
  ggtitle("Comparing RMSE between train and validation sets")
