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



### Data Exploration ####

# Downloading the train and validation sets from GitHub to Rstudio environment

train <- read.csv("https://github.com/a-raulet/house-prices-prediction/blob/main/train.csv", 
                  stringsAsFactors = FALSE)

validation <- read.csv("https://github.com/a-raulet/house-prices-prediction/blob/main/test.csv", 
                       stringsAsFactors = FALSE)

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

# We must be sure we have the same number levels of in both sets.
# Checking the levels of factors in train and validation sets :
a <- train %>% summarise_if(is.factor, nlevels)

b <- validation %>% summarise_if(is.factor, nlevels)

a == b

# To be sure we have the same number of levels in both sets, we bind the sets by row and reordering the levels.
temp_df <- rbind(train[,-c(81:84)], validation[,-81])

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
ggplot(train, aes(Neighborhood, SalePrice)) +
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

# Selecting only categorical variables
categ_data <- select_if(train, is.factor)
length(categ_data)

# Creating a grid plot with all categorical variables against the 'SalePrice' :
list_factors <-lapply(1:ncol(categ_data_sp[,-25]),
                      function(col) ggplot2::qplot(categ_data_sp[[col]], train$SalePrice,
                                                   geom = "boxplot",
                                                   xlab = names(categ_data_sp)[[col]]))

cowplot::plot_grid(plotlist = list_factors)


### Correlation matrix and PCA ####

# Getting correlation matrix for all numerical variables
train_num <- train %>% select_if(is.numeric)
length(train_num)

train_num_correl <- cor(train_num[, -1])# The 'Id' variable (the 1st column) will not be useful

# Creating a matrix of p-value to get rid of insignificant values on the correlation matrix
p_mat <- cor_pmat(train_num[, -1])

# Plot of the correlations
library(ggcorrplot)
ggcorrplot(train_num_correl, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, lab_size = 1.5, 
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

# Screeplot to visualize the "elbow" to determine the number of components
fviz_screeplot(pca_num)

# Applying Kaiser-Guttman rule to determine the number of components (eigenvalue > 1) :
get_eigenvalue(pca_num) %>% filter(eigenvalue > 1)

# Parallel analysis to determine the number of components to retain
# Parallel analysis :
paran_output <- paran(train_num[,-c(1, 54)], seed = 69, graph = TRUE)

# Number of components to retain :
paran_output$Retained



## Training the models

### Data partition and cross-validation plan

# Splitting in train and test sets

set.seed(69, sample.kind = "Rounding")

test_index <- createDataPartition(train_reordered$SalePrice, times = 1, p = 0.8, list = FALSE)

train_set <- train_reordered[test_index,]
test_set <- train_reordered[-test_index,]