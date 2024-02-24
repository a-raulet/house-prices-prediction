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
library(dplyr)
library(tibble)
library(tidyverse)
library(caret)
library(data.table)
library(naniar)
library(missMDA)
library(ggcorrplot)
library(factoextra)
library(paran)
library(randomForest)
library(gam)
library(mgcv)
library(nlme)
library(xgboost)
library(vtreat)
library(cowplot)
library(glmnet)
library(magrittr)
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

# There are a lot of categorical variables, and we must be sure we have the
# same number of levels for each. We'll bind both train and test sets in
# order to train the train set with the right number of levels.

SalePrice <- train$SalePrice

train$set_type <- 0 # We set the type of this set to zero for the train set.
test$set_type <- 1 # We set it to 1 for the test set.

big_set <- rbind(train[, -81], test) # We need to remove the SalePrice column to get the same number of columns.


# Number of categorical variables
categ_data <- select_if(big_set, is.character)
length(categ_data)

for(i in colnames(categ_data)){
  print(i)
  print(unique(categ_data[, i]))
}

# Number of numerical variables
num_data <- select_if(big_set, is.numeric)
length(num_data)


### Cleaning and Missing Data ####

library(naniar)
# Checking missing value
sum(is.na(big_set))

miss_var_summary(big_set) %>% filter(n_miss > 0) %>% print(n = 34)

# Barplot of variables with missing values
gg_miss_var(big_set)


# According to data description most NAs mean actually "None"
# Imputing "None" for 10 variables.
# For ordinal variables :
big_set$Fence[is.na(big_set$Fence)] <- "None"
big_set$BsmtQual[is.na(big_set$BsmtQual)] <- "None"
big_set$BsmtCond[is.na(big_set$BsmtCond)] <- "None"
big_set$BsmtExposure[is.na(big_set$BsmtExposure)] <- "None"
big_set$BsmtFinType1[is.na(big_set$BsmtFinType1)] <- "None"
big_set$BsmtFinType2[is.na(big_set$BsmtFinType2)] <- "None"
big_set$FireplaceQu[is.na(big_set$FireplaceQu)] <- "None"
big_set$GarageFinish[is.na(big_set$GarageFinish)] <- "None"
big_set$GarageQual[is.na(big_set$GarageQual)] <- "None"
big_set$GarageCond[is.na(big_set$GarageCond)] <- "None"


# The case of PoolQC. Let's check if there are NAs for a pool area greater than zero.
which(is.na(big_set$PoolQC) & big_set$PoolArea > 0)

big_set %>% 
  ggplot(aes(factor(OverallQual), PoolArea, col = factor(PoolQC))) +
  geom_jitter() +
  geom_text(label = big_set$Id) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# There is one observation with a good overall quality, and two with
# a below average or fair quality. Moreover, there is one category mentioned
# in the description but not present in the data : "TA".
# We can suppose that the 2504th observation has a good pool quality, the 2421th
# has an average quality, and the 2600th has a fair quality. The other observations
# can be set to "None".

big_set$PoolQC[2504] <- "Gd"
big_set$PoolQC[2421] <- "TA"
big_set$PoolQC[2600] <- "Fa"

big_set$PoolQC[is.na(big_set$PoolQC)] <- "None"
# For other categorical variables :
# MiscFeature Variable :
which(is.na(big_set$MiscFeature))

big_set %>% 
  ggplot(aes(factor(MSZoning), MiscVal, col = factor(MiscFeature))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# As we can see on the plot, there is one NA that has the maximum value.
# Moreover, there is one category mentioned in the description but not displayed
# here : "Elev" for Elevator. We can suppose this NA corresponds to the Elevator.
# The other NA must have no additional feature as the value is zero.
which(big_set$MiscVal == max(big_set$MiscVal) & is.na(big_set$MiscFeature))

big_set$MiscFeature[2550] <- "Elev"
big_set$MiscFeature[is.na(big_set$MiscFeature)] <- "None"


big_set$Alley[is.na(big_set$Alley)] <- "None"
big_set$GarageType[is.na(big_set$GarageType)] <- "None"

# For 'MasVnrType', the description mentions one category that is not present here : 'CBlock'.
# We can suppose that one the NAs corresponds to this category.
which(is.na(big_set$MasVnrType))
which(is.na(big_set$MasVnrArea))

# NAs in the veneer area column correspond to the NAs in the veneer type.
# There is one observation that have an veneer area superior to zero. We can suppose it
# is the lacking category "Cblock".
which(is.na(big_set$MasVnrType) & big_set$MasVnrArea > 0)

big_set$MasVnrType[2611] <- "CBlock"

# For other observations, hard to say if there is a veneer type or not.
# We'll suppose that NAs are "None", and thus, the veneer area will be zero, as the area is also NA for these rows.
which(is.na(big_set$MasVnrArea) & is.na(big_set$MasVnrType))

big_set %>% 
  ggplot(aes(factor(Neighborhood), MasVnrArea, col = factor(MasVnrType))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

big_set$MasVnrType[is.na(big_set$MasVnrType)] <- "None"
big_set$MasVnrArea[is.na(big_set$MasVnrArea)] <- 0

### Basement variables
# NAs of "Basement" variables (2 rows). No basement, so all areas equal to 0. 
# So is the number of baths.
which(is.na(big_set$BsmtFinSF1)) 
which(is.na(big_set$BsmtFinSF2))
which(is.na(big_set$BsmtUnfSF))
which(is.na(big_set$TotalBsmtSF))
which(is.na(big_set$BsmtFullBath))
which(is.na(big_set$BsmtHalfBath))


big_set$BsmtFinSF1[is.na(big_set$BsmtFinSF1)] <- 0
big_set$BsmtFinSF2[is.na(big_set$BsmtFinSF2)] <- 0
big_set$BsmtUnfSF[is.na(big_set$BsmtUnfSF)] <- 0
big_set$TotalBsmtSF[is.na(big_set$TotalBsmtSF)] <- 0
big_set$BsmtFullBath[is.na(big_set$BsmtFullBath)] <- 0
big_set$BsmtHalfBath[is.na(big_set$BsmtHalfBath)] <- 0

# We replace GarageYrBlt by the YearBuilt value
big_set$GarageYrBlt[is.na(big_set$GarageYrBlt)] <- big_set$YearBuilt[is.na(big_set$GarageYrBlt)]

# We replace the only NA in Electrical variable with the most common value : "SBrkr"
big_set %>% ggplot(aes(Electrical)) +
  geom_bar()

big_set$Electrical[is.na(big_set$Electrical)] <- "SBrkr"

# Changing 'CentralAir' variable with '0' and '1' instead of 'N' and 'Y' :
big_set <- big_set %>% mutate(CentralAir = ifelse(CentralAir == "Y", 1, 0))

# Converting numerical variable 'MSSubClass' to factor variable :
big_set$MSSubClass <- factor(big_set$MSSubClass)
unique(big_set$MSSubClass)

### Exterior variables
# NA of Exterior1st and 2nd :
which(is.na(big_set$Exterior1st))
which(is.na(big_set$Exterior2nd))

sort(unique(big_set$Exterior1st))
sort(unique(big_set$Exterior2nd))

# We need to correct some values in Exterior2nd
big_set$Exterior2nd[big_set$Exterior2nd == "CmentBd"] <- "CemntBd"
big_set$Exterior2nd[big_set$Exterior2nd == "Brk Cmn"] <- "BrkComm"
big_set$Exterior2nd[big_set$Exterior2nd == "Wd Shng"] <- "WdShing"

# The data description mentions there is one category called "PreCast" that we don't see here.
# We can suppose the only NA corresponds to this category.

big_set$Exterior1st[is.na(big_set$Exterior1st)] <- "PreCast"

big_set$Exterior2nd[is.na(big_set$Exterior2nd)] <- "PreCast"

# Checking levels
unique(big_set$Exterior1st)
unique(big_set$Exterior2nd)

### MSZoning variable
# Where are NAs for MSZoning ?
which(is.na(big_set$MSZoning))

big_set %>% 
  ggplot(aes(factor(Neighborhood), OverallQual, col = factor(MSZoning))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The data description mentions two types of zones that are not displayed here : Industrial (I) and
# Residential Low Density Park (RP).
# We can suppose that both zones correspond to the NAs.
# For NAs located in "IDOTRR" :
big_set$MSZoning[is.na(big_set$MSZoning)] <- "I"

# For NA located in "Mitchel" :
big_set$MSZoning[is.na(big_set$MSZoning)] <- "RP"

### KitchenQual variable
# Where is the NA for KitchenQual ?
which(is.na(big_set$KitchenQual))

big_set %>% 
  ggplot(aes(factor(OverallCond), OverallQual, col = factor(KitchenQual))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

big_set$KitchenQual[is.na(big_set$KitchenQual)] <- "TA"

### Garage variables
# Where is the NA for GarageCars ?
which(is.na(big_set$GarageCars))

big_set %>% 
  ggplot(aes(factor(GarageType), GarageQual, col = factor(GarageCars))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

big_set %>% 
  ggplot(aes(factor(GarageCond), GarageQual, col = factor(GarageCars))) +
  geom_jitter()

# 1 seems to be the best value according to the plot
big_set$GarageCars[is.na(big_set$GarageCars)] <- 1

# Where is the NA for GarageArea ?
which(is.na(big_set$GarageArea))

# The Garage Type is "Detchd"
big_set$GarageType[2577]

big_set %>% filter(GarageArea > 0) %>% group_by(GarageType) %>% 
  summarize(min = min(GarageArea), median = median(GarageArea))

big_set %>% 
  ggplot(aes(GarageType, GarageArea)) +
  geom_boxplot()

# We impute the median area for "Detchd" garage type
big_set$GarageArea[is.na(big_set$GarageArea)] <- 400


# NAs of Functional :
which(is.na(big_set$Functional))

big_set %>% ggplot(aes(Functional)) +
  geom_bar()

# The description mentions one category that is not displayed here : "Sal"
# We can suppose it corresponds to the NAs.
big_set$Functional[is.na(big_set$Functional)] <- "Sal"

# NAs of Utilities :
which(is.na(big_set$Utilities))

big_set %>% ggplot(aes(Utilities)) +
  geom_bar()

big_set %>% 
  ggplot(aes(factor(MSZoning), OverallQual, col = factor(Utilities))) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# The data description mentions two other types of Utilities : NoSewr and ELO
which(big_set$MSZoning == "I" & is.na(big_set$Utilities))
big_set$Utilities[1916] <- "NoSewr"

big_set$Utilities[1946] <- "ELO"

### SaleType variable
# NA of "SaleType" : the description mentions the 'VWD' category but does not appear.
# We can suppose it corresponds to this NA.
which(is.na(big_set$SaleType))

big_set %>% ggplot(aes(SaleType)) +
  geom_bar()

big_set$SaleType[is.na(big_set$SaleType)] <- "VWD"


# To avoid multicollinearity with predictors that give the same information
# when creating dummy variables, let's modify a little Exterior2nd, depending on Exterior1st,
# and Condition2, depending on Condition1.

big_set <- big_set |> mutate(Condition2 = ifelse(Condition2 == Condition1, "Same", Condition2),
                             Exterior2nd = ifelse(Exterior2nd == Exterior1st, "Same", Exterior2nd))


# Changing character ordinal variables to numerical ordinal variables
big_set <- big_set %>% mutate(LotShape = factor(LotShape, levels = c("IR3", "IR2", "IR1", "Reg"), ordered = TRUE, labels = c(1, 2, 3, 4)),  
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
                              PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), ordered = TRUE, labels = c(0, 1, 2)),
                              Exterior1st = factor(Exterior1st, levels = c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock",  "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone",  
                                                                           "Stucco",  "VinylSd", "Wd Sdng", "WdShing")),
                              Exterior2nd = factor(Exterior2nd, levels = c("AsbShng", "AsphShn", "BrkComm", "BrkFace", "CBlock",  "CemntBd", "HdBoard", "ImStucc", "MetalSd", "Other", "Plywood", "PreCast", "Stone",  
                                                                           "Stucco",  "VinylSd", "Wd Sdng", "WdShing", "Same")),
                              Condition1 = factor(Condition1, levels = c("Artery", "Feedr",  "Norm", "PosA", "PosN", "RRAe", "RRAn", "RRNe", "RRNn")),
                              Condition2 = factor(Condition2, levels = c("Artery", "Feedr",  "Norm", "PosA", "PosN", "RRAe", "RRAn", "RRNe", "RRNn", "Same")))


# Number of remaining character variables :
big_set %>% select_if(is.character) %>% length()

# Changing remaining character variables to factor variables
big_set <- big_set %>% mutate_if(is.character, factor)

### Checking factor levels in train and test sets
# We must be sure we have the same number of levels of in both sets.
# Checking the levels of factors in train and test sets :
a <- train %>% summarise_if(is.factor, nlevels)

b <- test %>% summarise_if(is.factor, nlevels)

a == b

str(big_set)
glimpse(big_set)

miss_var_summary(big_set) |> filter(n_miss > 0)


# For LotFrontage, we will impute missing values in with KNN in the recipe.

# Our dataset is almost clean. Before creating a recipe, let's take a closer look
# at the data.

### Analysis of the sale price variable ####


# Distribution of Sale Prices
ggplot(train, aes(SalePrice)) +
  geom_histogram() +
  ggtitle("Distribution of Sale Prices")

ggplot(train, aes(log(SalePrice))) +
  geom_histogram() +
  ggtitle("Distribution of LOG Sale Prices")

# Boxplot of Sale Prices
ggplot(train, aes(SalePrice)) +
  geom_boxplot() +
  coord_flip()

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
grid_num <- lapply(2:ncol(num_data[, -52]), # Except the 'Id' column
                   function(col) ggplot2::qplot(x = num_data[[col]],
                                                y = train$SalePrice,
                                                geom = "point",
                                                xlab = names(num_data)[[col]],
                                                ylab = ""))

cowplot::plot_grid(plotlist = grid_num)

# Numerical variables with bivariate normal distribution (selection for better visualization)
selected_num <- num_data %>% dplyr::select(OverallQual, OverallCond, BsmtFinSF1, TotalBsmtSF, X1stFlrSF, X2ndFlrSF,
                                           GrLivArea, TotRmsAbvGrd, GarageCars, GarageArea, 
                                           FullBath) 

grid_num_selected <-lapply(2:ncol(selected_num),
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

pred_lm <- predict(model_lm, dev)
RMSE(log(pred_lm), log(dev$SalePrice))

# Linear model with 'log(SalePrice)' as outcome to predict :
model_lm_log <- lm(log(SalePrice) ~ GrLivArea * OverallQual,
                   train)

pred_lm_log <- predict(model_lm_log, dev)
RMSE(pred_lm_log, log(dev$SalePrice))

# The RMSE is better when we transform the outcome in our model with the `log` function 
# rather than transforming our predictions afterwards.

### Feature Engineering
# It looks like some predictors may be correlated. Let's check that.
# Detecting multicollinearity between predictors

library(corrplot)
train_matrix <- select_if(train, is.numeric)
corr_matrix <- cor(as.matrix(train_matrix))

corrplot(corr_matrix, method = "number", sig.level = 0.05)

library(ggcorrplot)
#p_mat <- cor_pmat(train_num[, -1])

ggcorrplot(corr_matrix, 
           method = "circle", 
           #hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.5, 
           #p.mat = p_mat, 
           insig = "blank")


# Feature Engineering : 'Area' variables.
# There is some multicollinearity here, and they're all more or less
# correlated to the SalePrice. Let's create a big area variable that sums them all.
# We'll do the same for the number of bathrooms.
# We'll also create two new variables that reflect the quality of the basement by square-feet according
# its type.

big_set <- big_set |> mutate(Total_Area = TotalBsmtSF + X1stFlrSF + X2ndFlrSF + GarageArea,
                             Total_Bath = FullBath + HalfBath + BsmtHalfBath + BsmtFullBath,
                             Bsmt_PercentQual_type1 = BsmtFinSF1/TotalBsmtSF * as.numeric(BsmtFinType1),
                             Bsmt_PercentQual_type2 = BsmtFinSF2/TotalBsmtSF * as.numeric(BsmtFinType2)) 


big_set <- big_set %>% dplyr::select(!c(TotalBsmtSF, X1stFlrSF, X2ndFlrSF, GarageArea, FullBath, HalfBath, BsmtHalfBath, BsmtFullBath,
                                        BsmtFinSF1, BsmtFinSF2, BsmtFinType1, BsmtFinType2))

# Lastly, let's check the NAs for the LotFrontage variable, by neighborhood.

ggplot(big_set, aes(reorder(Neighborhood, LotArea), LotFrontage, color = LotShape)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Most of the data points seem to have more or less a regular shape. The LotFrontage is almost
# the same across the neighborhoods. We'll remove NAs by imputing new values with a KNN algorithm
# in a recipe.

miss_var_summary(big_set) |> filter(n_miss > 0)

# Our new variables have NAs. Let's check them.

big_set |> filter(is.na(Bsmt_PercentQual_type1))

# It looks like the NAs appeared for houses with no basement. We'll replace NAs with zeros.

big_set$Bsmt_PercentQual_type1[is.na(big_set$Bsmt_PercentQual_type1)] <- 0
big_set$Bsmt_PercentQual_type2[is.na(big_set$Bsmt_PercentQual_type2)] <- 0

miss_var_summary(big_set) |> filter(n_miss > 0)

# Let's check also if sales are higher depending on the month of the year.

ggplot(train, aes(as.factor(MoSold))) + geom_bar()

#It seems that houses are sold mostly between spring and summer.

# Let's split now the train and test sets.

train <- big_set[big_set$set_type == 0, ]
train$set_type <- NULL
train$SalePrice <- SalePrice

test <- big_set[big_set$set_type == 1, ]
test$set_type <- NULL

# Train - Dev Split
#n = floor(0.8 * nrow(train))
#index = sample(seq_len(nrow(train)), size = n)

set.seed(69)
index = createDataPartition(train$SalePrice, p = 0.9, times = 1, list = FALSE)
train = train[index,]
dev = train[-index,]

### Recettes
# Let's create a recipe based on what we saw previously.
library(recipes)

base_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              Season = case_when(MoSold >= 1 & MoSold < 3 & MoSold == 12 ~ "Winter",
                                 MoSold >= 3 & MoSold < 6 ~ "Spring",
                                 MoSold >= 6 & MoSold < 9 ~ "Summer",
                                 .default = "Autumn")) %>%
  step_mutate(Season = as.factor(Season)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(LotFrontage, neighbors = 5, impute_with = imp_vars(all_predictors())) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold)) %>%
  step_dummy(all_nominal_predictors())

base_prep <- base_recipe %>% prep()

print(base_prep)

transformed_base_train <- base_prep %>% bake(new_data = NULL)

print(transformed_base_train)

miss_var_summary(transformed_base_train)

# Our dataset is clean. We have no more NAs, and the same number of levels in train and test sets.
# We can start our analysis.


### Training the models ####
### First model : Linear regression ####
# Simple linear regression model with lm() function

lm_mod <- lm(SalePrice ~ . -Id, data = transformed_base_train)
summary(lm_mod)

plot(lm_mod)

ggplot(transformed_base_train, aes(fitted(lm_mod), SalePrice)) +
  geom_point() + 
  geom_abline(color = "blue")

RMSE(fitted(lm_mod), transformed_base_train$SalePrice) # 0.100

baked_dev <- base_prep |> bake(new_data = dev)
pred_lm <- predict(lm_mod, newdata = baked_dev)

(rmse_lm <- RMSE(pred_lm, baked_dev$SalePrice)) # 0.076


# Detecting multicollinearity between predictors
library(plm)
detect.lindep(transformed_base_train)

#suspicious_feat <- transformed_base_train[, c(16, 18, 19, 20, 23, 24, 25, 26, 66, 123, 145, 148, 162, 53)]
#suspicious_feat <- transformed_base_train[, c(69, 119, 122, 125, 150, 164, 187, 198)]
#suspicious_feat <- transformed_base_train[, c(17, 19, 20, 21, 25, 26, 27, 28, 69, 125, 147, 150, 164)]
#suspicious_feat <- transformed_base_train[, c(7, 19, 20, 21, 25, 26, 27, 28, 69, 71, 76, 119, 120, 121, 122, 123, 124, 125, 126, 127, 129, 159, 161, 170, 177, 185, 204, 205, 206, 207, 208, 21)]
#suspicious_feat <- transformed_base_train[, c(60, 62, 67, 110, 111, 112, 113, 114, 115, 116, 117, 118, 120, 150, 152, 161, 168, 176, 195, 196, 197, 198, 199, 207)]
suspicious_feat <- transformed_base_train[, c(60, 62, 67, 110, 111, 112, 113, 114, 115, 116, 117, 118, 120, 134, 142, 150, 152, 160, 161, 168, 176, 195, 196, 197, 198, 199, 207)]


susp_corr_matrix <- cor(as.matrix(suspicious_feat))

corrplot(susp_corr_matrix, method = "number")

ggcorrplot(susp_corr_matrix, method = "circle")

# Let's remove the collinear variables 'BldgType_Duplex', 'Condition2_Same', 'MiscFeature_None', 
# 'Condition2_RRAe'. Let's also add a zero-variance step.


clean_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              Season = case_when(MoSold >= 1 & MoSold < 3 & MoSold == 12 ~ "Winter",
                                 MoSold >= 3 & MoSold < 6 ~ "Spring",
                                 MoSold >= 6 & MoSold < 9 ~ "Summer",
                                 .default = "Autumn")) %>%
  step_mutate(Season = as.factor(Season)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(LotFrontage, neighbors = 5, impute_with = imp_vars(all_predictors())) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BldgType_Duplex, Condition2_Same, MiscFeature_None, Condition2_RRAe)) %>%
  step_zv(all_predictors())

clean_prep <- clean_recipe %>% prep()

print(clean_prep)

clean_train <- clean_prep %>% bake(new_data = NULL)
clean_dev <- clean_prep %>% bake(new_data = dev)

clean_lm_mod <- lm(SalePrice ~ . -Id, data = clean_train)
summary(clean_lm_mod) %>% print()

plot(clean_lm_mod)

pred_clean_lm <- predict(clean_lm_mod, newdata = clean_dev)
(rmse_clean_lm <- RMSE(pred_clean_lm, clean_dev$SalePrice)) # 0.076


# By removing multicollinearity and zero-variance variables, our RMSE stayed the same. (0.076).
# Let's check multicollinearity again, but with other functions.

# Checking again multicollinearity, but with the vif() and kappa() function.
detect.lindep(clean_train)
# The detect.lindep() function does not find multicollinearity anymore.

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

# Condition number of (X.T X) : multicollinearity if greater than 30
kappa(clean_lm_mod, exact = TRUE)

# Both the vif function and the condition number kappa indicate there is still multicollinearity.
# We can remove more variables, as suggested by the plot
# or build a ridge-lasso regression model or a PCA.
# Let's first look at the correlation of variables with high VIF.
high_vif_var <- vif_df |> filter(vif > 30)

suspicious_feat <- clean_train[, high_vif_var$variable]

susp_corr_matrix <- cor(as.matrix(suspicious_feat))

corrplot(susp_corr_matrix, method = "number")

ggcorrplot(susp_corr_matrix, method = "circle")

# Let's reduce variables even more. We'll also create a new variable to measure the overall quality
# and condition with the living area above ground (as 'GrLivArea' is collinear with 'Total_Area').

reduced_clean_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              Season = case_when(MoSold >= 1 & MoSold < 3 & MoSold == 12 ~ "Winter",
                                 MoSold >= 3 & MoSold < 6 ~ "Spring",
                                 MoSold >= 6 & MoSold < 9 ~ "Summer",
                                 .default = "Autumn"),
              OverallQC_per_GrLivArea = GrLivArea / Total_Area * (OverallQual + OverallCond)) %>%
  step_mutate(Season = as.factor(Season)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(LotFrontage, neighbors = 5, impute_with = imp_vars(all_predictors())) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold, GrLivArea, OverallQual, OverallCond)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BldgType_Duplex, Condition2_Same, MiscFeature_None, Condition2_RRAe)) %>%
  step_zv(all_predictors()) %>%
  step_select(-c(RoofStyle_Hip, MSSubClass_X120, Condition1_Norm, GarageType_Attchd,
                 MSSubClass_X160, Exterior2nd_Same, HouseStyle_X1Story, MSZoning_RM, SaleCondition_Partial))

reduced_clean_prep <- reduced_clean_recipe %>% prep()

print(reduced_clean_prep)

reduced_clean_train <- reduced_clean_prep %>% bake(new_data = NULL)
reduced_clean_dev <- reduced_clean_prep %>% bake(new_data = dev)

reduced_clean_lm_mod <- lm(SalePrice ~ . -Id, data = reduced_clean_train)
summary(reduced_clean_lm_mod) %>% print()

pred_reduced_clean_lm <- predict(reduced_clean_lm_mod, newdata = reduced_clean_dev)
(rmse_redu_clean_lm <- RMSE(pred_reduced_clean_lm, reduced_clean_dev$SalePrice)) # 0.085

# Checking again multicollinearity
vif(reduced_clean_lm_mod)
vif_df = as.data.frame(vif(reduced_clean_lm_mod))
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
kappa(reduced_clean_lm_mod, exact = TRUE)

# If we remove too many variables, The RMSE goes up, but the condition number kappa decreases.
# Let's try to remove the variables with a high VIF in the recipe, that is, 'RoofMatl_CompShg',
# 'MiscFeature_Gar2', 'Heating_GasA', 'MSSubClass_X190' and 'BldgType_X2fmCon'.

reduced_clean_recipe <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              Season = case_when(MoSold >= 1 & MoSold < 3 & MoSold == 12 ~ "Winter",
                                 MoSold >= 3 & MoSold < 6 ~ "Spring",
                                 MoSold >= 6 & MoSold < 9 ~ "Summer",
                                 .default = "Autumn"),
              OverallQC_per_GrLivArea = GrLivArea / Total_Area * (OverallQual + OverallCond)) %>%
  step_mutate(Season = as.factor(Season)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(LotFrontage, neighbors = 5, impute_with = imp_vars(all_predictors())) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold, GrLivArea, OverallQual, OverallCond)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BldgType_Duplex, Condition2_Same, MiscFeature_None, Condition2_RRAe)) %>%
  step_zv(all_predictors()) %>%
  step_select(-c(RoofStyle_Hip, MSSubClass_X120, Condition1_Norm, GarageType_Attchd,
                 MSSubClass_X160, Exterior2nd_Same, HouseStyle_X1Story, MSZoning_RM, SaleCondition_Partial,
                 RoofMatl_CompShg, MiscFeature_Gar2, Heating_GasA, MSSubClass_X190, BldgType_X2fmCon))

reduced_clean_prep <- reduced_clean_recipe %>% prep()

print(reduced_clean_prep)

reduced_clean_train <- reduced_clean_prep %>% bake(new_data = NULL)
reduced_clean_dev <- reduced_clean_prep %>% bake(new_data = dev)

reduced_clean_lm_mod <- lm(SalePrice ~ . -Id, data = reduced_clean_train)
summary(reduced_clean_lm_mod) %>% print()

pred_reduced_clean_lm <- predict(reduced_clean_lm_mod, newdata = reduced_clean_dev)
(rmse_redu_clean_lm2 <- RMSE(pred_reduced_clean_lm, reduced_clean_dev$SalePrice)) # 0.094

# Condition number of (X.T X)
kappa(reduced_clean_lm_mod, exact = TRUE)

# The RMSE is still going up a bit, but the condition number kappa decreased to 190.
# We can also try to select the most important variables.

## Other methods for multicollinearity and Feature Importance
# Find highly correlated variables
red_num <- reduced_clean_train |> select_if(is.numeric)
corr_matrix2 <- cor(as.matrix(red_num))
high_corr <- findCorrelation(as.matrix(corr_matrix2), cutoff = 0.75, names = TRUE)
print(high_corr)

p_mat <- cor_pmat(red_num)

ggcorrplot(corr_matrix2, 
           method = "circle", 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.5, 
           p.mat = p_mat,
           sig.level = 0.05,
           insig = "blank")

# Feature Importance
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_leapF <- caret::train(SalePrice~., data = red_num, method = "leapForward", trControl = control)
importance <- varImp(model_leapF, scale = FALSE)
plot(importance)

roc_imp <- filterVarImp(x = red_num[, -red_num$SalePrice], y = red_num$SalePrice)
print(roc_imp)


### Importance des variables avec randomForest ou RFE (caret)

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
RMSE(pred_lm100, clean_dev100$SalePrice) # 0.094

# With a simple hypothesis of selecting the 100 most important features,
# the RMSE stays the same compared to the previous model (0.094).
# We can also select the 100 most important features from the summary of
# the reduced clean model 'reduced_clean_lm_mod'.

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

feat100 <- rownames(significant_vars)[1:100]

print(feat100)

feat100 <- feat100[2:42] #NAs after the 42th variable

clean_tr100 <- reduced_clean_train[, feat100]
clean_tr100$SalePrice <- reduced_clean_train$SalePrice

clean_dev100 <- reduced_clean_dev[, feat100]
clean_dev100$SalePrice <- reduced_clean_dev$SalePrice

lm100 <- lm(SalePrice ~ ., data = clean_tr100)

summary(lm100) %>% print()

pred_lm100 <- predict(lm100, newdata = clean_dev100)
RMSE(pred_lm100, clean_dev100$SalePrice)

# The RMSE is still higher (0.094), but with 42 predictors only this time. 
# (NAs found from the 43th to the 100th column)

library(tidymodels)
library(randomForest)

# Créer un modèle de pipeline avec recipes et randomForest
rf_spec <- rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_workflow <- workflow() %>%
  add_recipe(reduced_clean_recipe) %>%
  add_model(rf_spec)

# Ajuster le modèle
rf_fit <- rf_workflow %>%
  fit(data = train)

# Extraire l'importance des variables
#variable_importance <- rf_fit %>%
#  extract_fit_parsnip() %>%
#  randomForest::importance()

# Afficher l'importance des variables
#print(variable_importance)


### Learning Vector Quantization pour sélectionner variables (principalement numériques)

# prepare training scheme

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
#model_lvq <- caret::train(SalePrice ~ ., data= red_num, method="lvq", trControl=control)
# estimate variable importance
#importance <- varImp(model_lvq, scale=FALSE)
# summarize importance
#print(importance)
# plot importance
#plot(importance)

# ou autre méthode
# Ajuster le modèle de régression quantile linéaire
library(quantreg)
#quantreg_model <- rq(reduced_clean_prep, tau = 0.5)  # Choisissez le quantile désiré (ici 0.5 pour la médiane)
# Extraire les coefficients du modèle
#coefficients <- coef(quantreg_model)

# Afficher les coefficients et leur significativité
#print(coefficients)



# Feature selection with forward method
library(leaps)
reg = regsubsets(SalePrice ~ . -Id, data = reduced_clean_train, method = "forward", nvmax = 181)
reg_summ = summary(reg)

reg_summ$which
length(reg_summ$bic)
n = dim(clean_train)[1]

AIC = 2 * (2:182) + n * log(reg_summ$rss/n)
which.min(AIC)

# According to AIC, the forward method selects 89 predictors
plot(AIC ~ I(1:181), xlab = "nb of predictors", ylab = "AIC")
points(x = which.min(AIC), pch = 19, col = "blue")

# According to Adjusted R-squared, the forward method selects 103 predictors
plot(1:181, reg_summ$adjr2, xlab = "number of predictors", ylab = "Adj. R-squared")
which.max(reg_summ$adjr2)

BIC = log(n) * (2:182) + n * log(reg_summ$rss / n)
which.min(reg_summ$bic)
which.min(BIC)

# According to BIC, the forward method selects 42 predictors
plot(BIC ~ I(1:181), xlab = "nb of predictors", ylab = "BIC")
points(x = which.min(BIC), pch = 19, col = "blue")

plot(reg_summ$bic ~ I(1:181), xlab = "nb of predictors", ylab = "BIC")



# Feature selection with backward selection
reg = regsubsets(SalePrice ~ . -Id, data = reduced_clean_train, method = "backward", nvmax = 181)
reg_summ = summary(reg)

reg_summ$which
length(reg_summ$bic)
n = dim(reduced_clean_train)[1]

AIC = 2 * (2:182) + n * log(reg_summ$rss/n)
min_aic_index = which.min(AIC)

# According to AIC, the backward method selects 85 predictors
plot(AIC ~ I(1:181), xlab = "nb of predictors", ylab = "AIC")
points(x = min_aic_index, y = min(AIC), pch = 19, col = "blue")
abline(v = min_aic_index, col = "red", lty = 2)
text(x = min_aic_index, y = min(AIC) + 60, labels = paste("Predictors =", min_aic_index), col = "black")

# According to Adjusted R-squared, the backward method selects 117 predictors
plot(1:181, reg_summ$adjr2, xlab = "number of predictors", ylab = "Adj. R-squared")
which.max(reg_summ$adjr2)

BIC = log(n) * (2:182) + n * log(reg_summ$rss / n)
which.min(BIC)
which.min(reg_summ$bic)

# According to BIC, the backward method selects 41 predictors
plot(BIC ~ I(1:181), xlab = "nb of predictors", ylab = "BIC")
points(x = which.min(BIC), pch = 19, col = "blue")

plot(reg_summ$bic ~ I(1:179), xlab = "nb of predictors", ylab = "BIC")

# To resume,
# For forward selection, we have :
# AIC = 89 predictors
# Adj.R2 = 103 predictors
# BIC = 42 predictors
#
# For backward selection, we have :
# AIC = 85 predictors
# Adj.R2 = 117 predictors
# BIC = 41 predictors

# Let's create linear models for each hypotheses.
# H1 = 89 predictors
lm_aic_H1 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:91])
summary(lm_aic_H1)
pred_aic_H1 <- predict(lm_aic_H1, newdata = reduced_clean_dev[, 2:91])
RMSE(pred_aic_H1, reduced_clean_dev$SalePrice) #0.097

# H2 = 103 predictors
lm_adjr2_H2 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:105])
summary(lm_adjr2_H2)
pred_adjr2_H2 <- predict(lm_adjr2_H2, newdata = reduced_clean_dev[, 2:105])
RMSE(pred_adjr2_H2, reduced_clean_dev$SalePrice) # 0.097

# H3 = 42 predictors
lm_bic_H3 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:44])
summary(lm_bic_H3)
pred_bic_H3 <- predict(lm_bic_H3, newdata = reduced_clean_dev[, 2:44])
RMSE(pred_bic_H3, reduced_clean_dev$SalePrice) # 0.12

# H4 = 85 predictors
lm_aic_H4 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:87])
summary(lm_aic_H4)
pred_aic_H4 <- predict(lm_aic_H4, newdata = reduced_clean_dev[, 2:87])
RMSE(pred_aic_H4, reduced_clean_dev$SalePrice) # 0.099

# H5 = 117 predictors
lm_adjr2_H5 <- lm(SalePrice ~ ., data = reduced_clean_train[, 2:119])
summary(lm_adjr2_H5)
pred_adjr2_H5 <- predict(lm_adjr2_H5, newdata = reduced_clean_dev[, 2:119])
RMSE(pred_adjr2_H5, reduced_clean_dev$SalePrice) # 0.097

# H6 = 41 predictors
lm_bic_H6 <- lm(SalePrice ~ ., data = reduced_clean_train[, c(2:42, 44)])
summary(lm_bic_H6)
pred_bic_H6 <- predict(lm_bic_H6, newdata = reduced_clean_dev[, c(2:42, 44)])
RMSE(pred_bic_H6, reduced_clean_dev$SalePrice) # 0.12

# It seems the RMSE gets better with more predictors and gets worse with less predictors.
# A ridge regression seems to be more adapted to keep more variables. 
# We can also add some L1 regularization (lasso) as well, to remove useless predictors.



### Second model : GLMnet ####
# Creating a cross-validation plan
cv_plan <- trainControl(method = "cv", number = 10)
# Training a GLMnet model
set.seed(69, sample.kind = "Rounding")

model_glmnet <- caret::train(SalePrice ~ ., 
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

(rmse_glmnet <- RMSE(pred_glmnet, reduced_clean_dev$SalePrice)) # 0.091

# Ridge regression only
x_train <- model.matrix(SalePrice ~ ., reduced_clean_train)[, -44]
y_train <- reduced_clean_train$SalePrice

x_dev <- model.matrix(SalePrice ~ ., reduced_clean_dev)[, -44]
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
RMSE(ridge_pred, y_dev) # 0.108

# Lasso regression only
lasso_mod <- glmnet(x_train, y_train, alpha = 1, lambda = grid, standardize = FALSE)

plot(lasso_mod)

set.seed(69)
cv_out_lasso <- cv.glmnet(x_train, y_train, alpha = 1)
plot(cv_out_lasso)

best_lambda_lasso <- cv_out_lasso$lambda.min
best_lambda_lasso

lasso_pred <- predict(lasso_mod, s = best_lambda_lasso, newx = x_dev)
RMSE(lasso_pred, y_dev) # 0.112

# Elasticnet model
# Let's build an Elasticnet model with different values for alpha.

cv_elnet_model <- cv.glmnet(x_train, y_train, alpha = 0.4, lambda = grid)
plot(cv_elnet_model)

best_elnet_lambda = cv_elnet_model$lambda.min

elnet_model <- glmnet(x_train, y_train, lambda = best_elnet_lambda, standardize = FALSE)
elnet_pred <- predict(elnet_model, x_dev)

RMSE(elnet_pred, y_dev) # 0.114

# Plot of predictions of linear model
as.data.frame(y_dev) %>% cbind(elnet_pred) %>% 
  ggplot(aes(elnet_pred, y_dev)) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("GLMnet model predictions vs actual values")

#It looks like we cannot go under 0.10 ~ 0.11 for now...

# PCA
pca <- prcomp(reduced_clean_train[, -c(1, 44)])

summary(pca)

fviz_pca_var(pca, select.var = list(contrib = 5), repel = TRUE)


# Checking contribution of variables for PC1 and PC2 :
# PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 20)

# PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20)

# Screeplot to visualize the "elbow" to determine the number of components (2 or 3 components)
fviz_screeplot(pca)

# Applying Kaiser-Guttman rule to determine the number of components (eigenvalue > 1) : (15 components)
get_eigenvalue(pca) %>% filter(eigenvalue > 1)

# Parallel analysis to determine the number of components to retain 
# Parallel analysis :
paran_output <- paran(reduced_clean_train[,-c(1, 44)], seed = 69, graph = TRUE)

# Number of components to retain : (42 components)
paran_output$Retained

# PCR model with 15 components
library(pls)
pcr_mod <- pcr(SalePrice ~ . -Id, data = reduced_clean_train, scale = FALSE, validation = "CV")
validationplot(pcr_mod)

pcr_pred <- predict(pcr_mod, reduced_clean_dev, ncomp = 15)
RMSE(pcr_pred, reduced_clean_dev$SalePrice) # 0.12


# Let's create a recipe by adding a PCA and retaining 15 components
pca_recipe1 <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              Season = case_when(MoSold >= 1 & MoSold < 3 & MoSold == 12 ~ "Winter",
                                 MoSold >= 3 & MoSold < 6 ~ "Spring",
                                 MoSold >= 6 & MoSold < 9 ~ "Summer",
                                 .default = "Autumn"),
              OverallQC_per_GrLivArea = GrLivArea / Total_Area * (OverallQual + OverallCond)) %>%
  step_mutate(Season = as.factor(Season)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(LotFrontage, neighbors = 5, impute_with = imp_vars(all_predictors())) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold, GrLivArea, OverallQual, OverallCond)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BldgType_Duplex, Condition2_Same, MiscFeature_None, Condition2_RRAe)) %>%
  step_zv(all_predictors()) %>%
  step_select(-c(RoofStyle_Hip, MSSubClass_X120, Condition1_Norm, GarageType_Attchd,
                 MSSubClass_X160, Exterior2nd_Same, HouseStyle_X1Story, MSZoning_RM, SaleCondition_Partial,
                 RoofMatl_CompShg, MiscFeature_Gar2, Heating_GasA, MSSubClass_X190, BldgType_X2fmCon)) %>%
  step_pca(all_predictors(), num_comp = 15)

pca_prep1 <- pca_recipe1 %>% prep()

print(pca_prep1)

pca1_train <- pca_prep1 %>% bake(new_data = NULL)
pca1_dev <- pca_prep1 %>% bake(new_data = dev)

pca1_lm_mod <- lm(SalePrice ~ . -Id, data = pca1_train)
summary(pca1_lm_mod) %>% print()

pred_pca1_lm <- predict(pca1_lm_mod, newdata = pca1_dev)
RMSE(pred_pca1_lm, pca1_dev$SalePrice) # 0.130

# Let's create a recipe by adding a PCA and retaining 41 components
pca_recipe2 <- recipe(SalePrice ~ ., data = train) %>%
  update_role(Id, new_role = "ID") %>%
  step_mutate(house_age = YrSold - YearBuilt,
              garage_age = YrSold - GarageYrBlt,
              had_remod = ifelse((YearRemodAdd - YearBuilt) > 0, 1, 0),
              Season = case_when(MoSold >= 1 & MoSold < 3 & MoSold == 12 ~ "Winter",
                                 MoSold >= 3 & MoSold < 6 ~ "Spring",
                                 MoSold >= 6 & MoSold < 9 ~ "Summer",
                                 .default = "Autumn"),
              OverallQC_per_GrLivArea = GrLivArea / Total_Area * (OverallQual + OverallCond)) %>%
  step_mutate(Season = as.factor(Season)) %>%
  step_ordinalscore(all_ordered_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_impute_knn(LotFrontage, neighbors = 5, impute_with = imp_vars(all_predictors())) %>%
  step_mutate(SalePrice = log(SalePrice)) %>%
  step_select(-c(YrSold, YearBuilt, GarageYrBlt, YearRemodAdd, MoSold, GrLivArea, OverallQual, OverallCond)) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_select(-c(BldgType_Duplex, Condition2_Same, MiscFeature_None, Condition2_RRAe)) %>%
  step_zv(all_predictors()) %>%
  step_select(-c(RoofStyle_Hip, MSSubClass_X120, Condition1_Norm, GarageType_Attchd,
                 MSSubClass_X160, Exterior2nd_Same, HouseStyle_X1Story, MSZoning_RM, SaleCondition_Partial,
                 RoofMatl_CompShg, MiscFeature_Gar2, Heating_GasA, MSSubClass_X190, BldgType_X2fmCon)) %>%
  step_pca(all_predictors(), num_comp = 41)

pca_prep2 <- pca_recipe2 %>% prep()

print(pca_prep2)

pca2_train <- pca_prep2 %>% bake(new_data = NULL)
pca2_dev <- pca_prep2 %>% bake(new_data = dev)

pca2_lm_mod <- lm(SalePrice ~ . -Id, data = pca2_train)
summary(pca2_lm_mod) %>% print()

pred_pca44_lm <- predict(pca44_lm_mod, newdata = pca44_dev)
RMSE(pred_pca44_lm, pca44_dev$SalePrice) # 0.167

# PCA is surprisingly disappointing, whether it is with 15 or 41 components.

# Let's try something else.

### Third model : randomForest ####
# Training a randomForest model
model_rf <- caret::train(SalePrice ~ . -Id, 
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

# The 'ranger' model gets the best performance for now : 0.047.

# Plot of the randomForest predictions
reduced_clean_dev %>% 
  ggplot(aes(pred_rf, SalePrice)) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("randomForest model predictions vs actual values")


### Fourth model : XGBoost ####
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
xgb_tune <- caret::train(SalePrice ~ . -Id,
                         data = reduced_clean_train,
                         method = "xgbTree",
                         trControl = control, # 10-fold cross-validation plan
                         tuneGrid = xgb_grid, # hyperparameters we want to tune
                         verbose = FALSE,
                         metric = "RMSE",
                         nthread =3)

# Plot of the XGBoost tuning
plot(xgb_tune)

# XGBoost best tune
xgb_tune$bestTune

# Training an XGBoost model
model_xgb <- xgboost(data = as.matrix(reduced_clean_train[, -c(1, 44)]), # training data as matrix without the 'SalePrice' outcome
                     label = reduced_clean_train$SalePrice,  # we must use the original column of train_set
                     nrounds = 200,       # number of trees to build
                     objective = "reg:squarederror", # for regression
                     eta = 0.1,
                     max_depth = 5,
                     verbose = 0)  # silent

# Predictions of the XGBoost model
pred_xgb <- predict(model_xgb, as.matrix(reduced_clean_dev[, -c(1, 44)]))

(rmse_xgb <- RMSE(pred_xgb, clean_dev$SalePrice))

# XGBoost predictions versus actual values
clean_dev %>% cbind(pred_xgb) %>% ggplot(aes(pred_xgb, SalePrice)) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("XGBoost predictions vs actual values")

# XGBoost performs even better with 0.037.


### Fifth model : Generalized Additive Model (GAM) ####

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
# Let's check again the most important predictors and see if their non-linear relationships
vip(reduced_clean_lm_mod, n = 20)
print(imp_feat, n = 20)

imp_feat20 <- imp_feat$Variable[1:20]
imp_feat20

train20 <- reduced_clean_train[, imp_feat20]
train20$SalePrice <- reduced_clean_train$SalePrice

pairs(SalePrice ~ ., data = train20)

model_gam <- gam(SalePrice ~ s(OverallQC_per_GrLivArea) + s(Total_Area) + Bsmt_PercentQual_type1 +
                   GarageCars + Functional + ScreenPorch + SaleCondition_Normal + CentralAir + KitchenQual +
                   Condition2_Norm + BldgType_Twnhs + BldgType_TwnhsE + s(Bsmt_PercentQual_type2) + s(LotArea) + Total_Bath +
                   Neighborhood_StoneBr + TotRmsAbvGrd + RoofMatl_WdShngl + s(BsmtUnfSF) + Neighborhood_NridgHt +
                   SaleType_New, 
                 family = gaussian, # Important !
                 data = reduced_clean_train)

summary(model_gam)
plot(model_gam)
# Predictions of GAM :
pred_gam <- predict(model_gam, reduced_clean_dev)

# RMSE for GAM :
(rmse_gam <- RMSE(pred_gam, reduced_clean_dev$SalePrice)) # 0.10

# GAM predictions VS actual values plot :
reduced_clean_dev %>% cbind(pred_gam) %>% 
  ggplot(., aes(pred_gam, SalePrice)) +
  geom_point() +
  geom_abline(color = "blue") +
  ggtitle("GAM predictions vs actual values")


### Neural Network Model ###
library(reticulate)

#install_miniconda(update = TRUE)
#install_python(version = "3.11:latest")
#conda_create(envname = "r-tflow", python_version = "3.11.6")

#tensorflow::install_tensorflow(version = "nightly", envname = "r-tflow")
#keras::install_keras(method = "conda", version = "nightly", envname = "r-tflow")

conda_version()
conda_list()

#use_condaenv("r-tflow")

library(tensorflow)
library(keras)

tf$constant("Hello TensorFlow!")

tensorflow::tf_version()
print(tf$config$list_physical_devices('GPU'))

tensorflow::tf_config()
py_config()

# Fractionner les données en ensembles d'entraînement et de dev
set.seed(69)

X_train <- reduced_clean_train[, -c(1, 44)] |> as.matrix()
y_train <- reduced_clean_train$SalePrice |> as.vector()
X_dev <- reduced_clean_dev[, -c(1, 44)] |> as.matrix()
y_dev <- reduced_clean_dev$SalePrice |> as.vector()

# Créer le modèle
model <- keras_model_sequential() %>%
  layer_dense(units = 512, activation = "tanh", input_shape = c(ncol(X_train)), 
              kernel_regularizer = regularizer_l2(l = 0.099)) %>%
  layer_dense(units = 512, activation = "tanh", kernel_regularizer = regularizer_l2(0.10)) %>%
  layer_dense(units = 256, activation = "tanh", kernel_regularizer = regularizer_l1(0.005)) %>%
  layer_dense(units = 256, activation = "tanh", kernel_regularizer = regularizer_l1(0.05)) %>%
  layer_dense(units = 256, activation = "tanh") %>%
  layer_dense(units = 128, activation = "tanh") %>%
  layer_dense(units = 64, activation = "tanh") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

summary(model)
# Compiler le modèle
model %>% compile(optimizer = "adam", loss = "mean_squared_error", metrics = c("mean_squared_error"))

# Entraîner le modèle
model %>% keras::fit(
  x = X_train,
  y = y_train,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2
)


# Évaluer le modèle sur l'ensemble de test
loss <- model %>% evaluate(X_dev, y_dev)
print(loss)

pred_nn <- model |> predict(X_dev)

(rmse_tf <- RMSE(pred_nn, y_dev))


### Analysis of Variance
anova(lm_mod, signif_lm)







### Sum up of results of the different train sets (original variables, 9 PCs, and 16 PCs) ####

#data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "GAM", "XGBoost"),
#           RMSE_original_train = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_gam, rmse_xgb),
#           RMSE_9_components_train = c(rmse_lm_9, rmse_glmnet_9, rmse_rf_9, rmse_gam_9, rmse_xgb_9),
#           RMSE_16_components_train = c(rmse_lm_16, rmse_glmnet_16, rmse_rf_16, rmse_gam_16, rmse_xgb_16))




### Ensemble ####
# Creating an ensemble of the 5 best models : Base Linear Regression, GLMnet, randomForest, XGBoost, and GAM
ensemble <- (pred_lm + pred_glmnet + pred_rf + pred_xgb + pred_gam) / 5

# RMSE of the ensemble
(rmse_ensemble <- RMSE(ensemble, log(dev$SalePrice)))



### Predictions on the Test Set ####
test$SalePrice <- NA
base_test <- base_prep %>% bake(new_data = test)
clean_test <- clean_prep %>% bake(new_data = test)
reduced_clean_test <- reduced_clean_prep %>% bake(new_data = test)

## Predictions of Simple Linear Regression Models
# Base model
pred_base_lm <- predict(lm_mod, base_test)

submission_base_lm <- base_test |> mutate(SalePrice = exp(pred_base_lm)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_base_lm, "sub_base_lm.csv", row.names = FALSE)

# Clean model
pred_clean_lm <- predict(clean_lm_mod, clean_test)

submission_clean_lm <- clean_test |> mutate(SalePrice = exp(pred_clean_lm)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_clean_lm, "sub_clean_lm.csv", row.names = FALSE)

# Reduced clean model
pred_redu_clean_lm <- predict(reduced_clean_lm_mod, reduced_clean_test)

submission_redu_clean_lm <- reduced_clean_test |> mutate(SalePrice = exp(pred_redu_clean_lm)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_redu_clean_lm, "sub_redu_clean_lm.csv", row.names = FALSE)

## Predictions of GLMnet

pred_test_glmnet <- predict(model_glmnet, reduced_clean_test)

submission_glmnet <- reduced_clean_test |> mutate(SalePrice = exp(pred_test_glmnet)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_glmnet, "sub_glmnet.csv", row.names = FALSE)

## Predicting 'SalePrice' with the randomForest model 

pred_test_rf <- predict(model_rf, reduced_clean_test)

submission_rf <- reduced_clean_test |> mutate(SalePrice = exp(pred_test_rf)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_rf, "sub_rf.csv", row.names = FALSE)

## Predictions of XGBoost model

pred_test_xgb <- predict(model_xgb, as.matrix(reduced_clean_test[, -c(1, 44)]))

submission_xgb <- reduced_clean_test |> mutate(SalePrice = exp(pred_test_xgb)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_xgb, "sub_xgb.csv", row.names = FALSE)

## Predictions of the GAM model 

pred_test_gam <- predict(model_gam, reduced_clean_test)

submission_gam <- reduced_clean_test |> mutate(SalePrice = exp(pred_test_rf)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_gam, "sub_gam.csv", row.names = FALSE)


# Neural Network with Tensorflow
X_test <- reduced_clean_test[, -c(1, 44)] |> as.matrix()

pred_nn_test <- model |> predict(X_test)

submission_nn <- reduced_clean_test |> mutate(SalePrice = exp(pred_nn_test)) |>
  dplyr::select(Id, SalePrice)

write.csv(submission_nn, "sub_nn.csv", row.names = FALSE)

## Creating the ensemble
library(psych)
ensemble_val <- (pred_base_lm + pred_test_glmnet + pred_test_rf + pred_test_xgb + pred_test_gam) / 5

#df_pred <- data.frame(pred_base_lm, pred_test_glmnet, pred_test_rf, pred_test_xgb, pred_test_gam)
#ensemble_geom_mean <- map(df_pred, geometric.mean)
# Selecting Id and predictions for submission. We must not forget to use the exponential
# function to get the real values of sale prices.

submission_ensemble <- test %>% mutate(SalePrice = exp(ensemble_val)) %>% 
  dplyr::select(Id, SalePrice)

# Saving the submission
write.csv(submission_ensemble, "Sub_Ensemble.csv", row.names = FALSE)


### Final result ####
# Comparison of RMSEs between train and test sets

data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "XGBoost", "GAM", "Ensemble"),
           RMSE_dev_set = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_xgb, rmse_gam, rmse_ensemble),
           RMSE_test_set = c(0.13659, 0.14211, 0.14023, 0.12471, 0.14023, 0.12006))


comparison_rmse <- data.frame(Model_type = c("Linear", "GLMnet", "randomForest", "XGBoost","GAM", "Ensemble", "Linear", "GLMnet", "randomForest", "XGBoost","GAM", "Ensemble"),
                              RMSE = c(rmse_lm, rmse_glmnet, rmse_rf, rmse_xgb, rmse_gam, rmse_ensemble, 0.13659, 0.14211, 0.14023, 0.12471, 0.14023, 0.12006),
                              dataset = c("dev", "dev", "dev", "dev", "dev", "dev", "test", "test", "test", "test", "test", "test"))



# Bar plot of RMSEs between train and test sets
comparison_rmse %>% ggplot(aes(Model_type, RMSE, fill = dataset)) +
  geom_col(position = "dodge") +
  ggtitle("Comparing RMSE between train and test sets")
