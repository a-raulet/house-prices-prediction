### Code enlevé - House Price ###

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



# Converting "Year" variables to date variables :
train <- train %>% mutate(YearBuilt = parse_date_time(YearBuilt, orders = "Y"),
                          YearRemodAdd = parse_date_time(YearRemodAdd, orders = "Y"),
                          GarageYrBlt = parse_date_time(GarageYrBlt, orders = "Y"),
                          DateSold = make_date(year = YrSold, month = MoSold))

# Adding duration variables :
train <- train %>% mutate(durationBltSold = difftime(DateSold, YearBuilt, units = "weeks"),
                          durationRemodSold = difftime(DateSold, YearRemodAdd, units = "weeks"))


# The variable PoolQC has only 7 observations. They won't be useful as it they are too few to be
# statistically significant. We delete it.

test$PoolQC <- NULL


### Lot Frontage variable
# LotFrontage variable : Where are the NAs ?
test %>% ggplot(aes(BldgType, OverallQual, color = is.na(LotFrontage))) +
  geom_jitter()

# Creating matrix with numerical variables
num_matrix_val <- select_if(test, is.numeric) %>% as.matrix()

# Estimation of the optimal number of dimensions
number_cp_val <- estim_ncpPCA(num_matrix_val)

# Imputing values instead of NAs
complete_matrix_val <- imputePCA(num_matrix, ncp = number_cp$ncp, scale = TRUE)

# Extracting the complete matrix
clean_matrix_val <- complete_matrix_val$completeObs

# Changing the complete matrix as a data frame
clean_num_val <- as.data.frame(clean_matrix_val)

# Replacing NAs in the train set for 'LotFrontage' variable :
test$LotFrontage[is.na(test$LotFrontage)] <- clean_num_val$LotFrontage[is.na(test$LotFrontage)]