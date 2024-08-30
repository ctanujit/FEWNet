################################## Figure 06 : Giacomini & Rossi Test ###################

# GR Tests for BRIC Countries: 24 months ahead forecasts

# install.packages('murphydiagram')
# install.packages('changepoint')

library(murphydiagram)
library(changepoint)

set.seed(20240101) # For reproducibility, we are using this seed value

########################### Brazil #######################
# Working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Brazil: 24M forecasts from FEWNet, AR and XGBoost Models datasets
data_brazil_24M <- read.csv("GR_test_brazil_24M_data.csv",header=TRUE)
str(data_brazil_24M)

# Convert date into numeric entity
tml <- as.numeric(data_brazil_24M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data 
score_FEWNet <- extremal_score(x = data_brazil_24M$FEWNet_24M,
                               y = data_brazil_24M$test_data_24M,
                               theta = 6)
score_FEWNet

# Compute extremal scores of AR model and Test Data 
score_AR <- extremal_score(x = data_brazil_24M$AR_24M,
                           y = data_brazil_24M$test_data_24M,
                           theta = 6)
score_AR

# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_brazil_24M$XGBoost_24M,
                                y = data_brazil_24M$test_data_24M, 
                                theta = 12)
score_XGBoost

# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.3,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.1,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

########################### Russia #######################
# Working directory
setwd("/FEWNet/dataset/russia")
getwd()

# Russia: 24M forecasts from FEWNet, AR and XGBoost Models datasets
data_russia_24M <- read.csv("GR_test_russia_24M_data.csv",header=TRUE)
str(data_russia_24M)

# Convert date into numeric entity
tml <- as.numeric(data_russia_24M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_russia_24M$FEWNet_24M,
                               y = data_russia_24M$test_data_24M,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data
score_AR <- extremal_score(x = data_russia_24M$AR_24M,
                           y = data_russia_24M$test_data_24M,
                           theta = 6)
score_AR
score_AR

# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_russia_24M$XGBoost_24M,
                                y = data_russia_24M$test_data_24M, 
                                # functional = "quantile",
                                theta = 12)
score_XGBoost
str(score_XGBoost)


# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.2,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m


# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.2,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

########################### India #######################
# Working directory
setwd("/FEWNet/dataset/india")
getwd()


# India: 24M forecasts from FEWNet, AR and XGBoost Models datasets
data_india_24M <- read.csv("GR_test_india_24M_data.csv",header=TRUE)
str(data_india_24M)

# Convert date into numeric entity
tml <- as.numeric(data_india_24M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_india_24M$FEWNet_24M,
                               y = data_india_24M$test_data_24M,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data
score_AR <- extremal_score(x = data_india_24M$AR_24M,
                           y = data_india_24M$test_data_24M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 6)
score_AR
str(score_AR)


# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_india_24M$XGBoost_24M,
                                y = data_india_24M$test_data_24M, 
                                # functional = "quantile",
                                theta = 6)
score_XGBoost
str(score_XGBoost)

# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.5,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.1,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

########################### China #######################
# Working directory
setwd("/FEWNet/dataset/china")
getwd()

# China: 24M forecasts from FEWNet, AR and XGBoost Models datasets
data_china_24M <- read.csv("GR_test_china_24M_data.csv",header=TRUE)
str(data_china_24M)

# Convert date into numeric entity
tml <- as.numeric(data_china_24M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_china_24M$FEWNet_24M,
                               y = data_china_24M$test_data_24M,
                               theta = 3)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data
score_AR <- extremal_score(x = data_china_24M$AR_24M,
                           y = data_china_24M$test_data_24M,
                           theta = 3)
score_AR
str(score_AR)

# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_china_24M$XGBoost_24M,
                                y = data_china_24M$test_data_24M, 
                                theta = 6)
score_XGBoost
str(score_XGBoost)

# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.6,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.2,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

########################### End of Code: GR test ##############################
