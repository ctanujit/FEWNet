################################## Figure 05 : Giacomini & Rossi Test ###################

# GR Tests for BRIC Countries: 12 months ahead forecasts

# install.packages('murphydiagram')
# install.packages('changepoint')

library(murphydiagram)
library(changepoint)

set.seed(20240101) # For reproducibility, we are using this seed value

########################### Brazil #######################
# Working directory
setwd("./FEWNet/dataset/brazil")
getwd()

# Brazil: 12M forecasts from FEWNet, AR and XGBoost Models datasets
data_brazil_12M <- read.csv("GR_test_brazil_12M_data.csv",header=TRUE)
str(data_brazil_12M)

# Convert date into numeric entity
tml <- as.numeric(data_brazil_12M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_brazil_12M$FEWNet_12M,
                               y = data_brazil_12M$test_data_12M,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR model and Test Data 
score_AR <- extremal_score(x = data_brazil_12M$AR,
                           y = data_brazil_12M$test_data_12M,
                           theta = 6)
score_AR
str(score_AR)

# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_brazil_12M$XGBoost_12M,
                                y = data_brazil_12M$test_data_12M, 
                                theta = 7)
score_XGBoost
str(score_XGBoost)


# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.7,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgboost_12m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.5,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_12m

########################### Russia #######################
# Working directory
setwd("./FEWNet/dataset/russia")
getwd()

# Russia: 12M forecasts from FEWNet, AR and XGBoost Models datasets
data_russia_12M <- read.csv("GR_test_russia_12M_data.csv",header=TRUE)
str(data_russia_12M)

# Convert date into numeric entity
tml <- as.numeric(data_russia_12M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_russia_12M$FEWNet_12M,
                               y = data_russia_12M$test_data_12M,
                               theta = 7)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data
score_AR <- extremal_score(x = data_russia_12M$AR,
                           y = data_russia_12M$test_data_12M,
                           theta = 7)
score_AR
str(score_AR)

# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_russia_12M$XGBoost_12M,
                                y = data_russia_12M$test_data_12M, 
                                theta = 9)
score_XGBoost
str(score_XGBoost)

# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.2,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgb_12m <- fluctuation_test(score_FEWNet, 
                                              score_XGBoost,
                                              mu = 0.7,
                                              # dmv_fullsample = FALSE,
                                              conf_level = 0.10,
                                              time_labels = tml, 
                                              lag_truncate = 1)
fluct_test_fewnet_xgb_12m


########################### India #######################
# Working directory
setwd("./FEWNet/dataset/india")
getwd()

# India: 12M forecasts from FEWNet, AR and XGBoost Models datasets
data_india_12M <- read.csv("GR_test_india_12M_data.csv",header=TRUE)
str(data_india_12M)

# Convert date into numeric entity
tml <- as.numeric(data_india_12M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_india_12M$FEWNet_12M,
                               y = data_india_12M$test_data_12M,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data
score_AR <- extremal_score(x = data_india_12M$AR,
                           y = data_india_12M$test_data_12M,
                           theta = 6)
score_AR
str(score_AR)

# Compute extremal scores of XGBoost and Test Data
score_XGBoost <- extremal_score(x = data_india_12M$XGBoost_12M,
                                y = data_india_12M$test_data_12M,
                                theta = 6)
score_XGBoost
str(score_XGBoost)

# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.3,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# GR test - plot: FEWNet Vs. XGBoost
fluct_test_fewnet_xgb_12m <- fluctuation_test(score_FEWNet, 
                                              score_XGBoost,
                                              mu = 0.2,
                                              conf_level = 0.10,
                                              time_labels = tml, 
                                              lag_truncate = 1)
fluct_test_fewnet_xgb_12m

########################### China #######################
# Working directory
setwd("./FEWNet/dataset/china")
getwd()

# China: 12M forecasts from FEWNet, AR and XGBoost Models datasets
data_china_12M <- read.csv("GR_test_china_12M_data.csv",header=TRUE)
str(data_china_12M)

# Convert date into numeric entity
tml <- as.numeric(data_china_12M$Date_Year)
tml

# Compute extremal scores of FEWNet and Test Data
score_FEWNet <- extremal_score(x = data_china_12M$FEWNet_12M,
                               y = data_china_12M$test_data_12M,
                               theta = 1)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data
score_AR <- extremal_score(x = data_china_12M$AR,
                           y = data_china_12M$test_data_12M,
                           theta = 3)
score_AR

# Compute extremal scores of XGB and Test Data
score_XGB <- extremal_score(x = data_china_12M$XGBoost_12M,
                            y = data_china_12M$test_data_12M,
                            theta = 3)
score_XGB

# GR test - plot: FEWNet Vs. AR
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet,
                                             score_AR,
                                             mu = 0.2,
                                             conf_level = 0.10,
                                             time_labels = tml,
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# GR test - plot: FEWNet Vs. XGboost
fluct_test_fewnet_xgb_12m <- fluctuation_test(score_FEWNet,
                                              score_XGB,
                                              mu = 0.2,
                                              conf_level = 0.10,
                                              time_labels = tml,
                                              lag_truncate = 1)
fluct_test_fewnet_xgb_12m

########################### End of Code: GR test ##############################

