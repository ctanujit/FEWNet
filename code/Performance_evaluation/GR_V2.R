########### Giacomini & Rossi Forecasting Ability Test under instabilities #############
############################# GR Test for Brazil: 24M ########################
# install.packages('murphydiagram')
library(murphydiagram)

setwd("/data/brazil")
getwd()

setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/Github/github_code_final/FEWNet/dataset/brazil")
getwd()

# 24M
data_brazil_24M <- read.csv("GR_test_brazil_24M_data.csv",header=TRUE)
str(data_brazil_24M)

# Convert date into numeric entity
tml <- as.numeric(data_brazil_24M$Date_Year)
tml

# Compare Robustness among FEWNet and 4 challenger models
# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_brazil_24M$FEWNet_24M,
                               y = data_brazil_24M$test_data_24M,
                               # functional = "quantile",
                               # alpha = 0.8,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

score_AR <- extremal_score(x = data_brazil_24M$AR_24M,
                           y = data_brazil_24M$test_data_24M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 6)
score_AR
str(score_AR)

# Generate the murphydiagram
murphydiagram(data_brazil_24M$FEWNet_24M, 
              data_brazil_24M$AR_24M,
              data_brazil_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "AR"))

murphydiagram_diff(data_brazil_24M$FEWNet_24M, 
                   data_brazil_24M$AR_24M,
                   data_brazil_24M$test_data_24M, 
                   lag_truncate = 1,
                   conf_level = 0.90)

# Compute extremal scores of XGBoost_24M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_brazil_24M$XGBoost_24M,
                                y = data_brazil_24M$test_data_24M, 
                                # functional = "quantile",
                                theta = 12)
score_XGBoost
str(score_XGBoost)

# Generate the murphydiagram
murphydiagram(data_brazil_24M$FEWNet_24M, 
              data_brazil_24M$XGBoost_24M,
              data_brazil_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "XGBoost"))

murphydiagram_diff(data_brazil_24M$FEWNet_24M, 
                   data_brazil_24M$XGBoost_24M,
                   data_brazil_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Alternative Approach: FEWNet_24M Vs AR_24M
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.3,
                                             # dmv_fullsample = TRUE,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# Alternative Approach: FEWNet_24M Vs XgBoost_24M
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.1,
                                                  # dmv_fullsample = TRUE,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

############################# GR Test for Russia: 24M ###################
setwd("/data/russia")
getwd()

# 24M
data_russia_24M <- read.csv("GR_test_russia_24M_data.csv",header=TRUE)
str(data_russia_24M)

# Convert date into numeric entity
tml <- as.numeric(data_russia_24M$Date_Year)
tml

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_russia_24M$FEWNet_24M,
                               y = data_russia_24M$test_data_24M,
                               # functional = "quantile",
                               # alpha = 0.8,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

score_AR <- extremal_score(x = data_russia_24M$AR_24M,
                           y = data_russia_24M$test_data_24M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 6)
score_AR
str(score_AR)

# Generate the murphydiagram
murphydiagram(data_russia_24M$FEWNet_24M, 
              data_russia_24M$AR_24M,
              data_russia_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "AR"))

murphydiagram_diff(data_russia_24M$FEWNet_24M, 
                   data_russia_24M$AR_24M,
                   data_russia_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Compute extremal scores of XGBoost_24M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_russia_24M$XGBoost_24M,
                                y = data_russia_24M$test_data_24M, 
                                # functional = "quantile",
                                theta = 12)
score_XGBoost
str(score_XGBoost)

# Generate the murphydiagram
murphydiagram(data_russia_24M$FEWNet_24M, 
              data_russia_24M$XGBoost_24M,
              data_russia_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "XGBoost"))

murphydiagram_diff(data_russia_24M$FEWNet_24M, 
                   data_russia_24M$XGBoost_24M,
                   data_russia_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Alternative Approach: FEWNet_24M Vs AR_24M
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.2,
                                             # dmv_fullsample = TRUE,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# Alternative Approach: FEWNet_24M Vs XgBoost_24M
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.2,
                                                  # dmv_fullsample = TRUE,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

############################# GR Test for India: 24M ########################
setwd("/data/india")
getwd()

# 24M
data_india_24M <- read.csv("GR_test_india_24M_data.csv",header=TRUE)
str(data_india_24M)

# Convert date into numeric entity
tml <- as.numeric(data_india_24M$Date_Year)
tml

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_india_24M$FEWNet_24M,
                               y = data_india_24M$test_data_24M,
                               # functional = "quantile",
                               # alpha = 0.8,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

score_AR <- extremal_score(x = data_india_24M$AR_24M,
                           y = data_india_24M$test_data_24M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 6)
score_AR
str(score_AR)

# Generate the murphydiagram
murphydiagram(data_india_24M$FEWNet_24M, 
              data_india_24M$AR_24M,
              data_india_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "AR"))

murphydiagram_diff(data_india_24M$FEWNet_24M, 
                   data_india_24M$AR_24M,
                   data_india_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Compute extremal scores of XGBoost_24M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_india_24M$XGBoost_24M,
                                y = data_india_24M$test_data_24M, 
                                # functional = "quantile",
                                theta = 6)
score_XGBoost
str(score_XGBoost)

# Generate the murphydiagram
murphydiagram(data_india_24M$FEWNet_24M, 
              data_india_24M$XGBoost_24M,
              data_india_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "XGBoost"))

murphydiagram_diff(data_india_24M$FEWNet_24M, 
                   data_india_24M$XGBoost_24M,
                   data_india_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Alternative Approach: FEWNet_24M Vs AR_24M
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.5,
                                             # dmv_fullsample = TRUE,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# Alternative Approach: FEWNet_24M Vs XgBoost_24M
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.1,
                                                  # dmv_fullsample = TRUE,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

############################# GR Test for China: 24M  ########################
setwd("/data/china")
getwd()

# 24M
data_china_24M <- read.csv("GR_test_china_24M_data.csv",header=TRUE)
str(data_china_24M)

# Convert date into numeric entity
tml <- as.numeric(data_china_24M$Date_Year)
tml

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_china_24M$FEWNet_24M,
                               y = data_china_24M$test_data_24M,
                               theta = 3)
score_FEWNet
str(score_FEWNet)


score_AR <- extremal_score(x = data_china_24M$AR_24M,
                           y = data_china_24M$test_data_24M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 3)
score_AR
str(score_AR)

# Generate the murphydiagram
murphydiagram(data_china_24M$FEWNet_24M, 
              data_china_24M$AR_24M,
              data_china_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "AR"))

murphydiagram_diff(data_china_24M$FEWNet_24M, 
                   data_china_24M$AR_24M,
                   data_china_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Compute extremal scores of XGBoost_24M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_china_24M$XGBoost_24M,
                                y = data_china_24M$test_data_24M, 
                                # functional = "quantile",
                                theta = 6)
score_XGBoost
str(score_XGBoost)

# Generate the murphydiagram
murphydiagram(data_china_24M$FEWNet_24M, 
              data_china_24M$XGBoost_24M,
              data_china_24M$test_data_24M, 
              # alpha = 0.3,
              labels = c("FEWNet", "XGBoost"))

murphydiagram_diff(data_china_24M$FEWNet_24M, 
                   data_china_24M$XGBoost_24M,
                   data_china_24M$test_data_24M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Alternative Approach: FEWNet_24M Vs AR_24M
fluct_test_fewnet_ar_24m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.6,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_24m

# Alternative Approach: FEWNet_24M Vs XgBoost_24M
fluct_test_fewnet_xgboost_24m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.2,
                                                  # dmv_fullsample = TRUE,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_24m

############################# GR Test for Brazil: 12M ########################
setwd("/data/brazil")
getwd()

# 12M
data_brazil_12M <- read.csv("GR_test_brazil_12M_data.csv",header=TRUE)
str(data_brazil_12M)

# Convert date into numeric entity
tml <- as.numeric(data_brazil_12M$Date_Year)
tml

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_brazil_12M$FEWNet_12M,
                               y = data_brazil_12M$test_data_12M,
                               # functional = "quantile",
                               # alpha = 0.8,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_AR <- extremal_score(x = data_brazil_12M$AR,
                           y = data_brazil_12M$test_data_12M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 6)
score_AR
str(score_AR)

murphydiagram_diff(data_brazil_12M$FEWNet_12M, 
                   data_brazil_12M$AR,
                   data_brazil_12M$test_data_12M, 
                   lag_truncate = 1,
                   conf_level = 0.90)

# Alternative Approach: FEWNet_12M Vs AR_12M
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.7,
                                             # dmv_fullsample = TRUE,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# Compute extremal scores of XGBoost_24M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_brazil_12M$XGBoost_12M,
                                y = data_brazil_12M$test_data_12M, 
                                # functional = "quantile",
                                theta = 7)
score_XGBoost
str(score_XGBoost)

murphydiagram_diff(data_brazil_12M$FEWNet_12M, 
                   data_brazil_12M$XGBoost_12M,
                   data_brazil_12M$test_data_12M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Alternative Approach: FEWNet_12M Vs XgBoost_12M
fluct_test_fewnet_xgboost_12m <- fluctuation_test(score_FEWNet, 
                                                  score_XGBoost,
                                                  mu = 0.5,
                                                  # dmv_fullsample = TRUE,
                                                  conf_level = 0.10,
                                                  time_labels = tml, 
                                                  lag_truncate = 1)
fluct_test_fewnet_xgboost_12m

############################# GR Test for Russia: 12M ########################
setwd("/data/russia")
getwd()

# 12M
data_russia_12M <- read.csv("GR_test_russia_12M_data.csv",header=TRUE)
str(data_russia_12M)

# Convert date into numeric entity
tml <- as.numeric(data_russia_12M$Date_Year)
tml

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_russia_12M$FEWNet_12M,
                               y = data_russia_12M$test_data_12M,
                               # functional = "quantile",
                               # alpha = 0.8,
                               theta = 7)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data (theta = 3)
score_AR <- extremal_score(x = data_russia_12M$AR,
                           y = data_russia_12M$test_data_12M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 7)
score_AR
str(score_AR)

murphydiagram_diff(data_russia_12M$FEWNet_12M, 
                   data_russia_12M$AR,
                   data_russia_12M$test_data_12M, 
                   lag_truncate = 1,
                   # labels = c("FEWNet", "MSGARCH"),
                   conf_level = 0.90)

# Alternative Approach: FEWNet_24M Vs AR_24M
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.2,
                                             # dmv_fullsample = TRUE,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# Compute extremal scores of XGBoost_24M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_russia_12M$XGBoost_12M,
                                y = data_russia_12M$test_data_12M, 
                                # functional = "quantile",
                                theta = 9)
score_XGBoost
str(score_XGBoost)

# Alternative Approach: FEWNet_24M Vs XGBoost_24M
fluct_test_fewnet_xgb_12m <- fluctuation_test(score_FEWNet, 
                                              score_XGBoost,
                                              mu = 0.7,
                                              # dmv_fullsample = FALSE,
                                              conf_level = 0.10,
                                              time_labels = tml, 
                                              lag_truncate = 1)
fluct_test_fewnet_xgb_12m

############################# GR Test for India: 12M ########################
setwd("/data/india")
getwd()

# 24M
data_india_12M <- read.csv("GR_test_india_12M_data.csv",header=TRUE)
str(data_india_12M)

# Convert date into numeric entity
tml <- as.numeric(data_india_12M$Date_Year)
tml

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_india_12M$FEWNet_12M,
                               y = data_india_12M$test_data_12M,
                               # functional = "quantile",
                               # alpha = 0.8,
                               theta = 6)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of FEWNet_24M and Test Data (theta = 3)
score_AR <- extremal_score(x = data_india_12M$AR,
                           y = data_india_12M$test_data_12M,
                           # functional = "quantile",
                           # alpha = 0.8,
                           theta = 6)
score_AR
str(score_AR)

# Alternative Approach: FEWNet_12M Vs AR
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet, 
                                             score_AR,
                                             mu = 0.3,
                                             # dmv_fullsample = FALSE,
                                             conf_level = 0.10,
                                             time_labels = tml, 
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# Compute extremal scores of XGBoost_12M and Test Data (theta = 3)
score_XGBoost <- extremal_score(x = data_india_12M$XGBoost_12M,
                                y = data_india_12M$test_data_12M,
                                # functional = "quantile",
                                theta = 6)
score_XGBoost
str(score_XGBoost)

# Alternative Approach: FEWNet_12M Vs AR
fluct_test_fewnet_xgb_12m <- fluctuation_test(score_FEWNet, 
                                              score_XGBoost,
                                              mu = 0.2,
                                              # dmv_fullsample = FALSE,
                                              conf_level = 0.10,
                                              time_labels = tml, 
                                              lag_truncate = 1)
fluct_test_fewnet_xgb_12m

############################# GR Test for China: 12M ########################
setwd("/data/china")
getwd()

# 24M
data_china_12M <- read.csv("GR_test_china_12M_data.csv",header=TRUE)
str(data_china_12M)

# Convert date into numeric entity
tml <- as.numeric(data_china_12M$Date_Year)
tml

# Compute extremal scores of FEWNet_12M and Test Data (theta = 3)
score_FEWNet <- extremal_score(x = data_china_12M$FEWNet_12M,
                               y = data_china_12M$test_data_12M,
                               theta = 1)
score_FEWNet
str(score_FEWNet)

# Compute extremal scores of AR and Test Data (theta = 3)
score_AR <- extremal_score(x = data_china_12M$AR,
                           y = data_china_12M$test_data_12M,
                           theta = 3)
score_AR
str(score_AR)

# Alternative Approach: FEWNet_12M Vs AR
fluct_test_fewnet_ar_12m <- fluctuation_test(score_FEWNet,
                                             score_AR,
                                             mu = 0.2,
                                             # dmv_fullsample = FALSE,
                                             conf_level = 0.10,
                                             time_labels = tml,
                                             lag_truncate = 1)
fluct_test_fewnet_ar_12m

# Compute extremal scores of XGB and Test Data (theta = 3)
score_XGB <- extremal_score(x = data_china_12M$XGBoost_12M,
                            y = data_china_12M$test_data_12M,
                            theta = 3)
score_XGB
str(score_XGB)

# Alternative Approach: FEWNet_12M Vs AR
fluct_test_fewnet_xgb_12m <- fluctuation_test(score_FEWNet,
                                              score_XGB,
                                              mu = 0.2,
                                              # dmv_fullsample = FALSE,
                                              conf_level = 0.10,
                                              time_labels = tml,
                                              lag_truncate = 1)
fluct_test_fewnet_xgb_12m

########################### End of Code: GR test ##############################
