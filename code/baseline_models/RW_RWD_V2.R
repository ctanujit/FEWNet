############### RW and RWD models for BRIC nations: 12M & 24M ################
############## Generating RW and RWD Forecasts for BRIC Countries: 12M ############
library(forecast)
# Brazil - FH: 12M
setwd("/data/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_12M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_12M_R.csv",header=TRUE)

# Generate 12M Forward Forecasts using RWF method
rwf_bzl_12m <- rwf(
  ts(cpi.train.df$CPI_inflation_rate),
  h = 12,
  drift = FALSE,
  # model = 'ets',
  lambda = "auto",
  biasadj = TRUE,
)

# Get the forecasts values
rwf_bzl_fcst_12m <- rwf_bzl_12m$mean
rwf_bzl_fcst_12m

# Generate 12M Forward Forecasts using RWFD method
rwfd_bzl_12m <- rwf(
  ts(cpi.train.df$CPI_inflation_rate),
  h = 12,
  drift = TRUE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)

# Get the forecasts values
rwfd_bzl_fcst_12m <- rwfd_bzl_12m$mean
rwfd_bzl_fcst_12m

# Russia - FH:12M
setwd("/data/russia")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

# Generate 12M Forward Forecasts using RWF method
rwf_rus_12m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 12,
  drift = FALSE,
  # model = 'ets',
  lambda = "auto",
  biasadj = TRUE,
)
# Get the forecasts values
rwf_rus_fcst_12m <- rwf_rus_12m$mean
rwf_rus_fcst_12m

# Generate 12M Forward Forecasts using RWFD method
rwfd_rus_12m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 12,
  drift = TRUE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwfd_rus_fcst_12m <- rwfd_rus_12m$mean
rwfd_rus_fcst_12m

# India - FH:12M
setwd("/data/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_12M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_12M_R.csv",header=TRUE)

# Generate 12M Forward Forecasts using RWF method
rwf_ind_12m <- rwf(
  ts(cpi.train.df$CPI_inflation_Rate),
  h = 12,
  drift = FALSE,
  # model = 'ets',
  lambda = "auto",
  biasadj = TRUE,
)
# Get the forecasts values
rwf_ind_fcst_12m <- rwf_ind_12m$mean
rwf_ind_fcst_12m

# Generate 12M Forward Forecasts using RWFD method
rwfd_ind_12m <- rwf(
  ts(cpi.train.df$CPI_inflation_Rate),
  h = 12,
  drift = TRUE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwfd_ind_fcst_12m <- rwfd_ind_12m$mean
rwfd_ind_fcst_12m

# China - FH:12M
setwd("/data/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_12M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_12M_R.csv",header=TRUE)

# Generate 12M Forward Forecasts using RWF method
rwf_chn_12m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 12,
  drift = FALSE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwf_chn_fcst_12m <- rwf_chn_12m$mean
rwf_chn_fcst_12m

# Generate 12M Forward Forecasts using RWFD method
rwfd_chn_12m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 12,
  drift = TRUE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwfd_chn_fcst_12m <- rwfd_chn_12m$mean
rwfd_chn_fcst_12m

#################### END of CODE :12M ########################

# Brazil - FH:24M
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)


# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)


# Generate 24M Forward Forecasts using RWF method
rwf_bzl_24m <- rwf(
  ts(cpi.train.df$CPI_inflation_rate),
  h = 24,
  drift = FALSE,
  # model = 'ets',
  lambda = "auto",
  biasadj = TRUE,
)

# Get the forecasts values
rwf_bzl_fcst_24m <- rwf_bzl_24m$mean
rwf_bzl_fcst_24m

# Generate 24M Forward Forecasts using RWFD method
rwfd_bzl_24m <- rwf(
  ts(cpi.train.df$CPI_inflation_rate),
  h = 24,
  drift = TRUE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)

# Get the forecasts values
rwfd_bzl_fcst_24m <- rwfd_bzl_24m$mean
rwfd_bzl_fcst_24m

############## Generating RW and RWD Forecasts for BRIC Countries: 24M ############
# Brazil - FH:24M
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)

# Generate 24M Forward Forecasts using RWF method
rwf_bzl_24m <- rwf(
  ts(cpi.train.df$CPI_inflation_rate),
  h = 24,
  drift = FALSE,
  # model = 'ets',
  lambda = "auto",
  biasadj = TRUE,
)

# Get the forecasts values
rwf_bzl_fcst_24m <- rwf_bzl_24m$mean
rwf_bzl_fcst_24m

# Generate 24M Forward Forecasts using RWFD method
rwfd_bzl_24m <- rwf(
  ts(cpi.train.df$CPI_inflation_rate),
  h = 24,
  drift = TRUE,
  # model = 'ets',
  lambda = NULL,
  biasadj = TRUE,
)

# Get the forecasts values
rwfd_bzl_fcst_24m <- rwfd_bzl_24m$mean
rwfd_bzl_fcst_24m

# Russia - FH:24M
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/russia")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_24M_R.csv",header=TRUE)

# Generate 24M Forward Forecasts using RWF method
rwf_rus_24m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 24,
  drift = FALSE,
  lambda = "auto",
  biasadj = TRUE,
)
# Get the forecasts values
rwf_rus_fcst_24m <- rwf_rus_24m$mean
rwf_rus_fcst_24m

# Generate 12M Forward Forecasts using RWFD method
rwfd_rus_24m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 24,
  drift = TRUE,
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwfd_rus_fcst_24m <- rwfd_rus_24m$mean
rwfd_rus_fcst_24m

# India - FH:24M
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)

# Generate 24M Forward Forecasts using RWF method
rwf_ind_24m <- rwf(
  ts(cpi.train.df$CPI_inflation_Rate),
  h = 24,
  drift = FALSE,
  lambda = "auto",
  biasadj = TRUE,
)
# Get the forecasts values
rwf_ind_fcst_24m <- rwf_ind_24m$mean
rwf_ind_fcst_24m

# Generate 24M Forward Forecasts using RWFD method
rwfd_ind_24m <- rwf(
  ts(cpi.train.df$CPI_inflation_Rate),
  h = 24,
  drift = TRUE,
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwfd_ind_fcst_24m <- rwfd_ind_24m$mean
rwfd_ind_fcst_24m

# China - FH:24M
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)

# Generate 24M Forward Forecasts using RWF method
rwf_chn_24m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 24,
  drift = FALSE,
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwf_chn_fcst_24m <- rwf_chn_24m$mean
rwf_chn_fcst_24m

# Generate 24M Forward Forecasts using RWFD method
rwfd_chn_24m <- rwf(
  ts(cpi.train.df$cpi_inflation_rate),
  h = 24,
  drift = TRUE,
  lambda = NULL,
  biasadj = TRUE,
)
# Get the forecasts values
rwfd_chn_fcst_24m <- rwfd_chn_24m$mean
rwfd_chn_fcst_24m

#################### END of CODE :24M ########################
