########################### ARFIMAx Model for long-term forecasting of CPI Inflation #####################

### Explore ARFIMA model with exogenous factors #####
# Read the base Table
setwd("/dataset/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

# Import relevant libraries
library(lubridate)
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(tseries)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(arfima)
library(fracdiff)
library(arfima)
library(fracdiff)

str(cpi.train.df)
str(cpi.test.df)

# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_ind_cycle_cf_l1,
  cpi.train.df$gprc_ind_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new

xMat.test.new <- matrix(cbind(
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_ind_cycle_cf_l1,
  cpi.test.df$gprc_ind_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
cpi.df.test.ts

con_tst = cpi.test.df$CPI_inflation_Rate

set.seed(211)
n.fore <- 24
fit_ARFIMAX=arfima(cpi.df.train.ts,
                   order = c(6,1,0), # (6,1,0) - Best so far
                   numeach = c(0, 0),
                   xreg = xMat.train.new,
                   back= TRUE,
                   lmodel = "n"
)
summary(fit_ARFIMAX)
print(fit_ARFIMAX)

# Generate the long-term forecasts for CPI inflation
predARFIMAX <- predict(fit_ARFIMAX,
                       n.ahead = n.fore,
                       xreg = xMat.train.new[1:24,1:6])

# Print the forecasts
predARFIMAX[[1]][["Forecast"]]
# Print the test data series
con_tst

############################# End of Code ####################################
