########################### ARNNx Model for long-term forecasting of CPI Inflation #####################
### Explore ARNN model with exogenous factors #####
# This is an example code for the ARNNx model. The same code module can be replicated for other geographies
# Read the base Table
setwd("/FEWNet/dataset/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

# Import all the relevant libraries
library(lubridate)
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(egcm)
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)

# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_chn_cycle_cf_l1,
  cpi.train.df$gprc_chn_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new

xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_chn_cycle_cf_l1,
  cpi.test.df$gprc_chn_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new

cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

set.seed(99)
fit_ARNNX = nnetar(cpi.df.train.ts, #cpi.df.train$cpi,
                   xreg = xMat.train.new,
                   size = 24)

predARNNX=forecast::forecast(fit_ARNNX,
                             xreg=xMat.train.new[1:24,1:6],
                           h= 24)

# Model summary
summary(fit_ARNNX)

# Generate the plot
plot(predARNNX)

# Forecasts
predARNNX$mean

# Actual
cpi.test.df$cpi_inflation_rate

################################### End of Code #############################

############## Sample Code: ERM ##################
# Calculate the Training MSE for the Empirical Risk Minimization Exercise
# Fitted Training Values
pred_arnnx = as.data.frame(fit_ARNNX.1$fitted)
# pred_arnnx$`fit_ARNNX.1$fitted`
# Impute the NA with 0
pred_arnnx[is.na(pred_arnnx)] <- 0
str(pred_arnnx$x)
pred_arnnx$x
# Actual Training Obs
str(cpi.df.train.ts)
# con_tr
# Calculate the Training - MSE
rmse(cpi.df.train.ts, pred_arnnx$x)**2
# rmse(con_tr, pred_warnnx$`fit_warnnx$FinalPrediction`)
# Alternative deduction
mean((cpi.df.train.ts - pred_arnnx$x)^2)
################################### End of Code #############################
