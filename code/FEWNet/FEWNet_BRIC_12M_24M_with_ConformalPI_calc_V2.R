############################# FEWNet for BRIC Countries: 12M and 24months forwards forecasts ####################
# Installing relevant package
library(wavelets)
################################ Brazil: 12Months ###################################
# Setting up the working directory
setwd("/dataset/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_ind_cycle_cf_l1,
  cpi.train.df$gprc_ind_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new
# create a matrix of external regressors: Test data -- We would not be using this matrix
xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_ind_cycle_cf_l1,
  cpi.test.df$gprc_ind_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_rate
con_tst = cpi.test.df$CPI_inflation_rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:12,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_12M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 6, #best=6
                                   NForecast = 12)
fore_fewnet_12M = as.data.frame(fit_fewnet_12M$Finalforecast, h = 12)
# Performance Evaluation
forecast::accuracy(fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)*100
mase(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)
# Check for the forecasts: 12M
fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ Brazil: 24Months ###################################
# Setting up the working directory
setwd("/dataset/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_ind_cycle_cf_l1,
  cpi.train.df$gprc_ind_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new
# create a matrix of external regressors: Test data -- We would not be using this matrix
xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_ind_cycle_cf_l1,
  cpi.test.df$gprc_ind_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_rate
con_tst = cpi.test.df$CPI_inflation_rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:24,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_24M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 18,#best=18
                                   NForecast = 24)
fore_fewnet_24M = as.data.frame(fit_fewnet_24M$Finalforecast, h = 24)
# Performance Evaluation
forecast::accuracy(fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)*100
mase(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)
# Check for the forecasts: 24M
fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# Check for the evaluation data series
con_tst
#################### End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ Russia: 12Months ###################################
# Setting up the working directory
setwd("/dataset/russia")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_rus_cycle_cf_l1,
  cpi.train.df$gprc_rus_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new
# create a matrix of external regressors: Test data -- We would not be using this matrix
xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_rus_cycle_cf_l1,
  cpi.test.df$gprc_rus_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
con_tst = cpi.test.df$cpi_inflation_rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:12,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_12M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 1, #best=1
                                   NForecast = 12)
fore_fewnet_12M = as.data.frame(fit_fewnet_12M$Finalforecast, h = 12)
# Performance Evaluation
forecast::accuracy(fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)*100
mase(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)
# Check for the forecasts: 12M
fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ Russia: 24Months ###################################
# Setting up the working directory
setwd("/dataset/russia")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_rus_cycle_cf_l1,
  cpi.train.df$gprc_rus_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new
# create a matrix of external regressors: Test data -- We would not be using this matrix
xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_rus_cycle_cf_l1,
  cpi.test.df$gprc_rus_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
con_tst = cpi.test.df$cpi_inflation_rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:24,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_24M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 18 ,#best=18
                                   NForecast = 24)
fore_fewnet_24M = as.data.frame(fit_fewnet_24M$Finalforecast, h = 24)
# Performance Evaluation
forecast::accuracy(fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)*100
mase(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)
# Check for the forecasts: 24M
fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ India: 12Months ###################################
# Setting up the working directory
setwd("/dataset/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_ind_cycle_cf_l1,
  cpi.train.df$gprc_ind_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new
# create a matrix of external regressors: Test data -- We would not be using this matrix
xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_ind_cycle_cf_l1,
  cpi.test.df$gprc_ind_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_Rate
con_tst = cpi.test.df$CPI_inflation_Rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:12,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_12M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 18, #best=18
                                   NForecast = 12)
fore_fewnet_12M = as.data.frame(fit_fewnet_12M$Finalforecast, h = 12)
# Performance Evaluation
forecast::accuracy(fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)*100
mase(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)
# Check for the forecasts: 12M
fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ India: 24Months ###################################
# Setting up the working directory
setwd("/dataset/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_ind_cycle_cf_l1,
  cpi.train.df$gprc_ind_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new
# create a matrix of external regressors: Test data -- We would not be using this matrix
xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_ind_cycle_cf_l1,
  cpi.test.df$gprc_ind_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_Rate
con_tst = cpi.test.df$CPI_inflation_Rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:24,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_24M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 12 ,#best=12
                                   NForecast = 24)
fore_fewnet_24M = as.data.frame(fit_fewnet_24M$Finalforecast, h = 24)
# Performance Evaluation
forecast::accuracy(fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)*100
mase(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)
# Check for the forecasts: 24M
fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ China: 12Months ###################################
# Setting up the working directory
setwd("/dataset/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
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
# create a matrix of external regressors: Test data -- We would not be using this matrix
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
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
con_tst = cpi.test.df$cpi_inflation_rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:12,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_12M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 18, #best=18
                                   NForecast = 12)
fore_fewnet_12M = as.data.frame(fit_fewnet_12M$Finalforecast, h = 12)
# Performance Evaluation
forecast::accuracy(fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)*100
mase(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)
# Check for the forecasts: 12M
fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

################################ China: 24Months ###################################
# Setting up the working directory
setwd("/dataset/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

########################### Data Processing ############################
# create a matrix of external regressors: Train data
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
# create a matrix of external regressors: Test data -- We would not be using this matrix
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
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Check for the datatype
str(cpi.train.df)
str(cpi.test.df)

# Create the training and Test data objects: CPI inflation Series for China
# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
con_tst = cpi.test.df$cpi_inflation_rate

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(nnet)

# Optionals
# library(lubridate)
# library(dLagM)
# library(tictoc)
# library(lmtest)
# library(tseries)
# library(pracma)

################### Code for the FEWNet Algorithm #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts),
                                      xreg = as.data.frame(xMat.train.new[,1:6]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:24,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

####################### Proposed FEWNet Model ##########################
set.seed(42)
fit_fewnet_24M = WaveletFittingnar(ts(con_tr),
                                   Waveletlevels = floor(log(length(con_tr))),
                                   boundary = "periodic",
                                   FastFlag = TRUE,
                                   MaxARParam = 12,#best=12
                                   NForecast = 24)
fore_fewnet_24M = as.data.frame(fit_fewnet_24M$Finalforecast, h = 24)
# Performance Evaluation
forecast::accuracy(fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)*100
mase(con_tst, fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`)
# Check for the forecasts: 24M
fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# Check for the evaluation data series
con_tst
####################End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

#################### End of Code ###########################

