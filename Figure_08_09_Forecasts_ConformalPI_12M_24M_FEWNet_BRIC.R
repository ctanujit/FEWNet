################### Forecast generation using FEWNet: - BRIC ###############
set.seed(20240101) # For reproducibility, we are using this seed value
################################ Brazil: 12Months ###################################
# Set the working directory
setwd("/FEWNet/dataset/brazil")
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
                                   MaxARParam = 6,
                                   NForecast = 12)
fore_fewnet_12M = as.data.frame(fit_fewnet_12M$Finalforecast, h = 12)
# Performance Evaluation
forecast::accuracy(fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`, con_tst)
smape(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)*100
mase(con_tst, fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`)
# Check for the forecasts: 12M
fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# Forecasts
# 4.51
# 4.97
# 5.53
# 5.86
# 6.29
# 7.02
# 7.62
# 8.06
# 8.33
# 8.40
# 8.30
# 8.21

# Comment: These values are expected to change a bit with respect 
# to the change in the run time environment

# Check for the test/evaluation data series
con_tst
# [1]  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823  9.679774
# [10] 10.246209 10.672622 10.738501
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(4.51, 4.97, 5.53, 5.86 , 6.29,7.02,7.62,8.06,8.33,8.40,8.30,8.21)
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
# Comment:These values are expected to change a bit with respect 
# to the change in the run time environment
# lower_95  upper_95
# 1  1.981499  7.038501
# 2  2.441499  7.498501
# 3  3.001499  8.058501
# 4  3.331499  8.388501
# 5  3.761499  8.818501
# 6  4.491499  9.548501
# 7  5.091499 10.148501
# 8  5.531499 10.588501
# 9  5.801499 10.858501
# 10 5.871499 10.928501
# 11 5.771499 10.828501
# 12 5.681499 10.738501
#################### End of Code ###########################

################################ Brazil: 24Months ###################################
# Setting up the working directory
setwd("/FEWNet/dataset/brazil")
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
# Forecasts:
# 4.62
# 5.45
# 5.63
# 5.26
# 5.02
# 5.08
# 5.21
# 5.20
# 5.10
# 5.31
# 5.91
# 6.04
# 6.07
# 5.98
# 6.05
# 6.33
# 6.75
# 6.90
# 6.94
# 6.97
# 6.96
# 6.87
# 6.58
# 6.55
# Check for the evaluation data series
con_tst
# Test data
# 4.31
# 4.19
# 4.01
# 3.30
# 2.40
# 1.88
# 2.13
# 2.31
# 2.44
# 3.14
# 3.92
# 4.31
# 4.52
# 4.56
# 5.20
# 6.10
# 6.76
# 8.06
# 8.35
# 8.99
# 9.68
# 10.25
# 10.67
# 10.74

#################### End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(4.62,
                        5.45,
                        5.63,
                        5.26,
                        5.02,
                        5.08,
                        5.21,
                        5.20,
                        5.10,
                        5.31,
                        5.91,
                        6.04,
                        6.07,
                        5.98,
                        6.05,
                        6.33,
                        6.75,
                        6.90,
                        6.94,
                        6.97,
                        6.96,
                        6.87,
                        6.58,
                        6.55)
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
# Comment:These values are expected to change a bit with respect 
# to the change in the run time environment
# Conformal PIs
# lower_95	Upper_95
# 0.527285	8.715227
# 1.3514411	9.539383
# 1.5387731	9.726715
# 1.1670529	9.354995
# 0.9267418	9.114683
# 0.9894375	9.177379
# 1.112407	9.300349
# 1.1071832	9.295125
# 1.0022546	9.190196
# 1.2131934	9.401135
# 1.8180749	10.006017
# 1.9449257	10.132867
# 1.9799676	10.167909
# 1.8869712	10.074913
# 1.9565949	10.144537
# 2.2372505	10.425192
# 2.659427	10.847369
# 2.8050469	10.992989
# 2.8468805	11.034822
# 2.8759586	11.0639
# 2.8672853	11.055227
# 2.778232	10.966174
# 2.4846805	10.672622
# 2.4603745	10.648316
#################### End of Code ###########################

################################ Russia: 12Months ###################################
# Set the working directory
setwd("/FEWNet/dataset/russia")
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

# Create the training and Test data objects: CPI inflation Series for Russia
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
# Forecasts:
# 5.44
# 6.38
# 6.83
# 6.78
# 6.51
# 6.14
# 5.84
# 5.80
# 6.05
# 6.50
# 7.02
# 7.51
# Check for the evaluation data series
con_tst
# [1] 4.912471 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086 8.135334
# [12] 8.403766
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(5.44,
                        6.38,
                        6.83,
                        6.78,
                        6.51,
                        6.14,
                        5.84,
                        5.80,
                        6.05,
                        6.50,
                        7.02,
                        7.51)
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

# Conformal PIs
# lower_95	Upper_95
# 4.254055	6.624345
# 5.194607	7.564896
# 5.647229	8.017518
# 5.597583	7.967872
# 5.320661	7.69095
# 4.951183	7.321473
# 4.650137	7.020427
# 4.611981	6.982271
# 4.86377	7.23406
# 5.315563	7.685852
# 5.835001	8.20529
# 6.321437	8.691727

#################### End of Code ###########################

################################ Russia: 24Months ###################################
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
# Forecasts:
# 4.34
# 4.45
# 4.25
# 3.82
# 3.36
# 3.17
# 3.28
# 3.02
# 3.05
# 3.39
# 4.00
# 4.67
# 5.47
# 6.19
# 7.03
# 7.89
# 8.85
# 9.52
# 9.89
# 10.44
# 10.76
# 10.97
# 11.31
# 11.73
# Check for the evaluation data series
con_tst
# [1] 3.046190 2.423893 2.311721 2.546287 3.098436 3.026511 3.211885 3.366394 3.573624 3.666989 3.977587
# [12] 4.423442 4.912471 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086
# [23] 8.135334 8.403766
####################End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# forecast_waveletnn
set.seed(42)
forecast_waveletnn <- c(4.34,
                        4.45,
                        4.25,
                        3.82,
                        3.36,
                        3.17,
                        3.28,
                        3.02,
                        3.05,
                        3.39,
                        4.00,
                        4.67,
                        5.47,
                        6.19,
                        7.03,
                        7.89,
                        8.85,
                        9.52,
                        9.89,
                        10.44,
                        10.76,
                        10.97,
                        11.31,
                        11.73)
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

# Conformal PIs
# lower_95	Upper_95
# 0.37418395	8.308571
# 0.48539722	8.419784
# 0.28256304	8.21695
# -0.14459568	7.789791
# -0.61047067	7.323916
# -0.79565073	7.138736
# -0.68833076	7.246056
# -0.94354712	6.99084
# -0.92022131	7.014166
# -0.57871828	7.355669
# 0.03580662	7.970194
# 0.70540312	8.63979
# 1.50448115	9.438868
# 2.21840428	10.152791
# 3.05819914	10.992586
# 3.92589283	11.86028
# 4.88217072	12.816558
# 5.55089986	13.485287
# 5.92519293	13.85958
# 6.46906949	14.403457
# 6.79414201	14.728529
# 7.00116759	14.935555
# 7.34527603	15.279663
# 7.76097002	15.695357
#################### End of Code ###########################

################################ India: 12Months ###################################
# Set the working directory
setwd("/FEWNet/dataset/india")
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
set.seed(45) 
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
# Forecasts:
# 4.37
# 5.04
# 5.83
# 6.55
# 6.04
# 5.41
# 4.80
# 4.40
# 4.26
# 4.56
# 4.57
# 5.22
# Check for the evaluation data series
con_tst
# [1] 3.686636 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048 4.518828
# [12] 4.837364
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(4.37,
                        5.04,
                        5.83,
                        6.55,
                        6.04,
                        5.41,
                        4.80,
                        4.40,
                        4.26,
                        4.56,
                        4.57,
                        5.22)
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

# Conformal PIs
# lower_95	Upper_95
# 2.492734	6.237596
# 3.162966	6.907828
# 3.962357	7.707219
# 4.676217	8.421079
# 4.167689	7.912551
# 3.53982	7.284682
# 2.929654	6.674516
# 2.523675	6.268536
# 2.387585	6.132447
# 2.684132	6.428993
# 2.695545	6.440407
# 3.352222	7.097084

#################### End of Code ###########################

################################ India: 24Months ###################################
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
# Forecasts:
# 7.69
# 6.34
# 5.92
# 5.11
# 4.53
# 4.15
# 3.95
# 5.57
# 5.37
# 4.25
# 3.85
# 3.37
# 3.86
# 4.75
# 5.39
# 5.65
# 4.78
# 4.36
# 4.87
# 4.06
# 4.28
# 4.73
# 5.04
# 4.72
# Check for the evaluation data series
con_tst
# [1] 9.634551 7.491857 6.840391 5.501618 5.448718 5.095541 5.063291 5.329154 5.625000 5.636574 5.902162
# [12] 5.284787 3.686636 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048
# [23] 4.518828 4.837364
####################End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(7.69,
                        6.34,
                        5.92,
                        5.11,
                        4.53,
                        4.15,
                        3.95,
                        5.57,
                        5.37,
                        4.25,
                        3.85,
                        3.37,
                        3.86,
                        4.75,
                        5.39,
                        5.65,
                        4.78,
                        4.36,
                        4.87,
                        4.06,
                        4.28,
                        4.73,
                        5.04,
                        4.72)
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

# Conformal PIs
# lower_95	Upper_95
# 5.750752	9.634551
# 4.397682	8.281481
# 3.975385	7.859184
# 3.169151	7.05295
# 2.58776	6.47156
# 2.204358	6.088157
# 2.008917	5.892717
# 3.629368	7.513167
# 3.424062	7.307861
# 2.306424	6.190223
# 1.912153	5.795952
# 1.424242	5.308041
# 1.921774	5.805573
# 2.810093	6.693892
# 3.448647	7.332446
# 3.707477	7.591277
# 2.838932	6.722731
# 2.415118	6.298917
# 2.929959	6.813758
# 2.121908	6.005708
# 2.337258	6.221057
# 2.785837	6.669636
# 3.096607	6.980406
# 2.773324	6.657123
#################### End of Code ###########################

################################ China: 12Months ###################################
# Set the working directory
setwd("/FEWNet/dataset/china")
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
set.seed(45)
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
# Forecasts:
# -0.19
# -0.45
# -0.17
# 0.50
# 1.24
# 2.03
# 2.44
# 3.15
# 2.86
# 3.34
# 4.45
# 5.44
# Check for the evaluation data series
con_tst
# [1]  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494  1.2401969  1.9125597  0.6415800
# [10]  0.4613803  1.4328405  2.4772978
####################End of Code###########################

####################### Conformal Prediction Intervals for 12M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_12M$`fit_fewnet_12M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(-0.19,
                        -0.45,
                        -0.17,
                        0.50,
                        1.24,
                        2.03,
                        2.44,
                        3.15,
                        2.86,
                        3.34,
                        4.45,
                        5.44)
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

# lower_95	Upper_95
# -3.2103773	2.825078
# -3.4656601	2.569795
# -3.1854621	2.849993
# -2.5217115	3.513744
# -1.7763805	4.259075
# -0.990514	5.044941
# -0.5795845	5.45587
# 0.134712	6.170167
# -0.155056	5.880399
# 0.322024	6.357479
# 1.4328405	7.468296
# 2.4227135	8.458169
#################### End of Code ###########################

################################ China: 24Months ###################################
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
# Forecasts:
# 4.10
# 3.71
# 3.68
# 3.25
# 3.28
# 3.31
# 3.58
# 3.63
# 3.40
# 3.39
# 3.24
# 2.90
# 3.20
# 3.75
# 3.38
# 3.42
# 3.06
# 2.82
# 2.58
# 2.39
# 2.44
# 2.16
# 1.88
# 2.08
# Check for the evaluation data series
con_tst
# [1]  4.4131455  5.4205607  5.1803885  4.2711235  3.2467532  2.4118738  2.5069638  1.7576318  2.3875115
# [10]  1.7304189  0.5415162 -0.4496403  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494
# [19]  1.2401969  1.9125597  0.6415800  0.4613803  1.4328405  2.4772978
####################End of Code###########################

####################### Conformal Prediction Intervals for 24M forecast numbers ##########################
library(caretForecast)

# Forecasts
# forecast_waveletnn <- fore_fewnet_24M$`fit_fewnet_24M$Finalforecast`
# forecast_waveletnn

forecast_waveletnn <- c(4.10,
                        3.71,
                        3.68,
                        3.25,
                        3.28,
                        3.31,
                        3.58,
                        3.63,
                        3.40,
                        3.39,
                        3.24,
                        2.90,
                        3.20,
                        3.75,
                        3.38,
                        3.42,
                        3.06,
                        2.82,
                        2.58,
                        2.39,
                        2.44,
                        2.16,
                        1.88,
                        2.08)
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

# Conformal PIs
# lower_95	Upper_95
# 0.29526393	7.900911
# -0.09294219	7.512705
# -0.12022088	7.485426
# -0.55765838	7.047989
# -0.52258372	7.083063
# -0.49304187	7.112605
# -0.22545443	7.380193
# -0.16891176	7.436735
# -0.40568415	7.199963
# -0.41351548	7.192132
# -0.55837627	7.047271
# -0.90188431	6.703763
# -0.60726274	6.998384
# -0.05392519	7.551722
# -0.4227999	7.182847
# -0.37849104	7.227156
# -0.73981253	6.865835
# -0.97932453	6.626323
# -1.22775845	6.377889
# -1.41390328	6.191744
# -1.36655515	6.239092
# -1.64664421	5.959003
# -1.92538746	5.68026
# -1.72254559	5.883102
#################### End of Code ###########################



con_tst = cpi.test.df$CPI_inflation_rate
