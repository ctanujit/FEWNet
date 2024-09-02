### Explore ARNN model with exogenous factors #####
# Read the base Table
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/russia")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_24M_R.csv",header=TRUE)
# View(head(cpi.train.df))
# View(tail(cpi.train.df))
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_24M_R.csv",header=TRUE)
# View(head(cpi.test.df))
# View(tail(cpi.test.df))
str(cpi.test.df)

# Convert the Date
library(lubridate)
# Use Series cpi, EPU and GPR for this exercise
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
# library(egcm)

str(cpi.train.df)
str(cpi.test.df)

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

library(nnet)
library(forecast)

# create a matrix of external regressors
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

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)

################### Code for the WARNNX in R #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='bl14', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
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
                                      # xreg = xreg_tr[,1:6],
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

head(cpi.train.df)
head(cpi.test.df)
str(cpi.train.df)
str(cpi.test.df)

# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_rus_cycle_cf_l1,
  cpi.train.df$gprc_rus_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('epu_cycle_cf_l1',
                       'epu_trend_hp_l1',
                       'gprc_rus_cycle_cf_l1',
                       'gprc_rus_trend_hp_l1',
                       'cpi_cycle_cf_l1',
                       'cpi_trend_hp_l1')
xreg_tst = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_rus_cycle_cf_l1,
  cpi.test.df$gprc_rus_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('epu_cycle_cf_l1',
                        'epu_trend_hp_l1',
                        'gprc_rus_cycle_cf_l1',
                        'gprc_rus_trend_hp_l1',
                        'cpi_cycle_cf_l1',
                        'cpi_trend_hp_l1')

con_tst = cpi.test.df$cpi_inflation_rate

head(xreg_tr)
head(xreg_tst)
# View(tail(xreg_tst))
floor(log(length(con_tr)))
length(con_tr)
ts(con_tr)

xreg_tr[1:6]
xreg_tr

####################### Proposed WARNNX ##########################
# source("warnnx.R")
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 18, #best=18
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
# fit_warnnx$Finalforecast
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
con_tst
fore_warnnx$`fit_warnnx$Finalforecast`

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -3.269422 3.808417 3.269422 -85.30232 85.30232
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 51.37681
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 9.920628
# > con_tst
# [1] 3.046190 2.423893 2.311721 2.546287 3.098436 3.026511 3.211885 3.366394 3.573624 3.666989 3.977587 4.423442 4.912471
# [14] 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086 8.135334 8.403766
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 10.090303  8.741652  8.047883  7.763454  7.514259  7.282357  6.618545  5.509548  4.481694  4.215212  5.072520
# [12]  6.551320  7.536779  7.580871  6.606227  5.846102  6.420188  7.925384  9.629435 11.222886 12.188161 12.334289
# [23] 12.241744 12.428408

# Calculation of residuals
resid <- abs(fore_warnnx$`fit_warnnx$Finalforecast` - con_tst)
resid

# resid <- residuals(fore_warnnx$`fit_warnnx$Finalforecast`,con_tst)

checkresiduals(resid)
Box.test(resid,lag=12, fitdf=0, type="Lj")
############## A distribution - free measurement (conformal) of uncertainties for Forecasting ######
# Document Link: https://cran.r-project.org/web/packages/caretForecast/caretForecast.pdf
# packageVersion("methods")
# sessionInfo()

library(caretForecast)
forecast_waveletnn <- fore_warnnx$`fit_warnnx$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_rus <- abs(forecast_waveletnn - con_tst)
resid_rus
conf_waveletnn <- conformalRegressor(resid_rus, sigmas = NULL)
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

# Get the forecast and PIs and Actual Data elements
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/CPI_BRIC_24M")
getwd()

df_pi_rus_24 <- read.csv("russia_CPI_24M.csv",header=TRUE)
str(df_pi_rus_24)
head(df_pi_rus_24)
df_pi_rus_24

# install.packages('plotrix')
# install.packages('ggthemes')

library(magrittr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggthemes)
library(ggplot2)


df_pi_rus_24 <- data.frame(df_pi_rus_24)
str(df_pi_rus_24)


plotCI(x = df_pi_rus_24$mon_index,
       y = df_pi_rus_24$Forecasts,
       li = df_pi_rus_24$lower_95,
       ui = df_pi_rus_24$Upper_95)

# Final Plots
ggplot(df_pi_rus_24, aes(mon_index, Forecasts,color = Forecasts))+
  xlab("Month Index: 24Months") +
  ylab("Forecasts - Russia & conformalized PI") +
  geom_point(size = 1.5)+
  geom_errorbar(aes(ymin = lower_95, ymax = Upper_95),alpha = 0.5) +
  geom_line() + 
  theme_linedraw()
####################### WARNN(X) with only EPU and GPRC as exog ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/russia")
getwd()

# Train Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:203,1:4]
# View(head(cpi.train.df))
# View(tail(cpi.train.df))
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[204:227,1:4]
# View(head(cpi.test.df))
# View(tail(cpi.test.df))
str(cpi.test.df)

# Convert the Date
library(lubridate)
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(egcm)
library(nnet)
library(forecast)

str(cpi.train.df)
str(cpi.test.df)
# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$log_epu,
  cpi.train.df$gprc_rus
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$log_epu,
  cpi.test.df$gprc_rus
),
ncol=2)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:2]

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

################### Code for the WARNNX in R #######################
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
                                      # xreg = xreg_tr[,1:2],
                                      xreg = as.data.frame(xMat.train.new[,1:2]),
                                      p = MaxARParam,
                                      repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit,
                                             xreg = as.data.frame(xMat.train.new[1:24,1:2]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

str(cpi.train.df)
# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$log_epu,
  cpi.train.df$gprc_rus
)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('log_epu',
                       'gprc_rus')
xreg_tst = cbind(
  cpi.test.df$log_epu,
  cpi.test.df$gprc_rus
)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('log_epu',
                        'gprc_rus')

con_tst = cpi.test.df$cpi_inflation_rate

head(xreg_tr)
head(xreg_tst)
# View(tail(xreg_tst))

xreg_tr[1:6]
xreg_tr

####################### Proposed WARNNX ##########################
# source("warnnx.R")
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 12, #best=12
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE      MPE     MAPE
# Test set 1.155456 1.729626 1.185553 19.23253 20.38546
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 24.37453
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 3.597406
# > fit_warnnx$Finalforecast
[1] 3.172599 2.658657 2.246787 2.166181 2.194559 2.342202 2.697643 2.838633 3.054549 3.481331 3.777914
[12] 4.049802 4.481005 4.965174 5.160354 5.078672 4.696114 4.241778 3.896828 3.915345 4.069945 4.013270
[23] 4.067417 4.385407
# > con_tst
# [1] 3.046190 2.423893 2.311721 2.546287 3.098436 3.026511 3.211885 3.366394 3.573624 3.666989 3.977587
# [12] 4.423442 4.912471 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086
# [23] 8.135334 8.403766

####################### WARNN - Univariate forecasting ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/russia")
getwd()

# Train Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_univariate_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:203,1:2]
str(cpi.train.df)
# Test Data
cpi.test.df<-cpi.df[204:227,1:2]
str(cpi.test.df)

# Convert the Date
library(lubridate)
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(egcm)
library(nnet)
library(forecast)

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

str(cpi.test.df)
cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)

################### Code for the WARNN in R #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts,
                            filter='haar',
                            n.levels=Wvlevels,boundary=bndry,
                            fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)
  
{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,
                       bndry=boundary,
                       FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL
  
  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts), p=MaxARParam, repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit, h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

str(cpi.train.df)
# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
con_tst = cpi.test.df$cpi_inflation_rate

####################### Proposed WARNN ###########################
set.seed(42)
# source("warnn.R")
fit_warnn = WaveletFittingnar(ts(con_tr),
                              Waveletlevels = floor(log(length(con_tr))),
                              boundary = "periodic",
                              FastFlag = TRUE,
                              MaxARParam = 12, #best = 
                              NForecast = 24)
fore_warnn = as.data.frame(fit_warnn$Finalforecast, h = 24)
forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
fore_warnn$`fit_warnn$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
# ME      RMSE       MAE        MPE     MAPE
# Test set -0.1214149 0.4775479 0.4010705 -0.7652004 9.238027
# > smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
# [1] 9.431578
# > mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
# [1] 1.216996
# > fore_warnn$`fit_warnn$Finalforecast`
# [1] 3.201315 2.765635 2.352909 2.243729 2.286458 2.353489 2.635181 2.950392 3.316737 3.786139 4.236885
# [12] 4.634855 5.036506 5.496135 5.874532 6.167725 6.453497 6.730780 6.984148 7.256629 7.539235 7.773769
# [23] 7.997238 8.223145
# > con_tst
# [1] 3.046190 2.423893 2.311721 2.546287 3.098436 3.026511 3.211885 3.366394 3.573624 3.666989 3.977587
# [12] 4.423442 4.912471 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086
# [23] 8.135334 8.403766



