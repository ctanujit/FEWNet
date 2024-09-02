### Explore ARNN model with exogenous factors #####
# Read the base Table
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
# View(head(cpi.train.df))
# View(tail(cpi.train.df))
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
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

cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
cpi.df.test.ts

library(nnet)
library(forecast)

# create a matrix of external regressors
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
xMat.test.new[1:6,1:6]

cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
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

# View(head(cpi.train.df))
# View(head(cpi.test.df))
str(cpi.train.df)
str(cpi.test.df)

# floor(log(length(con_tr)))
# length(con_tr)
# ts(con_tr)
# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_ind_cycle_cf_l1,
  cpi.train.df$gprc_ind_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('epu_cycle_cf_l1',
                       'epu_trend_hp_l1',
                       'gprc_ind_cycle_cf_l1',
                       'gprc_ind_trend_hp_l1',
                       'cpi_cycle_cf_l1',
                       'cpi_trend_hp_l1')
xreg_tst = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_ind_cycle_cf_l1,
  cpi.test.df$gprc_ind_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('epu_cycle_cf_l1',
                        'epu_trend_hp_l1',
                        'gprc_ind_cycle_cf_l1',
                        'gprc_ind_trend_hp_l1',
                        'cpi_cycle_cf_l1',
                        'cpi_trend_hp_l1')

con_tst = cpi.test.df$CPI_inflation_rate

head(xreg_tr)
head(xreg_tst)
# tail(xreg_tst)

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
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set 0.5901395 2.822059 2.284711 -16.96181 48.25514
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 42.54726
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 4.654747
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 5.417258 5.027756 4.625646 4.639536 4.575335 4.753262 4.883850 4.943836 4.836718 4.610028 4.439504 4.554256 4.905307
# [14] 5.356243 5.362974 4.711728 4.484754 4.848138 5.193589 5.295612 5.439580 5.163085 4.849177 5.110540
# > con_tst
# [1]  4.306152  4.191771  4.005114  3.303158  2.399279  1.877727  2.132417  2.305625  2.438465  3.135329  3.918350
# [12]  4.311223  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823  9.679774 10.246209
# [23] 10.672622 10.738501

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
resid_bzl <- abs(forecast_waveletnn - con_tst)
resid_bzl
conf_waveletnn <- conformalRegressor(resid_bzl, sigmas = NULL)
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

df_pi_bzl_24 <- read.csv("brazil_CPI_24M.csv",header=TRUE)
str(df_pi_bzl_24)
head(df_pi_bzl_24)
df_pi_bzl_24

# install.packages('plotrix')
# install.packages('ggthemes')

library(magrittr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggthemes)
library(ggplot2)


df_pi_bzl_24 <- data.frame(df_pi_bzl_24)
str(df_pi_bzl_24)


plotCI(x = df_pi_bzl_24$mon_index,
       y = df_pi_bzl_24$Forecasts,
       li = df_pi_bzl_24$lower_95,
       ui = df_pi_bzl_24$Upper_95)

# Final Plots
ggplot(df_pi_bzl_24, aes(mon_index, Forecasts,color = Forecasts))+
  xlab("Month Index: 24Months") +
  ylab("Forecasts - Brazil & conformalized PI") +
  geom_point(size = 1.5)+
  geom_errorbar(aes(ymin = lower_95, ymax = Upper_95),alpha = 0.5) +
  geom_line() + 
  theme_linedraw()

####################### WARNN(X) with only EPU and GPRC as exog ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/brazil")
getwd()

# Train Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
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
# Use Series cpi, EPU and GPR for this exercise
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
  cpi.train.df$gprc_bra
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$log_epu,
  cpi.test.df$gprc_bra
),
ncol=2)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:2]

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
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
con_tr = cpi.train.df$CPI_inflation_rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$log_epu,
  cpi.train.df$gprc_bra
)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('log_epu',
                       'gprc_bra')
xreg_tst = cbind(
  cpi.test.df$log_epu,
  cpi.test.df$gprc_bra
)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('log_epu',
                        'gprc_bra')

con_tst = cpi.test.df$CPI_inflation_rate

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
                               MaxARParam = 24, #best=18
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE      MPE     MAPE
# Test set -0.7940249 1.682922 1.457801 -31.2201 39.97193
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 30.51355
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 2.970044
# > fit_warnnx$Finalforecast
# [1] 3.346746 3.435384 3.805176 3.715509 3.452339 3.750360 4.353658 4.767301 5.209719 5.707817 6.238448
# [12] 6.465695 6.949665 7.045417 7.162355 7.454367 7.701461 7.818778 8.144175 8.583443 8.934715 9.132812
# [23] 9.086841 8.985476
# > con_tst
# [1]  4.306152  4.191771  4.005114  3.303158  2.399279  1.877727  2.132417  2.305625  2.438465  3.135329
# [11]  3.918350  4.311223  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823
# [21]  9.679774 10.246209 10.672622 10.738501

####################### WARNN - Univariate forecasting ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/brazil")
getwd()

# Train Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_univariateCPI_202201.csv",header=TRUE)
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
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

str(cpi.test.df)
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
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
con_tr = cpi.train.df$CPI_inflation_rate
con_tst = cpi.test.df$CPI_inflation_rate

####################### Proposed WARNN ###########################
set.seed(42)
# source("warnn.R")
fit_warnn = WaveletFittingnar(ts(con_tr),
                              Waveletlevels = floor(log(length(con_tr))),
                              boundary = "periodic",
                              FastFlag = TRUE,
                              MaxARParam = 15, #best=15
                              NForecast = 24)
fore_warnn = as.data.frame(fit_warnn$Finalforecast, h = 24)
forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
fore_warnn$`fit_warnn$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
# ME     RMSE      MAE      MPE     MAPE
# Test set -0.2370285 1.357131 1.226623 -17.8093 30.85368
# > smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
# [1] 25.80289
# > mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
# [1] 2.499054
# > fore_warnn$`fit_warnn$Finalforecast`
# [1] 3.330975 3.310868 3.460707 3.158914 2.935413 3.240336 3.894524 4.188764 4.660543 5.066757 5.465540
# [12] 5.689288 6.075854 6.079937 6.126979 6.728654 7.060268 7.345248 7.681592 7.999838 8.283375 8.658321
# [23] 8.681329 8.755718
# > con_tst
# [1]  4.306152  4.191771  4.005114  3.303158  2.399279  1.877727  2.132417  2.305625  2.438465  3.135329
# [11]  3.918350  4.311223  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823
# [21]  9.679774 10.246209 10.672622 10.738501
