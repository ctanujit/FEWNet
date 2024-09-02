### Explore ARNN model with exogenous factors #####
# Read the base Table
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)
# View(head(cpi.train.df))
# View(tail(cpi.train.df))
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)
# head(cpi.test.df)
# tail(cpi.test.df)
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
library(wavelets)
# library(egcm)

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
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
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
cpi.df.test.ts

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(wavelets)

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

# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_Rate
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

con_tst = cpi.test.df$CPI_inflation_Rate

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
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -2.765453 3.773473 3.133153 -59.72818 63.99739
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 44.40477
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 5.449
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1]  6.884919  6.219972  6.449505  6.427833  8.015101  8.398981  7.829873  7.044319  6.707944  6.409492  7.462596
# [12]  7.906954  7.403285  6.363855  6.001451  7.406319  8.118303  8.869290  9.706772  9.520743  9.977713 11.586560
# [23] 12.414882 12.917521
# > con_tst
# [1] 9.634551 7.491857 6.840391 5.501618 5.448718 5.095541 5.063291 5.329154 5.625000 5.636574 5.902162 5.284787 3.686636
# [14] 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048 4.518828 4.837364

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
resid_ind <- abs(forecast_waveletnn - con_tst)
resid_ind
conf_waveletnn <- conformalRegressor(resid_ind, sigmas = NULL)
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

df_pi_ind_24 <- read.csv("india_CPI_24M.csv",header=TRUE)
str(df_pi_ind_24)
head(df_pi_ind_24)
df_pi_ind_24

# install.packages('plotrix')
# install.packages('ggthemes')

library(magrittr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(ggthemes)
library(ggplot2)


df_pi_ind_24 <- data.frame(df_pi_ind_24)
str(df_pi_ind_24)


plotCI(x = df_pi_ind_24$mon_index,
       y = df_pi_ind_24$Forecasts,
       li = df_pi_ind_24$lower_95,
       ui = df_pi_ind_24$Upper_95)

# Final Plots
ggplot(df_pi_ind_24, aes(mon_index, Forecasts,color = Forecasts))+
  xlab("Month Index: 24Months") +
  ylab("Forecasts - India & conformalized PI") +
  geom_point(size = 1.5)+
  geom_errorbar(aes(ymin = lower_95, ymax = Upper_95),alpha = 0.5) +
  geom_line() + 
  theme_linedraw()

####################### WARNN(X) with only EPU and GPRC as exog ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/india")
getwd()

# Train Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
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
  cpi.train.df$gprc_ind
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$log_epu,
  cpi.test.df$gprc_ind
),
ncol=2)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:2]


str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
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
con_tr = cpi.train.df$CPI_inflation_Rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$log_epu,
  cpi.train.df$gprc_ind
)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('log_epu',
                       'gprc_ind')
xreg_tst = cbind(
  cpi.test.df$log_epu,
  cpi.test.df$gprc_ind
)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('log_epu',
                        'gprc_ind')

con_tst = cpi.test.df$CPI_inflation_Rate

head(xreg_tr)
head(xreg_tst)
# View(tail(xreg_tst))

xreg_tr[1:6]
xreg_tr

####################### Proposed WARNNX ##########################
# source("warnnx.R")
set.seed(43)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 18,#best=18
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -1.379271 2.348076 1.657521 -28.93306 33.37194
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 25.64367
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 2.882665
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1]  8.685350  8.084798  7.738361  7.166208  6.371102  6.244053  6.598069  7.438622  6.638943  5.909177
# [11]  4.989831  3.807325  3.762364  3.962688  4.957437  6.010948  6.223018  6.633511  6.458059  6.254740
# [21]  7.895177  9.485391 10.563710 10.896941
# > con_tst
# [1] 9.634551 7.491857 6.840391 5.501618 5.448718 5.095541 5.063291 5.329154 5.625000 5.636574 5.902162
# [12] 5.284787 3.686636 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048
# [23] 4.518828 4.837364


####################### WARNN - Univariate forecasting ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/india")
getwd()

# Train Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_univariateCPI_202201.csv",header=TRUE)
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
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

str(cpi.test.df)
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
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
con_tr = cpi.train.df$CPI_inflation_Rate
con_tst = cpi.test.df$CPI_inflation_Rate



####################### Proposed WARNN ###########################
set.seed(42)
# source("warnn.R")
fit_warnn = WaveletFittingnar(ts(con_tr),
                              Waveletlevels = floor(log(length(con_tr))),
                              boundary = "periodic",
                              FastFlag = TRUE,
                              MaxARParam = 24, # best = 24
                              NForecast = 24)
fore_warnn = as.data.frame(fit_warnn$Finalforecast, h = 24)
forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
fore_warnn$`fit_warnn$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
# ME     RMSE      MAE      MPE     MAPE
# Test set 1.074544 1.678916 1.358361 21.28823 26.57506
# > smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
# [1] 33.51361
# > mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
# [1] 2.362384
# > fore_warnn$`fit_warnn$Finalforecast`
# [1] 8.583267 7.221719 7.140681 6.521660 5.817681 5.432084 5.095313 6.043475 5.287317 4.160826 3.228708
# [12] 1.885204 1.376365 2.286975 2.875312 3.122447 3.305949 3.315920 3.151979 2.247184 2.627827 3.374298
# [23] 4.311072 5.470992
# > con_tst
# [1] 9.634551 7.491857 6.840391 5.501618 5.448718 5.095541 5.063291 5.329154 5.625000 5.636574 5.902162
# [12] 5.284787 3.686636 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048
# [23] 4.518828 4.837364