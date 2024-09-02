############################### Code for Russia ##################################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

library(lubridate)
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(wavelets)
library(nnet)


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
xMat.test.new[1:6,1:6]

cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
cpi.df.test.ts

##################### LIBRARY ###########################################################
# install.packages("Metrics")
# install.packages("nonlinearTseries")
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)
library(wavelets)

################### Code for the WARNNX in R #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='c6', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
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

# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_rate
xreg_tr = cbind(
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

xreg_tr[1:6]
xreg_tr
####################### Proposed WARNNX ##########################
# install.packages("wavelets")
library(wavelets)
#New Method -- Final selected Model
set.seed(43)
fit_warnnx = WaveletFittingnar(ts(con_tr), 
                               Waveletlevels = floor(log(length(con_tr))), 
                               boundary = "periodic", 
                               FastFlag = TRUE, 
                               MaxARParam = 6, 
                               NForecast = 12)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 12)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst


# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE      MPE     MAPE
# Test set 0.6186944 1.364609 0.997748 4.939277 11.62725
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 12.41881
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 1.76421
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 4.059374 5.157219 5.777332 6.323459 7.347984 8.179364 8.506026 8.622855 8.366697 7.923368 7.892107 8.286331
# > con_tst
# [1]  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823  9.679774 10.246209 10.672622
# [12] 10.738501

########################## End of Code: Brazil ############################################

############################### Code for Russia ##################################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/russia")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)


# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
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
  mraout <- wavelets::modwt(ts, filter='c6', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
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

# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
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

xreg_tr[1:6]
xreg_tr

####################### Proposed WARNNX ##########################
#New Method - Final Model
library(wavelets)
set.seed(45)
fit_warnnx = WaveletFittingnar(ts(con_tr), 
                               Waveletlevels = floor(log(length(con_tr))), 
                               boundary = "periodic", 
                               FastFlag = TRUE, 
                               MaxARParam = 1, 
                               NForecast = 12)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 12)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME      RMSE       MAE       MPE     MAPE
# Test set -0.2007889 0.5706644 0.4803937 -4.523953 7.993146
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 7.659172
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 1.288621
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 6.099415 5.834485 6.112447 6.156992 6.236965 6.230417 6.404736 6.700625 6.967185 7.265560 7.485577 7.625210
# > con_tst
# [1] 4.912471 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086 8.135334 8.403766

########################## End of Code: Russia ############################################

############################### Code for India ##################################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/india")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_12M_R.csv",header=TRUE)
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

################### Code for the WARNNX in R #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='c6', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
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

# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_Rate
xreg_tr = cbind(
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

xreg_tr[1:6]
xreg_tr
####################### Proposed WARNNX ##########################
#New Method - Final Model
set.seed(45)
fit_warnnx = WaveletFittingnar(ts(con_tr), 
                               Waveletlevels = floor(log(length(con_tr))), 
                               boundary = "periodic", 
                               FastFlag = TRUE, 
                               MaxARParam = 18, 
                               NForecast = 12)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 12)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)

fore_warnnx$`fit_warnnx$Finalforecast` # MaxARParam = 18
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -1.716205 2.148346 1.771946 -40.82946 41.88839
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 31.36731
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 3.481074
# > 
#   > fore_warnnx$`fit_warnnx$Finalforecast` # MaxARParam = 18
# [1] 7.465236 7.085171 6.979281 7.605271 7.272441 7.044457 5.727166 4.929432 4.882606 5.420013 6.008262 6.994789
# > con_tst
# [1] 3.686636 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048 4.518828 4.837364

########################## End of Code: India ############################################

############################### Code for China ##################################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_12M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_12M_R.csv",header=TRUE)
str(cpi.test.df)

# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
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
  mraout <- wavelets::modwt(ts, filter='c6', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)
  
{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;
  AllWaveletPrediction <- NULL
  
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

# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_chn_cycle_cf_l1,
  cpi.train.df$gprc_chn_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('epu_cycle_cf_l1',
                       'epu_trend_hp_l1',
                       'gprc_chn_cycle_cf_l1',
                       'gprc_chn_trend_hp_l1',
                       'cpi_cycle_cf_l1',
                       'cpi_trend_hp_l1')
xreg_tst = cbind(
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_chn_cycle_cf_l1,
  cpi.test.df$gprc_chn_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('epu_cycle_cf_l1',
                        'epu_trend_hp_l1',
                        'gprc_chn_cycle_cf_l1',
                        'gprc_chn_trend_hp_l1',
                        'cpi_cycle_cf_l1',
                        'cpi_trend_hp_l1')

con_tst = cpi.test.df$cpi_inflation_rate


xreg_tr[1:6]
xreg_tr
####################### Proposed WARNNX ##########################

#New Method - Final Model
set.seed(45)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 18, # 1 ,18/15 gave the best results so far
                               NForecast = 12)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 12)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -2.303974 2.394816 2.303974 -141.9635 427.0167
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 123.3938
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 3.470152
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 2.050157 2.000419 2.569482 2.724160 3.554041 4.222349 4.149496 3.936406 3.445190 3.451933 3.025039 3.199172
# > con_tst
# [1]  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494  1.2401969  1.9125597  0.6415800  0.4613803
# [11]  1.4328405  2.4772978

########################## End of Code: China ############################################
