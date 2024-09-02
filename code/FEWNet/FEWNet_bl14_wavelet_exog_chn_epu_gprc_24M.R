### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)


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
library(wavelets)

# Thus, to obtain the Daubechies wavelet transform filter of length 4, the character string "d4" can be passed to wt.filter.
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
# source("warnnx.R")
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 12,#best=12
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > fore_warnnx$`fit_warnnx$Finalforecast`
# Forecasts:
# 4.03
# 4.10
# 3.87
# 3.60
# 3.84
# 2.63
# 2.73
# 2.73
# 2.91
# 3.29
# 3.09
# 3.21
# 3.03
# 3.03
# 3.41
# 3.90
# 3.08
# 3.61
# 3.19
# 3.17
# 3.30
# 3.43
# 4.45
# 4.81
# > con_tst
# [1]  4.4131455  5.4205607  5.1803885  4.2711235  3.2467532  2.4118738  2.5069638  1.7576318  2.3875115  1.7304189
# [11]  0.5415162 -0.4496403  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494  1.2401969  1.9125597
# [21]  0.6415800  0.4613803  1.4328405  2.4772978




