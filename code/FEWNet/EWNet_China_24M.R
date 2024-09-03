####################### EWNet with only EPU and GPRC as exog ##########################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

####################### EWNet with only EPU and GPRC as exog ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/china")
getwd()

# Train Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

cpi.train.df<-cpi.df[13:203,1:4]
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[204:227,1:4]
str(cpi.test.df)


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


# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  cpi.train.df$log_scmp_epu,
  cpi.train.df$gprc_chn
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
  cpi.test.df$log_scmp_epu,
  cpi.test.df$gprc_chn
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


# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
  cpi.train.df$log_scmp_epu,
  cpi.train.df$gprc_chn
)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('log_scmp_epu',
                       'gprc_chn')
xreg_tst = cbind(
  cpi.test.df$log_scmp_epu,
  cpi.test.df$gprc_chn
)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('log_scmp_epu',
                        'gprc_chn')

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
                               MaxARParam = 6,#best= 6
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# p = 6
# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
#               ME     RMSE     MAE       MPE     MAPE
# Test set -3.292997 3.761563 3.33431 -125.2685 560.7262
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 106.3732
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 4.690374
# > fit_warnnx$Finalforecast
# [1] 4.598195 4.998666 5.106519 5.158600 5.005163 5.275178 5.405838 5.551617 5.832642 5.848407 5.776095 5.739807 5.552272
# [14] 5.298359 5.281323 5.298488 5.245974 5.265232 4.931706 4.677489 4.616641 4.333165 4.205348 4.127598
# > con_tst
# [1]  4.4131455  5.4205607  5.1803885  4.2711235  3.2467532  2.4118738  2.5069638  1.7576318  2.3875115  1.7304189
# [11]  0.5415162 -0.4496403  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494  1.2401969  1.9125597
# [21]  0.6415800  0.4613803  1.4328405  2.4772978
