####################### EWNet with only EPU and GPRC as exog ##########################
### Explore EWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Train Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:215,1:4]
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[216:227,1:4]
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


# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  cpi.train.df$log_epu,
  cpi.train.df$gprc_bra
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
  cpi.test.df$log_epu,
  cpi.test.df$gprc_bra
),
ncol=2)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:2]


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
                                             # xreg = xreg_tr[,1:6],
                                             # Should this be set against the test dataset
                                             # xreg = xreg_tst[,1:6],
                                             # xreg = xreg_tst[,1:6],
                                             xreg = as.data.frame(xMat.train.new[1:12,1:2]),
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

xreg_tr[1:6]
xreg_tr
####################### Proposed WARNNX ##########################

# source("warnnx.R")
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr), 
                               Waveletlevels = floor(log(length(con_tr))), 
                               boundary = "periodic", 
                               FastFlag = TRUE, 
                               MaxARParam = 10, 
                               NForecast = 12)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 12)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# p=10
# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE      MPE     MAPE
# Test set 3.360047 3.625274 3.360047 41.78721 41.78721
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 54.03042
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 5.941208
# > fit_warnnx$Finalforecast
# [1] 3.888054 3.119456 2.721566 2.739980 3.269284 4.169234 5.058179 5.801685 6.267789 6.088018 5.514202 4.908436
# > con_tst
# [1]  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823  9.679774 10.246209 10.672622
# [12] 10.738501
