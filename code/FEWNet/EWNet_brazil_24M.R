####################### EWNet with only EPU and GPRC as exog ##########################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/brazil")
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
set.seed(50)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 1, 
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
# Forecasts:
# 3.35
# 3.38
# 3.43
# 3.15
# 3.32
# 3.87
# 4.30
# 4.68
# 5.10
# 5.31
# 5.72
# 6.08
# 6.33
# 6.57
# 7.00
# 6.85
# 6.14
# 5.90
# 5.89
# 5.68
# 5.27
# 5.53
# 5.50
# 5.08
con_tst
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

