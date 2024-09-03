################## EWNet : Russia : 12M Forecast horizon ###############
####################### WARNN(X) with only EPU and GPRC as exog ##########################
### Explore EWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/russia")
getwd()

setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/russia")
getwd()

# Train Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:215,1:4]
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[216:227,1:4]
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
  cpi.train.df$log_epu,
  cpi.train.df$gprc_rus
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
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
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
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
# ME     RMSE      MAE     MPE    MAPE
# Test set 1.817707 2.490327 1.817707 25.0658 25.0658
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 32.17596
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 4.875868
# > fit_warnnx$Finalforecast
# [1] 4.739141 5.169016 5.332567 5.463616 5.259737 5.105207 4.807665 4.291732 3.865080 3.658401 3.580750 3.624745
# > con_tst
# [1] 4.912471 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086 8.135334 8.403766
