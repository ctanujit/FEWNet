####################### EWNet with only EPU and GPRC as exog ##########################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/russia")
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
                               MaxARParam = 24, 
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
# Forecasts:
# 3.11
# 2.41
# 1.82
# 1.37
# 1.07
# 1.02
# 1.55
# 1.87
# 2.32
# 2.74
# 3.12
# 3.37
# 3.88
# 4.59
# 5.61
# 6.86
# 8.33
# 9.83
# 10.83
# 11.62
# 12.34
# 12.85
# 13.15
# 13.50
con_tst
# 3.05
# 2.42
# 2.31
# 2.55
# 3.10
# 3.03
# 3.21
# 3.37
# 3.57
# 3.67
# 3.98
# 4.42
# 4.91
# 5.19
# 5.67
# 5.78
# 5.52
# 6.01
# 6.51
# 6.47
# 6.69
# 7.41
# 8.14
# 8.40
con_tst
# 3.05
# 2.42
# 2.31
# 2.55
# 3.10
# 3.03
# 3.21
# 3.37
# 3.57
# 3.67
# 3.98
# 4.42
# 4.91
# 5.19
# 5.67
# 5.78
# 5.52
# 6.01
# 6.51
# 6.47
# 6.69
# 7.41
# 8.14
# 8.40
