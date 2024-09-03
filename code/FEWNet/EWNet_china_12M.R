####################### EWNet with only EPU and GPRC as exog ##########################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Train Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
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
                                             xreg = as.data.frame(xMat.train.new[1:12,1:2]),
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

xreg_tr[1:6]
xreg_tr
####################### Proposed WARNNX ##########################
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

# p=6
# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME      RMSE       MAE      MPE     MAPE
# Test set 0.3774741 0.8642327 0.7471725 21.86552 122.3565
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 105.5601
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 1.125361
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] -0.50143139 -0.63268371 -0.58241045 -0.41891694 -0.41059328  0.03533347  0.59528466  0.94933986  1.35228412
# [10]  1.57567203  1.82603519  2.36255021
# > con_tst
# [1]  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494  1.2401969  1.9125597  0.6415800  0.4613803
# [11]  1.4328405  2.4772978
