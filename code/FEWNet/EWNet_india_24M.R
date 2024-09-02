####################### EWNet with only EPU and GPRC as exog ##########################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/india")
getwd()

# Train Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:203,1:4]
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[204:227,1:4]
str(cpi.test.df)


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
  cpi.train.df$gprc_ind
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
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

####################### Proposed EWNet ##########################
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
