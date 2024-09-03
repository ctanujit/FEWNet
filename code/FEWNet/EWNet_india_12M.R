####################### EWNet with only EPU and GPRC as exog ##########################
### Explore EWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/india")
getwd()

# Train Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:215,1:4]
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[216:227,1:4]
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

################### Code for the EWNet in R #######################
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


xreg_tr[1:6]
xreg_tr
####################### Proposed WARNNX ##########################
#New Method
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
#               ME     RMSE      MAE       MPE     MAPE
# Test set -1.702731 1.765919 1.702731 -38.21926 38.21926
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 31.31708
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 3.345097
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 5.351758 5.768072 5.979629 6.455805 6.621372 6.695674 6.890118 6.712164 6.674327 6.345495 6.623340 7.134678
# > con_tst
# [1] 3.686636 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048 4.518828 4.837364
