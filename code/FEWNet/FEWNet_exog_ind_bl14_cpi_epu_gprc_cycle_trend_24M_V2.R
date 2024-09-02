### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)
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
library(wavelets)

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
                                      # xreg = xreg_tr[,1:6],
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
                               MaxARParam = 12, 
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1]  6.884919  6.219972  6.449505  6.427833  8.015101  8.398981  7.829873  7.044319  6.707944  6.409492  7.462596
# [12]  7.906954  7.403285  6.363855  6.001451  7.406319  8.118303  8.869290  9.706772  9.520743  9.977713 11.586560
# [23] 12.414882 12.917521
# > con_tst
# [1] 9.634551 7.491857 6.840391 5.501618 5.448718 5.095541 5.063291 5.329154 5.625000 5.636574 5.902162 5.284787 3.686636
# [14] 3.162966 4.494493 5.665658 5.139860 5.257646 5.577841 5.263880 4.811442 4.403048 4.518828 4.837364

############# End of Code #################
