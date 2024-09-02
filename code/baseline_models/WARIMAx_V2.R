########################### WARIMAx Model for long-term forecasting of CPI Inflation #####################
# This is an example code for the WARIMax model. The same code module can be replicated for other geographies and other forecast horizons
# Brazil
setwd("/FEWNet/dataset/brazil")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

# Import Relevant Libraries
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(forecast)
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)


# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
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
  # cpi.test.df$CPI_inflation_Rate_l1,
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
xMat.test.new[1:24,1:6]

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
cpi.df.test.ts

################### Code for the WARIMAX in R: Arima() #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='haar', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingarima<- function(ts,Waveletlevels,boundary,FastFlag,NForecast)

{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,bndry=boundary,FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL

  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletARIMAXFit <- forecast::Arima(y=as.ts(ts),
                                       order = c(6,1,3),
                                       seasonal = c(1, 1, 1),
                                       xreg = xMat.train.new[,1:6],
    )
    WaveletARIMAXPredict <- WaveletARIMAXFit$fitted
    WaveletARIMAXForecast <- forecast::forecast(WaveletARIMAXFit,
                                                xreg = xMat.train.new[1:24,1:6],
                                                h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletARIMAXPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletARIMAXForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

# Define con_tr and con_tst
# Training and Test dataset
con_tr = cpi.train.df$CPI_inflation_rate
con_tst = cpi.test.df$CPI_inflation_rate


set.seed(421)
fit_warimax = WaveletFittingarima(ts(con_tr),
                                  Waveletlevels = floor(log(length(con_tr))),
                                  boundary = "periodic",
                                  FastFlag = TRUE,
                                  NForecast = 24)

fore_warimax = as.data.frame(fit_warimax$Finalforecast, h = 24)

# Print the predictions
fore_warimax$`fit_warimax$Finalforecast`

# Print the Actual
con_tst

############################## End of Code #######################################
