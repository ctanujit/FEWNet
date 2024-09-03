####################### EWNet with only EPU and GPRC as exog ##########################
### Explore EWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()


# Train Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
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

str(cpi.train.df)
str(cpi.test.df)
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
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 36,
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# p=36
# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
#             ME     RMSE      MAE       MPE    MAPE
# Test set 0.3644345 2.653998 2.219053 -16.29048 45.9106
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 40.9394
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 4.520978
# > fit_warnnx$Finalforecast
# [1] 3.345976 3.383090 3.433757 3.152540 3.321302 3.872703 4.299716 4.683655 5.100893 5.307291 5.724420 6.084339 6.332038
# [14] 6.568987 6.998048 6.851960 6.140252 5.897747 5.893079 5.676773 5.265691 5.534906 5.499439 5.076032
# > con_tst
# [1]  4.306152  4.191771  4.005114  3.303158  2.399279  1.877727  2.132417  2.305625  2.438465  3.135329  3.918350
# [12]  4.311223  4.517457  4.559198  5.195379  6.099479  6.759116  8.056819  8.347072  8.994823  9.679774 10.246209
# [23] 10.672622 10.738501

###################### End of Code #################

