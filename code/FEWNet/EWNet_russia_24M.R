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
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[204:227,1:4]
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
                               MaxARParam = 36, #best=12
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# p=36
# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
#               ME     RMSE      MAE       MPE     MAPE
# Test set -0.9909671 2.900709 2.212395 -5.241187 41.52285
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 41.1864
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 6.713219
# > fit_warnnx$Finalforecast
# [1]  3.109650  2.405513  1.816166  1.370449  1.074331  1.023528  1.545094  1.873682  2.324248  2.742431  3.117838
# [12]  3.372045  3.876530  4.587081  5.614204  6.856384  8.330403  9.830083 10.828906 11.622464 12.335228 12.852676
# [23] 13.152772 13.504608
# > con_tst
# [1] 3.046190 2.423893 2.311721 2.546287 3.098436 3.026511 3.211885 3.366394 3.573624 3.666989 3.977587 4.423442 4.912471
# [14] 5.194607 5.666425 5.782023 5.519744 6.014352 6.511526 6.469069 6.692744 7.408086 8.135334 8.403766

##################### End of Code ######################
