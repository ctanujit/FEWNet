####################### EWNet with only EPU and GPRC as exog ##########################
### Explore FEWNet model with exogenous factors #####
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Train Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:203,1:4]
str(cpi.train.df)

# Test Data
cpi.test.df<-cpi.df[204:227,1:4]
str(cpi.test.df)

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

head(xreg_tr)
head(xreg_tst)

xreg_tr[1:6]
xreg_tr
####################### Proposed EWNet ##########################
# source("warnnx.R")
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 6,#best= 6
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst


# > fit_warnnx$Finalforecast
# [1] 4.598191 4.998653 5.106492 5.158560 5.005111 5.275120 5.405772 5.551540 5.832547 5.848286 5.775940
# [12] 5.739612 5.552026 5.298053 5.280936 5.297991 5.245335 5.264415 4.930678 4.676213 4.615073 4.331261
# [23] 4.203108 4.125040
# > con_tst
# [1]  4.4131455  5.4205607  5.1803885  4.2711235  3.2467532  2.4118738  2.5069638  1.7576318  2.3875115
# [10]  1.7304189  0.5415162 -0.4496403  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494
# [19]  1.2401969  1.9125597  0.6415800  0.4613803  1.4328405  2.4772978
