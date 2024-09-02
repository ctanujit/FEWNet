### Explore ARNN model with exogenous factors #####
# Read the base Table
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/china")
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)
# View(head(cpi.train.df))
# View(tail(cpi.train.df))
str(cpi.train.df)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)
# View(head(cpi.test.df))
# View(tail(cpi.test.df))
str(cpi.test.df)

# install.packages('wavelets')
# Convert the Date
library(lubridate)
# Use Series cpi, EPU and GPR for this exercise
library(dLagM)
library(tictoc)
library(lmtest)
library(tseries)
library(forecast)
library(pracma)
library(wavelets)
# library(egcm)
help(wt.filter)

# Train Data
# CPI_inflation_rate.train.ts <- ts(cpi.train.df$CPI_inflation_rate, start=c(2004,1), end = c(2020,11), frequency=12)
# CPI_inflation_rate_l1.train.ts <- ts(cpi.train.df$CPI_inflation_rate_l1, start=c(2004,1), end = c(2020,11), frequency=12)
# epu_cycle_cf_l1.train.ts <- ts(cpi.train.df$epu_cycle_cf_l1, start=c(2004,1), end = c(2020,11), frequency=12)
# epu_trend_hp_l1.train.ts <- ts(cpi.train.df$epu_trend_hp_l1, start=c(2004,1), end = c(2020,11), frequency=12)
# gprc_ind_cycle_cf_l1.train.ts <- ts(cpi.train.df$gprc_ind_cycle_cf_l1, start=c(2004,1), end = c(2020,11), frequency=12)
# gprc_ind_trend_hp_l1.train.ts <- ts(cpi.train.df$gprc_ind_trend_hp_l1, start=c(2004,1), end = c(2020,11), frequency=12)
# cpi_cycle_cf_l1.train.ts <- ts(cpi.train.df$cpi_cycle_cf_l1, start=c(2004,1), end = c(2020,11), frequency=12)
# cpi_trend_hp_l1.train.ts <- ts(cpi.train.df$cpi_trend_hp_l1, start=c(2004,1), end = c(2020,11), frequency=12)
#
# CPI_inflation_rate.train.ts
# CPI_inflation_rate_l1.train.ts
# epu_cycle_cf_l1.train.ts
# epu_trend_hp_l1.train.ts
# gprc_ind_cycle_cf_l1.train.ts
# gprc_ind_trend_hp_l1.train.ts
# cpi_cycle_cf_l1.train.ts
# cpi_trend_hp_l1.train.ts

# Test Data
# CPI_inflation_rate.test.ts <- ts(cpi.test.df$CPI_inflation_rate, start=c(2020,12), end = c(2021,11), frequency=12)
# CPI_inflation_rate_l1.test.ts <- ts(cpi.test.df$CPI_inflation_rate_l1, start=c(2020,12), end = c(2021,11), frequency=12)
# epu_cycle_cf_l1.test.ts <- ts(cpi.test.df$epu_cycle_cf_l1, start=c(2020,12), end = c(2021,11), frequency=12)
# epu_trend_hp_l1.test.ts <- ts(cpi.test.df$epu_trend_hp_l1, start=c(2020,12), end = c(2021,11), frequency=12)
# gprc_ind_cycle_cf_l1.test.ts <- ts(cpi.test.df$gprc_ind_cycle_cf_l1, start=c(2020,12), end = c(2021,11), frequency=12)
# gprc_ind_trend_hp_l1.test.ts <- ts(cpi.test.df$gprc_ind_trend_hp_l1, start=c(2020,12), end = c(2021,11), frequency=12)
# cpi_cycle_cf_l1.test.ts <- ts(cpi.test.df$cpi_cycle_cf_l1, start=c(2020,12), end = c(2021,11), frequency=12)
# cpi_trend_hp_l1.test.ts <- ts(cpi.test.df$cpi_trend_hp_l1, start=c(2020,12), end = c(2021,11), frequency=12)
#
# CPI_inflation_rate.test.ts
# CPI_inflation_rate_l1.test.ts
# epu_cycle_cf_l1.test.ts
# epu_trend_hp_l1.test.ts
# gprc_ind_cycle_cf_l1.test.ts
# gprc_ind_trend_hp_l1.test.ts
# cpi_cycle_cf_l1.test.ts
# cpi_trend_hp_l1.test.ts

str(cpi.train.df)
str(cpi.test.df)

# create a matrix of external regressors
xMat.train <- data.matrix(cbind(cpi.train.df$cpi_inflation_rate,
                                cpi.train.df$epu_cycle_cf_l1,
                                cpi.train.df$epu_trend_hp_l1,
                                cpi.train.df$gprc_chn_cycle_cf_l1,
                                cpi.train.df$gprc_chn_trend_hp_l1,
                                cpi.train.df$cpi_cycle_cf_l1,
                                cpi.train.df$cpi_trend_hp_l1))
xMat.train

xMat.test <- data.matrix(cbind(cpi.test.df$cpi_inflation_rate,
                               cpi.test.df$epu_cycle_cf_l1,
                               cpi.test.df$epu_trend_hp_l1,
                               cpi.test.df$gprc_chn_cycle_cf_l1,
                               cpi.test.df$gprc_chn_trend_hp_l1,
                               cpi.test.df$cpi_cycle_cf_l1,
                               cpi.test.df$cpi_trend_hp_l1))
xMat.test

str(cpi.train.df)
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

# install.packages("arfima")
#fitting ARNN model

# install.packages("nnet")
# install.packages("forecast")

library(nnet)
library(forecast)

# Link : Covid Paper: https://arxiv.org/pdf/2010.05079.pdf
# Link: https://cran.r-project.org/web/packages/forecast/index.html
# ARFIMA documentation: https://cran.r-project.org/web/packages/arfima/arfima.pdf
#fitting ARFIMA model
# ?arfima

# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_chn_cycle_cf_l1,
  cpi.train.df$gprc_chn_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1
),
ncol=6)
xMat.train.new

xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_chn_cycle_cf_l1,
  cpi.test.df$gprc_chn_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1
),
ncol=6)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:6]

# Image result for Slicing matrix data structure in R
# Slice a Matrix
# matrix_c[1,2] selects the element at the first row and second column.
# matrix_c[1:3,2:3] results in a R slice matrix with the data on the rows 1, 2, 3 and columns 2, 3,
# matrix_c[,1] selects all elements of the first column.
# matrix_c[1,] selects all elements of the first row.
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
library(wavelets)

# Thus, to obtain the Daubechies wavelet transform filter of length 4, the character string "d4" can be passed to wt.filter.
################### Code for the WARNNX in R #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts, filter='c6', n.levels=Wvlevels,boundary=bndry, fast=FFlag)
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
                                             # xreg = xreg_tr[,1:6],
                                             # Should this be set against the test dataset
                                             # xreg = xreg_tst[,1:6],
                                             # xreg = xreg_tst[,1:6],
                                             xreg = as.data.frame(xMat.train.new[1:24,1:6]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

# View(head(cpi.train.df))
# View(head(cpi.test.df))
str(cpi.train.df)
str(cpi.test.df)


# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$epu_cycle_cf_l1,
  cpi.train.df$epu_trend_hp_l1,
  cpi.train.df$gprc_chn_cycle_cf_l1,
  cpi.train.df$gprc_chn_trend_hp_l1,
  cpi.train.df$cpi_cycle_cf_l1,
  cpi.train.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tr) <- c('epu_cycle_cf_l1',
                       'epu_trend_hp_l1',
                       'gprc_chn_cycle_cf_l1',
                       'gprc_chn_trend_hp_l1',
                       'cpi_cycle_cf_l1',
                       'cpi_trend_hp_l1')
xreg_tst = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.test.df$epu_cycle_cf_l1,
  cpi.test.df$epu_trend_hp_l1,
  cpi.test.df$gprc_chn_cycle_cf_l1,
  cpi.test.df$gprc_chn_trend_hp_l1,
  cpi.test.df$cpi_cycle_cf_l1,
  cpi.test.df$cpi_trend_hp_l1)
# assigning new names to the columns of the data frame
colnames(xreg_tst) <- c('epu_cycle_cf_l1',
                        'epu_trend_hp_l1',
                        'gprc_chn_cycle_cf_l1',
                        'gprc_chn_trend_hp_l1',
                        'cpi_cycle_cf_l1',
                        'cpi_trend_hp_l1')

con_tst = cpi.test.df$cpi_inflation_rate

head(xreg_tr)
head(xreg_tst)
# View(tail(xreg_tst))

xreg_tr[1:6]
xreg_tr

floor(log(length(con_tr)))
length(con_tr)
ts(con_tr)
####################### Proposed WARNNX ##########################
# source("warnnx.R")
set.seed(42)
fit_warnnx = WaveletFittingnar(ts(con_tr),
                               Waveletlevels = floor(log(length(con_tr))),
                               boundary = "periodic",
                               FastFlag = TRUE,
                               MaxARParam = 12,#best=12
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fore_warnnx$`fit_warnnx$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -1.596573 2.216785 1.903501 -61.92783 333.5625
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 87.1936
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 2.677655
# > fore_warnnx$`fit_warnnx$Finalforecast`
# [1] 4.025540 4.099916 3.873747 3.602871 3.843285 2.626910 2.727028 2.729557 2.908772 3.286048 3.088543 3.208644 3.031024
# [14] 3.027614 3.407506 3.898016 3.079884 3.605120 3.186830 3.169688 3.298681 3.430237 4.451465 4.809219
# > con_tst
# [1]  4.4131455  5.4205607  5.1803885  4.2711235  3.2467532  2.4118738  2.5069638  1.7576318  2.3875115  1.7304189
# [11]  0.5415162 -0.4496403  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494  1.2401969  1.9125597
# [21]  0.6415800  0.4613803  1.4328405  2.4772978

# Calculation of residuals
resid <- abs(fore_warnnx$`fit_warnnx$Finalforecast` - con_tst)
resid

# resid <- residuals(fore_warnnx$`fit_warnnx$Finalforecast`,con_tst)

checkresiduals(resid)
Box.test(resid,lag=12, fitdf=0, type="Lj")

############## A distribution - free measurement (conformal) of uncertainties for Forecasting ######
# Document Link: https://cran.r-project.org/web/packages/caretForecast/caretForecast.pdf
# packageVersion("methods")
# sessionInfo()

library(caretForecast)
forecast_waveletnn <- fore_warnnx$`fit_warnnx$Finalforecast`
forecast_waveletnn
# Calculation of residuals
resid_chn <- abs(forecast_waveletnn - con_tst)
resid_chn
conf_waveletnn <- conformalRegressor(resid_chn, sigmas = NULL)
conf_waveletnn
## S3 method for class 'conformalRegressor'
conf_pred <- predict(
  conf_waveletnn,
  y_hat = forecast_waveletnn,
  sigmas = NULL,
  confidence = 0.95,
  y_min = -Inf,
  y_max = Inf
)
conf_pred

help(predict.conformalRegressor)

#### Visualisation of the Conformal Prdictions ###########
# Use GGplot2 to make the line charts
# install.packages('gcookbook')
# install.packages('ggplot2')
install.packages('ggplot')
# library(gcookbook) # Load gcookbook for the climate data set
# library(dplyr)
# library(ggplot2)
# 
# # Grab a subset of the climate data
# climate_mod <- climate %>%
#   filter(Source == "Berkeley") %>%
#   select(Year, Anomaly10y, Unc10y)
# 
# climate_mod
# str(climate_mod)
# 
# # Shaded region
# ggplot(climate_mod, aes(x = Year, y = Anomaly10y))
#   geom_ribbon(aes(ymin = Anomaly10y - Unc10y, ymax = Anomaly10y + Unc10y), alpha = 0.2) +
#   geom_line()

# Get the forecast and PIs and Actual Data elements
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/CPI_BRIC_24M")
getwd()

df_pi_chn_24 <- read.csv("china_CPI_24M.csv",header=TRUE)
str(df_pi_chn_24)
head(df_pi_chn_24)
df_pi_chn_24


library(magrittr)
library(dplyr)
library(ggplot2)

df_pi_chn_24 <- data.frame(df_pi_chn_24)
str(df_pi_chn_24)
# install.packages('plotrix')
# install.packages('ggthemes')
library(plotrix)
library(ggthemes)

plotCI(x = df_pi_chn_24$mon_index,
       y = df_pi_chn_24$Forecasts,
       li = df_pi_chn_24$lower_95,
       ui = df_pi_chn_24$Upper_95)

library(ggplot2)
library(ggthemes)
ggplot(df_pi_chn_24, aes(mon_index, Forecasts,color = Forecasts))+
  xlab("Month Index: 24Months") +
  ylab("Forecasts - China & conformalized PI") +
  # ylim(-1, 10) +
  geom_point(size = 1.5)+
  # theme_few()  + 
  geom_errorbar(aes(ymin = lower_95, ymax = Upper_95),alpha = 0.5) +
  # geom_ribbon(aes(ymin = lower_95, ymax = Upper_95))
  # geom_ribbon(aes(ymin = lower_95, ymax = Upper_95), alpha = 0.2) +
  # theme_tufte()
  geom_line() + 
  # scale_x_date(name = "Month Index - 24M",
  #              date_labels = "%m-%d") +
  # geom_line(aes(y = Actual), color = "green50") +
  theme_linedraw()
# geom_area(fill = "black") +
# theme_excel()
# theme_solarized_2()
# geom_smooth()

# Link: https://rdpeng.github.io/RProgDA/customizing-ggplot2-plots.html

####################### WARNN(X) with only EPU and GPRC as exog ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/china")
getwd()

# Train Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
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
# cpi.train.df$date <- dmy(cpi.train.df$date)
# head(cpi.train.df)
# tail(cpi.train.df)

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

# install.packages("arfima")
#fitting ARNN model

# install.packages("nnet")
# install.packages("forecast")



# Link : Covid Paper: https://arxiv.org/pdf/2010.05079.pdf
# Link: https://cran.r-project.org/web/packages/forecast/index.html
# ARFIMA documentation: https://cran.r-project.org/web/packages/arfima/arfima.pdf
#fitting ARFIMA model
# ?arfima
str(cpi.train.df)
str(cpi.test.df)
# create a matrix of external regressors
xMat.train.new <- matrix(cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
  cpi.train.df$log_scmp_epu,
  cpi.train.df$gprc_chn
),
ncol=2)
xMat.train.new

xMat.test.new <- matrix(cbind(
  # cpi.test.df$CPI_inflation_Rate_l1,
  cpi.test.df$log_scmp_epu,
  cpi.test.df$gprc_chn
),
ncol=2)
xMat.test.new
# print the first 6 rows of 6 columns of the matirix
xMat.test.new[1:6,1:2]

# Image result for Slicing matrix data structure in R
# Slice a Matrix
# matrix_c[1,2] selects the element at the first row and second column.
# matrix_c[1:3,2:3] results in a R slice matrix with the data on the rows 1, 2, 3 and columns 2, 3,
# matrix_c[,1] selects all elements of the first column.
# matrix_c[1,] selects all elements of the first row.
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
                                             xreg = as.data.frame(xMat.train.new[1:24,1:2]),
                                             h=NForecast)
    AllWaveletPrediction <- cbind(AllWaveletPrediction,WaveletNARPredict)
    AllWaveletForecast <- cbind(AllWaveletForecast,as.matrix(WaveletNARForecast$mean))
  }
  Finalforecast <- rowSums(AllWaveletForecast,na.rm = T)
  FinalPrediction <- rowSums(AllWaveletPrediction,na.rm = T)
  return(list(Finalforecast=Finalforecast,FinalPrediction=FinalPrediction))
}

# View(head(cpi.train.df))
# View(head(cpi.test.df))
# str(cpi.train.df)
# str(cpi.test.df)

# floor(log(length(con_tr)))
# length(con_tr)
# ts(con_tr)

# Training and Test dataset
con_tr = cpi.train.df$cpi_inflation_rate
xreg_tr = cbind(
  # cpi.train.df$CPI_inflation_Rate_l1,
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
                               MaxARParam = 6,#best= 6
                               NForecast = 24)
fore_warnnx = as.data.frame(fit_warnnx$Finalforecast, h = 24)
forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
fit_warnnx$Finalforecast
con_tst

# > forecast::accuracy(fore_warnnx$`fit_warnnx$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -3.292398 3.761027 3.333715 -125.2181 560.6531
# > smape(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)*100
# [1] 106.3652
# > mase(con_tst, fore_warnnx$`fit_warnnx$Finalforecast`)
# [1] 4.689536
# > fit_warnnx$Finalforecast
[1] 4.598191 4.998653 5.106492 5.158560 5.005111 5.275120 5.405772 5.551540 5.832547 5.848286 5.775940
[12] 5.739612 5.552026 5.298053 5.280936 5.297991 5.245335 5.264415 4.930678 4.676213 4.615073 4.331261
[23] 4.203108 4.125040
# > con_tst
# [1]  4.4131455  5.4205607  5.1803885  4.2711235  3.2467532  2.4118738  2.5069638  1.7576318  2.3875115
# [10]  1.7304189  0.5415162 -0.4496403  0.2697842 -0.2216312 -0.4227999  0.3219466  0.9226485  1.6443494
# [19]  1.2401969  1.9125597  0.6415800  0.4613803  1.4328405  2.4772978

####################### WARNN - Univariate forecasting ##########################
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/base_data/china")
getwd()

# Train Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_univariate_202201.csv",header=TRUE)
str(cpi.df)
# lets remove the first 12 observations to make the distribution comparable
cpi.train.df<-cpi.df[13:203,1:2]
str(cpi.train.df)
# Test Data
cpi.test.df<-cpi.df[224:227,1:2]
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
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

str(cpi.test.df)
cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts

##################### LIBRARY ###########################################################
library(tseries)
library(forecast)
library(Metrics)
library(nonlinearTseries)
library(lmtest)

################### Code for the WARNN in R #######################
WaveletFitting <- function(ts,Wvlevels,bndry,FFlag)
{
  mraout <- wavelets::modwt(ts,
                            filter='haar',
                            n.levels=Wvlevels,boundary=bndry,
                            fast=FFlag)
  WaveletSeries <- cbind(do.call(cbind,mraout@W),mraout@V[[Wvlevels]])
  return(list(WaveletSeries=WaveletSeries,WVSeries=mraout))
}

WaveletFittingnar<- function(ts,Waveletlevels,boundary,FastFlag,MaxARParam,NForecast)
  
{
  WS <- WaveletFitting(ts=ts,Wvlevels=Waveletlevels,
                       bndry=boundary,
                       FFlag=FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL;AllWaveletPrediction <- NULL
  
  for(WVLevel in 1:ncol(WS))
  {
    ts <- NULL
    ts <- WS[,WVLevel]
    WaveletNARFit <- forecast::nnetar(y=as.ts(ts), p=MaxARParam, repeats = 500)
    WaveletNARPredict <- WaveletNARFit$fitted
    WaveletNARForecast <- forecast::forecast(WaveletNARFit, h=NForecast)
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
con_tst = cpi.test.df$cpi_inflation_rate

####################### Proposed WARNN ###########################
set.seed(42)
# source("warnn.R")
fit_warnn = WaveletFittingnar(ts(con_tr),
                              Waveletlevels = floor(log(length(con_tr))),
                              boundary = "periodic",
                              FastFlag = TRUE,
                              MaxARParam = 3,#best=3
                              NForecast = 24)
fore_warnn = as.data.frame(fit_warnn$Finalforecast, h = 24)
forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
fore_warnn$`fit_warnn$Finalforecast`
con_tst

# > forecast::accuracy(fore_warnn$`fit_warnn$Finalforecast`, con_tst)
# ME     RMSE      MAE       MPE     MAPE
# Test set -3.143643 3.239022 3.143643 -431.0333 431.0333
# > smape(con_tst, fore_warnn$`fit_warnn$Finalforecast`)*100
# [1] 109.7313
# > mase(con_tst, fore_warnn$`fit_warnn$Finalforecast`)
# [1] 22.47558
# > fore_warnn$`fit_warnn$Finalforecast`
# [1] 4.405825 4.418098 4.284587 4.479160 4.525526 4.573004 4.463062 4.468371 4.483959 4.437601 4.402928
# [12] 4.399912 4.245719 4.124099 4.054446 3.983518 3.724007 3.573480 3.390521 3.299276 3.256038 3.037641
# [23] 2.968518 2.891302


