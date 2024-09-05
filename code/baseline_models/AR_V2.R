########################### AR Models for BRIC nations: 12M & 24M #####################

################### Brazil:12M ###################
library(stats)

setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_12M_R.csv",header=TRUE)


# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_12M_R.csv",header=TRUE)


# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
cpi.df.test.ts

# Generate 12M Forward Forecasts using AR model
ar_12m_brazil <- ar.ols(cpi.df.train.ts,
                        order.max = 3,
                        demean = TRUE,
                        intercept = TRUE)

ar_12m_brazil

# Generate Forecasts: 12M
forecasts_12M <- predict(ar_12m_brazil,
                         n.ahead = 12)
forecasts_12M$pred

################### Russia: 12M ###################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_12M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_12M_R.csv",header=TRUE)

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts


# Generate 12M Forward Forecasts using AR model
ar_12m_russia <- ar.ols(cpi.df.train.ts,
                        order.max = 3,
                        demean = TRUE,
                        intercept = TRUE)

ar_12m_russia

# Generate Forecasts: 12M
forecasts_12M <- predict(ar_12m_russia,
                         n.ahead = 12)
forecasts_12M$pred
################### India: 12M ###################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_12M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_12M_R.csv",header=TRUE)


# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
cpi.df.test.ts


# Generate 12M Forward Forecasts using AR model
ar_12m_india <- ar.ols(cpi.df.train.ts,
                       order.max = 3,
                       demean = TRUE,
                       intercept = TRUE)

ar_12m_india

# Generate Forecasts: 24M
forecasts_12M <- predict(ar_12m_india,
                         n.ahead = 12)
forecasts_12M$pred
################### China: 12M ###################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_12M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_12M_R.csv",header=TRUE)

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts


# Generate 12M Forward Forecasts using AR model
ar_12m_china <- ar.ols(cpi.df.train.ts,
                       order.max = 3,
                       demean = TRUE,
                       intercept = TRUE)

ar_12m_china

# Generate Forecasts: 24M
forecasts_12M <- predict(ar_12m_china,
                         n.ahead = 12)
forecasts_12M$pred

##################### End of Code: 12M FH ########################

############### AR Model for long-term (24M) forecasting of CPI Inflation ##############
################### Brazil: 24M ###################
library(stats)

setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_rate)
cpi.df.test.ts

# Generate 24M Forward Forecasts using AR model
ar_24m_brazil <- ar.ols(cpi.df.train.ts,
                        order.max = 3,
                        demean = TRUE,
                        intercept = TRUE)
ar_24m_brazil

forecasts_24M <- predict(ar_24m_brazil,
                         n.ahead = 24)
forecasts_24M$pred

################### Russia: 24M ###################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_24M_R.csv",header=TRUE)


# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts


# Generate 24M Forward Forecasts using AR model
ar_24m_russia <- ar.ols(cpi.df.train.ts,
                        order.max = 3,
                        demean = TRUE,
                        intercept = TRUE)

ar_24m_russia


# Generate Forecasts: 24M
forecasts_24M <- predict(ar_24m_russia,
                         n.ahead = 24)
forecasts_24M$pred

################### India: 24M ###################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$CPI_inflation_Rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$CPI_inflation_Rate)
cpi.df.test.ts


# Generate 24M Forward Forecasts using AR model
ar_24m_india <- ar.ols(cpi.df.train.ts,
                       order.max = 3,
                       demean = TRUE,
                       intercept = TRUE)

ar_24m_india

# Generate Forecasts: 24M
forecasts_24M <- predict(ar_24m_india,
                         n.ahead = 24)
forecasts_24M$pred

################### China: 24M ###################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)

# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)

# Convert the series into timeseries object
cpi.df.train.ts <- ts(cpi.train.df$cpi_inflation_rate)
cpi.df.train.ts

# Convert the series into timeseries object
cpi.df.test.ts <- ts(cpi.test.df$cpi_inflation_rate)
cpi.df.test.ts


# Generate 24M Forward Forecasts using AR model
ar_24m_china <- ar.ols(cpi.df.train.ts,
                       order.max = 3,
                       demean = TRUE,
                       intercept = TRUE)

ar_24m_china

# Generate Forecasts: 24M
forecasts_24M <- predict(ar_24m_china,
                         n.ahead = 24)
forecasts_24M$pred
##################### End of Code: 24M FH ########################
