############################ Global Characteristics: CPI Inflation, log(EPU) and GPRC: BRIC ######################

# install.packages('fpp2')

# Important Libraries
library(nonlinearTseries)
library(tseries)
library(forecast)
library(Metrics)
library(ggplot2)
library(readr)
library(WaveletArima)
library(caret)
library(nnfor)
library(tsDyn)
library(fracdiff)
library(bsts)
library(forecastHybrid)
library(e1071)
library(tseriesChaos)
library(pracma)
library(Kendall)
library(GeneCycle)
library(fpp2)
library(seastests)
library(entropy)

################## Brazil ############################
setwd("/dataset/brazil")
getwd()

# Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# Check for Skewness
skewness(cpi.df$CPI_inflation_rate)
skewness(cpi.df$log_epu)
skewness(cpi.df$gprc_bra)

# Check for Kurtosis
kurtosis(cpi.df$CPI_inflation_rate)
kurtosis(cpi.df$log_epu)
kurtosis(cpi.df$gprc_bra)

# Test for Seasonality
isSeasonal(ts(cpi.df$CPI_inflation_rate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$log_epu), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$gprc_bra), test = "combined", freq = 12)

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$CPI_inflation_rate), verbose = TRUE) # Non -linear
nonlinearityTest(ts(cpi.df$log_epu), verbose = TRUE) # Non -linear
nonlinearityTest(ts(cpi.df$gprc_bra), verbose = TRUE) # Non -linear


# Check for Long-range dependence
hurstexp(cpi.df$CPI_inflation_rate)
hurstexp(cpi.df$log_epu)
hurstexp(cpi.df$gprc_bra)

#Test for Stationarity 
kpss.test(cpi.df$CPI_inflation_rate) #Non-stationary
kpss.test(cpi.df$log_epu) #Non-stationary
kpss.test(cpi.df$gprc_bra) #Stationary

################## Russia ############################
setwd("/dataset/russia")
getwd()

# Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# Check for Skewness
skewness(cpi.df$cpi_inflation_rate)
skewness(cpi.df$log_epu)
skewness(cpi.df$gprc_rus)

# Check for Kurtosis
kurtosis(cpi.df$cpi_inflation_rate)
kurtosis(cpi.df$log_epu)
kurtosis(cpi.df$gprc_rus)

# Test for Seasonality
isSeasonal(ts(cpi.df$cpi_inflation_rate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$log_epu), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$gprc_rus), test = "combined", freq = 12)

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$cpi_inflation_rate), verbose = TRUE) # Linear
nonlinearityTest(ts(cpi.df$log_epu), verbose = TRUE) # Non -linear
nonlinearityTest(ts(cpi.df$gprc_rus), verbose = TRUE) # Linear


# Check for Long-range dependence
hurstexp(cpi.df$cpi_inflation_rate)
hurstexp(cpi.df$log_epu)
hurstexp(cpi.df$gprc_rus)

#Test for Stationarity 
kpss.test(cpi.df$cpi_inflation_rate) #Non-stationary
kpss.test(cpi.df$log_epu) #Non-stationary
kpss.test(cpi.df$gprc_rus) #Non-stationary

################## India ############################
setwd("/dataset/india")
getwd()

# Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# Check for Skewness
skewness(cpi.df$CPI_inflation_Rate)
skewness(cpi.df$log_epu)
skewness(cpi.df$gprc_ind)

# Check for Kurtosis
kurtosis(cpi.df$CPI_inflation_Rate)
kurtosis(cpi.df$log_epu)
kurtosis(cpi.df$gprc_ind)

# Test for Seasonality
isSeasonal(ts(cpi.df$CPI_inflation_Rate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$log_epu), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$gprc_ind), test = "combined", freq = 12)

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$CPI_inflation_Rate), verbose = TRUE) # Linear
nonlinearityTest(ts(cpi.df$log_epu), verbose = TRUE) # Linear
nonlinearityTest(ts(cpi.df$gprc_ind), verbose = TRUE) # Non - Linear


# Check for Long-range dependence
hurstexp(cpi.df$CPI_inflation_Rate)
hurstexp(cpi.df$log_epu)
hurstexp(cpi.df$gprc_ind)

#Test for Stationarity 
kpss.test(cpi.df$CPI_inflation_Rate) #Non-stationary
kpss.test(cpi.df$log_epu) #Non-stationary
kpss.test(cpi.df$gprc_ind) #Non-stationary

################## China ############################
setwd("/dataset/india")
getwd()

# Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# Check for Skewness
skewness(cpi.df$cpi_inflation_rate)
skewness(cpi.df$log_scmp_epu)
skewness(cpi.df$gprc_chn)

# Check for Kurtosis
kurtosis(cpi.df$cpi_inflation_rate)
kurtosis(cpi.df$log_scmp_epu)
kurtosis(cpi.df$gprc_chn)

# Test for Seasonality
isSeasonal(ts(cpi.df$cpi_inflation_rate), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$log_scmp_epu), test = "combined", freq = 12)
isSeasonal(ts(cpi.df$gprc_chn), test = "combined", freq = 12)

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$cpi_inflation_rate), verbose = TRUE) # Non - Linear (based on Keenan's test)
nonlinearityTest(ts(cpi.df$log_scmp_epu), verbose = TRUE) # Non - Linear (based on Tsay's test)
nonlinearityTest(ts(cpi.df$gprc_chn), verbose = TRUE) # Non - Linear (based on Tsay's test)


# Check for Long-range dependence
hurstexp(cpi.df$cpi_inflation_rate)
hurstexp(cpi.df$log_scmp_epu)
hurstexp(cpi.df$gprc_chn)

#Test for Stationarity 
kpss.test(cpi.df$cpi_inflation_rate) #Stationary
kpss.test(cpi.df$log_scmp_epu) #Non-stationary
kpss.test(cpi.df$gprc_chn) #Non-stationary

############################## End of Code #############################
