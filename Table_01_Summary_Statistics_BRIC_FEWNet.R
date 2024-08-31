################### Table 01: Sumary Statistics - BRIC ###############
set.seed(20240101) # For reproducibility, we are using this seed value

# Load the necessary libraries
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
library(zoo)
library(sandwich)
library(strucchange)
library(tseries)

################## Brazil ############################
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
# Observations: Output for Obs. in Table 01
str(cpi.df) # Obs.: 227 for CPI Inflation, log(EPU) and GPRC Series

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
# Min. Values
sapply(cpi.df[-1], function(x) min(x)) # reported values are rounded off to 2-decimal points
# Max. Values
sapply(cpi.df[-1], function(x) max(x)) # reported values are rounded off to 2-decimal points
# Q1 (25%) and Q3 (75%) Values
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1)) # reported values are rounded off to 2-decimal points
# Mean Values
sapply(cpi.df[-1], function(x) mean(x)) # reported values are rounded off to 2-decimal points
#Median Values
sapply(cpi.df[-1], function(x) median(x)) # reported values are rounded off to 2-decimal points

# Calculating the Coefficient of Variations for the 3 series
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)

# Calculating the Entropy for the 3 series
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

################## Russia ############################
# Set the working directory
setwd("/FEWNet/dataset/russia")
getwd()

# Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
# Observations: Output for Obs. in Table 01
str(cpi.df)

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
# Min. Values
sapply(cpi.df[-1], function(x) min(x)) # reported values are rounded off to 2-decimal points
# Max. Values
sapply(cpi.df[-1], function(x) max(x)) # reported values are rounded off to 2-decimal points
# Q1 (25%) and Q3 (75%) Values
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1)) # reported values are rounded off to 2-decimal points
# Mean Values
sapply(cpi.df[-1], function(x) mean(x)) # reported values are rounded off to 2-decimal points
#Median Values
sapply(cpi.df[-1], function(x) median(x)) # reported values are rounded off to 2-decimal points

# Calculating the Coefficient of Variations for the 3 series
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)

# Calculating the Entropy for the 3 series
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

################## India ############################
# Set the working directory
setwd("/FEWNet/dataset/india")
getwd()

# Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
# Observations: Output for Obs. in Table 01
str(cpi.df)

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
# Min. Values
sapply(cpi.df[-1], function(x) min(x)) # reported values are rounded off to 2-decimal points
# Max. Values
sapply(cpi.df[-1], function(x) max(x)) # reported values are rounded off to 2-decimal points
# Q1 (25%) and Q3 (75%) Values
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1)) # reported values are rounded off to 2-decimal points
# Mean Values
sapply(cpi.df[-1], function(x) mean(x)) # reported values are rounded off to 2-decimal points
#Median Values
sapply(cpi.df[-1], function(x) median(x)) # reported values are rounded off to 2-decimal points

# Calculating the Coefficient of Variations for the 3 series
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)

# Calculating the Entropy for the 3 series
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

################## China ############################
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
# Observations: Output for Obs. in Table 01
str(cpi.df)

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
# Min. Values
sapply(cpi.df[-1], function(x) min(x)) # reported values are rounded off to 2-decimal points
# Max. Values
sapply(cpi.df[-1], function(x) max(x)) # reported values are rounded off to 2-decimal points
# Q1 (25%) and Q3 (75%) Values
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1)) # reported values are rounded off to 2-decimal points
# Mean Values
sapply(cpi.df[-1], function(x) mean(x)) # reported values are rounded off to 2-decimal points
#Median Values
sapply(cpi.df[-1], function(x) median(x)) # reported values are rounded off to 2-decimal points

# Calculating the Coefficient of Variations for the 3 series
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)

# Calculating the Entropy for the 3 series
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))
################## End Of Code ############################


