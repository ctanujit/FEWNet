################### Table 02: Global Characteristics - BRIC ###############
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
library(car)

################## Brazil ############################
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
# Check
str(cpi.df) # Obs.: 227 for CPI Inflation, log(EPU) and GPRC Series

# Check for Skewness
skewness(cpi.df$CPI_inflation_rate) # reported values are rounded off to 2-decimal points
skewness(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
skewness(cpi.df$gprc_bra) # reported values are rounded off to 2-decimal points

# Check for Kurtosis
kurtosis(cpi.df$CPI_inflation_rate) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$gprc_bra) # reported values are rounded off to 2-decimal points

# Test for Seasonality
isSeasonal(ts(cpi.df$CPI_inflation_rate), test = "combined", freq = 12) # Non-Seasonal
isSeasonal(ts(cpi.df$log_epu), test = "combined", freq = 12) # Non-Seasonal
isSeasonal(ts(cpi.df$gprc_bra), test = "combined", freq = 12) # Non-Seasonal

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$CPI_inflation_rate), verbose = TRUE) # Non -linear (Tsay's Test for nonlinearity)
nonlinearityTest(ts(cpi.df$log_epu), verbose = TRUE) # Non -linear (Tsay's Test for nonlinearity)
nonlinearityTest(ts(cpi.df$gprc_bra), verbose = TRUE) # Non -linear (Tsay's Test for nonlinearity)

# Check for Long-range dependence
hurstexp(cpi.df$CPI_inflation_rate) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:0.725967
hurstexp(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation: 0.7665957 
hurstexp(cpi.df$gprc_bra) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:0.6274421 

#Test for Stationarity 
kpss.test(cpi.df$CPI_inflation_rate) #Non-stationary
# KPSS Level = 0.52913, Truncation lag parameter = 4, p-value = 0.03511
kpss.test(cpi.df$log_epu) #Non-stationary
# KPSS Level = 2.1582, Truncation lag parameter = 4, p-value = 0.01
kpss.test(cpi.df$gprc_bra) #Stationary
# KPSS Level = 0.15529, Truncation lag parameter = 4, p-value = 0.1

########################## Statistical Test for Outlier Detection ########################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

CPI_brazil <- ts(cpi.df$CPI_inflation_rate, start = 2003, end = 2021, frequency = 12)
CPI_brazil
length(CPI_brazil)

log_epu_brazil <- ts(cpi.df$log_epu, start = 2003, end = 2021, frequency = 12)
log_epu_brazil
length(log_epu_brazil)

gprc_bra_brazil <- ts(cpi.df$gprc_bra, start = 2003, end = 2021, frequency = 12)
gprc_bra_brazil
length(gprc_bra_brazil)

# Create a time variable (monotonic sequence)
time <- c(1:217)

# CPI
test_out_cpi <- outlierTest(lm(CPI_brazil~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)
# No Studentized residuals with Bonferroni p < 0.05

# log(EPU)
test_out_epu <- outlierTest(lm(log_epu_brazil~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_epu)

# 1 Outlier detected
# rstudent unadjusted p-value Bonferroni p
# 94 -3.944132         0.00010855     0.023555

# GPRC
test_out_gprc <- outlierTest(lm(gprc_bra_brazil~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

# 4 Outliers detected
# rstudent unadjusted p-value Bonferroni p
# 89  5.732367         3.3413e-08   7.2507e-06
# 194 5.028396         1.0447e-06   2.2670e-04
# 193 3.998130         8.7894e-05   1.9073e-02
# 90  3.986227         9.2097e-05   1.9985e-02

################## Russia ############################
# Set the working directory
setwd("/FEWNet/dataset/russia")
getwd()

# Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
# Check
str(cpi.df)

# Check for Skewness
skewness(cpi.df$cpi_inflation_rate) # reported values are rounded off to 2-decimal points
skewness(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
skewness(cpi.df$gprc_rus) # reported values are rounded off to 2-decimal points

# Check for Kurtosis
kurtosis(cpi.df$cpi_inflation_rate) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$gprc_rus) # reported values are rounded off to 2-decimal points

# Test for Seasonality
isSeasonal(ts(cpi.df$cpi_inflation_rate), test = "combined", freq = 12) # Non-Seasonal
isSeasonal(ts(cpi.df$log_epu), test = "combined", freq = 12) # Non-Seasonal
isSeasonal(ts(cpi.df$gprc_rus), test = "combined", freq = 12) # Non-Seasonal

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$cpi_inflation_rate), verbose = TRUE) # Linear
nonlinearityTest(ts(cpi.df$log_epu), verbose = TRUE) # Non -linear (Tsay's Test for nonlinearity)
nonlinearityTest(ts(cpi.df$gprc_rus), verbose = TRUE) # Linear

# Check for Long-range dependence
hurstexp(cpi.df$cpi_inflation_rate) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.7828272
hurstexp(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.7939474 
hurstexp(cpi.df$gprc_rus) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.7847786

#Test for Stationarity 
kpss.test(cpi.df$cpi_inflation_rate) #Non-stationary
# KPSS Level = 2.1281, Truncation lag parameter = 4, p-value = 0.01
kpss.test(cpi.df$log_epu) #Non-stationary
# KPSS Level = 3.4507, Truncation lag parameter = 4, p-value = 0.01
kpss.test(cpi.df$gprc_rus) #Non-stationary
# KPSS Level = 1.4805, Truncation lag parameter = 4, p-value = 0.01

########################## Statistical Test for Outlier Detection ########################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Create a time variable (monotonic sequence)
time <- c(1:217)

# CPI
CPI_russia <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 12)
CPI_russia
length(CPI_russia)

# log(EPU)
log_epu_russia <- ts(cpi.df$log_epu, start = 2003, end = 2021, frequency = 12)
log_epu_russia
length(log_epu_russia)

# GPRC
gprc_russia <- ts(cpi.df$gprc_rus, start = 2003, end = 2021, frequency = 12)
gprc_russia
length(gprc_russia)

# Oulier Test: CPI
test_out_cpi <- outlierTest(lm(CPI_russia~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)
# No Studentized residuals with Bonferroni p < 0.05

# Oulier Test: log(EPU)
test_out_log_epu <- outlierTest(lm(log_epu_russia~time, 
                                   cutoff=0.05, 
                                   n.max=10, 
                                   order=TRUE, 
                                   labels=names(rstudent)))
print(test_out_log_epu)
# No Studentized residuals with Bonferroni p < 0.05

# Oulier Test: GPRC
test_out_gprc <- outlierTest(lm(gprc_russia~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

# 2 Outliers detected
# rstudent unadjusted p-value Bonferroni p
# 135 5.240300         3.8328e-07   8.3172e-05
# 3   4.013253         8.2817e-05   1.7971e-02
################## India ############################
# Set the working directory
setwd("/FEWNet/dataset/india")
getwd()

# Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
# # Check
str(cpi.df)

# Check for Skewness
skewness(cpi.df$CPI_inflation_Rate) # reported values are rounded off to 2-decimal points
skewness(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
skewness(cpi.df$gprc_ind) # reported values are rounded off to 2-decimal points

# Check for Kurtosis
kurtosis(cpi.df$CPI_inflation_Rate) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$gprc_ind) # reported values are rounded off to 2-decimal points

# Test for Seasonality
isSeasonal(ts(cpi.df$CPI_inflation_Rate), test = "combined", freq = 12) # Non - Sesonal
isSeasonal(ts(cpi.df$log_epu), test = "combined", freq = 12) # Non - Sesonal
isSeasonal(ts(cpi.df$gprc_ind), test = "combined", freq = 12) # Non - Sesonal

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$CPI_inflation_Rate), verbose = TRUE) # Linear
nonlinearityTest(ts(cpi.df$log_epu), verbose = TRUE) # Linear
nonlinearityTest(ts(cpi.df$gprc_ind), verbose = TRUE) # Non - Linear (Tsay's Test for nonlinearity)

# Check for Long-range dependence
hurstexp(cpi.df$CPI_inflation_Rate) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.818633
hurstexp(cpi.df$log_epu) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.797259 
hurstexp(cpi.df$gprc_ind) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.7253037 

#Test for Stationarity 
kpss.test(cpi.df$CPI_inflation_Rate) #Non-stationary
# KPSS Level = 0.71808, Truncation lag parameter = 4, p-value = 0.0119
kpss.test(cpi.df$log_epu) #Non-stationary
# KPSS Level = 0.62445, Truncation lag parameter = 4, p-value = 0.02041
kpss.test(cpi.df$gprc_ind) #Non-stationary
# KPSS Level = 1.1869, Truncation lag parameter = 4, p-value = 0.01

########################## Statistical Test for Outlier Detection ########################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$Date)
str(cpi.df)

# Create a time variable (monotonic sequence)
time <- c(1:217)

# Convert CPI inflation into a time series object
CPI_india <- ts(cpi.df$CPI_inflation_Rate, start = 2003, end = 2021, frequency = 12)
CPI_india
length(CPI_india)

# Convert log(EPU) into a time series object
log_epu_india <- ts(cpi.df$log_epu, start = 2003, end = 2021, frequency = 12)
log_epu_india
length(log_epu_india)

# Convert GPRC into a time series object
gprc_india <- ts(cpi.df$gprc_ind, start = 2003, end = 2021, frequency = 12)
gprc_india
length(gprc_india)

# Outlier Test using Bonferroni Outlier Test: CPI
test_out_cpi <- outlierTest(lm(CPI_india~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)
# No Studentized residuals with Bonferroni p < 0.05

# Outlier Test using Bonferroni Outlier Test: log(EPU)
test_out_log_epu <- outlierTest(lm(log_epu_india~time, 
                                   cutoff=0.05, 
                                   n.max=10, 
                                   order=TRUE, 
                                   labels=names(rstudent)))
print(test_out_log_epu)
# No Studentized residuals with Bonferroni p < 0.05

# Outlier Test using Bonferroni Outlier Test: GPRC
test_out_gprc <- outlierTest(lm(gprc_india~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

# One outlier detected
# rstudent unadjusted p-value Bonferroni p
# 72 9.768904          7.098e-19   1.5403e-16
################## China ############################
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
# Check
str(cpi.df)

# Check for Skewness
skewness(cpi.df$cpi_inflation_rate) # reported values are rounded off to 2-decimal points
skewness(cpi.df$log_scmp_epu) # reported values are rounded off to 2-decimal points
skewness(cpi.df$gprc_chn) # reported values are rounded off to 2-decimal points

# Check for Kurtosis
kurtosis(cpi.df$cpi_inflation_rate) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$log_scmp_epu) # reported values are rounded off to 2-decimal points
kurtosis(cpi.df$gprc_chn) # reported values are rounded off to 2-decimal points

# Test for Seasonality
isSeasonal(ts(cpi.df$cpi_inflation_rate), test = "combined", freq = 12) # Non - Sesonal
isSeasonal(ts(cpi.df$log_scmp_epu), test = "combined", freq = 12) # Non - Sesonal
isSeasonal(ts(cpi.df$gprc_chn), test = "combined", freq = 12) # Non - Sesonal

#Tests for non-linearity 
nonlinearityTest(ts(cpi.df$cpi_inflation_rate), verbose = TRUE) # Non - Linear (based on Keenan's one-degree test for nonlinearity)
nonlinearityTest(ts(cpi.df$log_scmp_epu), verbose = TRUE) # Non - Linear (based on Tsay's Test for nonlinearity)
nonlinearityTest(ts(cpi.df$gprc_chn), verbose = TRUE) # Non - Linear (based on Tsay's Test for nonlinearity)

# Check for Long-range dependence
hurstexp(cpi.df$cpi_inflation_rate) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.6881485 
hurstexp(cpi.df$log_scmp_epu) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.8194812 
hurstexp(cpi.df$gprc_chn) # reported values are rounded off to 2-decimal points
# Simple R/S Hurst estimation:         0.7955109 

#Test for Stationarity 
kpss.test(cpi.df$cpi_inflation_rate) #Stationary
# KPSS Level = 0.26837, Truncation lag parameter = 4, p-value = 0.1
kpss.test(cpi.df$log_scmp_epu) #Non-stationary
# KPSS Level = 3.3998, Truncation lag parameter = 4, p-value = 0.01
kpss.test(cpi.df$gprc_chn) #Non-stationary
# KPSS Level = 1.8759, Truncation lag parameter = 4, p-value = 0.01

########################## Statistical Test for Outlier Detection ###################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Create a time variable (monotonic sequence)
time <- c(1:217)

# Convert CPI inflation into a time series object
CPI_china <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 12)
CPI_china
length(CPI_china)

# Convert log(EPU) into a time series object
log_scmp_epu_china <- ts(cpi.df$log_scmp_epu, start = 2003, end = 2021, frequency = 12)
log_scmp_epu_china
length(log_scmp_epu_china)

# Convert GPRC into a time series object
gprc_china <- ts(cpi.df$gprc_chn, start = 2003, end = 2021, frequency = 12)
gprc_china
length(gprc_china)

# Outlier Test using Bonferroni Outlier Test: CPI
test_out_cpi <- outlierTest(lm(CPI_china~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)
# No Studentized residuals with Bonferroni p < 0.05

# Outlier Test using Bonferroni Outlier Test: log(PEU)
test_out_log_epu <- outlierTest(lm(log_scmp_epu_china~time, 
                                   cutoff=0.05, 
                                   n.max=10, 
                                   order=TRUE, 
                                   labels=names(rstudent)))
print(test_out_log_epu)
# No Studentized residuals with Bonferroni p < 0.05

# Outlier Test using Bonferroni Outlier Test: GPRC
test_out_gprc <- outlierTest(lm(gprc_china~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

# One outlier detected
# rstudent unadjusted p-value Bonferroni p
# 185 4.075311          6.476e-05     0.014053
################## End Of Code ############################


