############################ Global Characteristics: CPI Inflation, log(EPU) and GPRC: BRIC ######################

# install.packages('entropy')

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
library(zoo)
library(sandwich)
library(strucchange)
library(tseries)

################## Brazil ############################
setwd("/data/brazil")
getwd()

# Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# Calculate Coefficient of Variation and STDV
#calculate CV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
#calculate STDV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x))

# entropy(cpi.df$cpi_inflation_rate, method="MM")
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))


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

time <- c(1:217)

library(car)
# CPI
test_out_cpi <- outlierTest(lm(CPI_brazil~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)

# log(EPU)
test_out_epu <- outlierTest(lm(log_epu_brazil~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_epu)

# GPRC
test_out_gprc <- outlierTest(lm(gprc_bra_brazil~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

############## Test for structural break-points: OLS - CUSUM ##############
# Identify the breakpoints
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_brazil <- ts(cpi.df$CPI_inflation_rate, start = 2003, end = 2021, frequency = 6)
CPI_brazil
length(CPI_brazil)
time <- c(1:109)

# Plot the Series
plot(CPI_brazil, type = "l", las = 1, xaxs = "i", xlab = "", ylab = "", col = "blue")

# Identify the breakpoints
length(CPI_brazil)
CPI_brazil_bp <- breakpoints(CPI_brazil ~ time, h = 7)
CPI_brazil_bp

# Check for the statistical significance of the break-points
coef(CPI_brazil_bp)

# Check for the statistical significance of the break-points
addi_CPI_BP <- efp(CPI_brazil ~ breakfactor(CPI_brazil_bp), type = "OLS-CUSUM")
plot(addi_CPI_BP)

################## Russia ############################
setwd("/dataset/russia")
getwd()

# Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

#calculate CV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
# entropy(cpi.df$cpi_inflation_rate, method="MM")
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

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

########################## Statistical Test for Outlier Detection ########################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

CPI_russia <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 12)
CPI_russia
length(CPI_russia)
time <- c(1:217)

log_epu_russia <- ts(cpi.df$log_epu, start = 2003, end = 2021, frequency = 12)
log_epu_russia
length(log_epu_russia)
time <- c(1:217)

gprc_russia <- ts(cpi.df$gprc_rus, start = 2003, end = 2021, frequency = 12)
gprc_russia
length(gprc_russia)
time <- c(1:217)

library(car)
test_out_cpi <- outlierTest(lm(CPI_russia~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)

test_out_log_epu <- outlierTest(lm(log_epu_russia~time, 
                                   cutoff=0.05, 
                                   n.max=10, 
                                   order=TRUE, 
                                   labels=names(rstudent)))
print(test_out_log_epu)


test_out_gprc <- outlierTest(lm(gprc_russia~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

############## Test for structural break-points: OLS - CUSUM ##############
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_russia <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 6)
CPI_russia
length(CPI_russia)
time <- c(1:109)

# Plot the Series
plot(CPI_russia, type = "l", las = 1, xaxs = "i", xlab = "", ylab = "", col = "blue")

# Identify the breakpoints
length(CPI_russia)
CPI_russia_bp <- breakpoints(CPI_russia ~ time, h=7)
CPI_russia_bp

# Check for the statistical significance of the break-points
coef(CPI_russia_bp)

# Check for the statistical significance of the break-points
addi_CPI_BP <- efp(CPI_russia ~ breakfactor(CPI_russia_bp), type = "OLS-CUSUM")
plot(addi_CPI_BP)

################## India ############################
setwd("/dataset/india")
getwd()

# Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

#calculate CV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
# entropy(cpi.df$cpi_inflation_rate, method="MM")
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

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

########################## Statistical Test for Outlier Detection ########################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$Date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_india <- ts(cpi.df$CPI_inflation_Rate, start = 2003, end = 2021, frequency = 12)
CPI_india
length(CPI_india)
time <- c(1:217)

# Convert CPI inflation into a time series object
log_epu_india <- ts(cpi.df$log_epu, start = 2003, end = 2021, frequency = 12)
log_epu_india
length(log_epu_india)
time <- c(1:217)

# Convert CPI inflation into a time series object
gprc_india <- ts(cpi.df$gprc_ind, start = 2003, end = 2021, frequency = 12)
gprc_india
length(gprc_india)
time <- c(1:217)

# Outlier Test using Bonferroni Outlier Test
test_out_cpi <- outlierTest(lm(CPI_india~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)

test_out_log_epu <- outlierTest(lm(log_epu_india~time, 
                                   cutoff=0.05, 
                                   n.max=10, 
                                   order=TRUE, 
                                   labels=names(rstudent)))
print(test_out_log_epu)

test_out_gprc <- outlierTest(lm(gprc_india~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)

############## Test for structural break-points: OLS - CUSUM ##############
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$Date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_india <- ts(cpi.df$CPI_inflation_Rate, start = 2003, end = 2021, frequency = 6)
CPI_india
length(CPI_india)
time <- c(1:109)

# Plot the Series
plot(CPI_india, type = "l", las = 1, xaxs = "i", xlab = "", ylab = "", col = "blue")

# Identify the breakpoints
length(CPI_india)
CPI_india_bp <- breakpoints(CPI_india ~ time, h = 7)
CPI_india_bp

# Check for the statistical significance of the break-points
coef(CPI_india_bp)

# Check for the statistical significance of the break-points
addi_CPI_BP <- efp(CPI_india ~ breakfactor(CPI_india_bp), type = "OLS-CUSUM")
plot(addi_CPI_BP)

################## China ############################
setwd("/dataset/india")
getwd()

#calculate CV for each column in data frame
sapply(cpi.df[-1], function(x) sd(x) / mean(x) * 100)
# entropy(cpi.df$cpi_inflation_rate, method="MM")
sapply(cpi.df[-1], function(x) entropy(x, method="MM"))

# Find the Min-value, Max Value, Mean Value, Median Value, Q1-value, Q3-value
sapply(cpi.df[-1], function(x) min(x))
sapply(cpi.df[-1], function(x) max(x))
sapply(cpi.df[-1], function(x) mean(x))
sapply(cpi.df[-1], function(x) median(x))
sapply(cpi.df[-1], function(x) quantile(x, prob=c(.25,.5,.75), type=1))

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

########################## Statistical Test for Outlier Detection ########################
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_china <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 12)
CPI_china
length(CPI_china)
time <- c(1:217)

log_scmp_epu_china <- ts(cpi.df$log_scmp_epu, start = 2003, end = 2021, frequency = 12)
log_scmp_epu_china
length(log_scmp_epu_china)
time <- c(1:217)

gprc_china <- ts(cpi.df$gprc_chn, start = 2003, end = 2021, frequency = 12)
gprc_china
length(gprc_china)
time <- c(1:217)

# Outlier Test using Bonferroni Outlier Test
library(car)
test_out_cpi <- outlierTest(lm(CPI_china~time, 
                               cutoff=0.05, 
                               n.max=10, 
                               order=TRUE, 
                               labels=names(rstudent)))
print(test_out_cpi)

test_out_log_epu <- outlierTest(lm(log_scmp_epu_china~time, 
                                   cutoff=0.05, 
                                   n.max=10, 
                                   order=TRUE, 
                                   labels=names(rstudent)))
print(test_out_log_epu)

test_out_gprc <- outlierTest(lm(gprc_china~time, 
                                cutoff=0.05, 
                                n.max=10, 
                                order=TRUE, 
                                labels=names(rstudent)))
print(test_out_gprc)
############## Test for structural break-points: OLS - CUSUM ##############
# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_china <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 6)
CPI_china
length(CPI_china)
time <- c(1:109)

# Plot the Series
plot(CPI_china, type = "l", las = 1, xaxs = "i", xlab = "", ylab = "", col = "blue")

# Identify the breakpoints
length(CPI_china)
CPI_china_bp <- breakpoints(CPI_china ~ time, h = 7)
CPI_china_bp

# Check for the statistical significance of the break-points
coef(CPI_china_bp)

# Check for the statistical significance of the break-points
addi_CPI_BP <- efp(CPI_china ~ breakfactor(CPI_china_bp), type = "OLS-CUSUM")
plot(addi_CPI_BP)

############################## End of Code #############################
