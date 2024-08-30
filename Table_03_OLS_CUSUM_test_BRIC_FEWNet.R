############### Test for Structural breakpoints - OLS-CUSUM Tests: BRIC ###############
# install.packages('zoo')
# install.packages('strucchange')
# install.packages('tseries')
# install.packages('sandwich')

library(zoo)
library(sandwich)
library(strucchange)
library(tseries)

set.seed(20240101) # For reproducibility, we are using this seed value


################## Brazil ############################
# Set the working directory
setwd("./FEWNet/dataset/brazil")
getwd()

# Data
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
cpi.df$date <- as.Date(cpi.df$date)
str(cpi.df)


# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)


# Convert CPI inflation into a time series object
CPI_brazil <- ts(cpi.df$CPI_inflation_rate, start = 2003, end = 2021, frequency = 6)
CPI_brazil
length(CPI_brazil)

# Define a time period variable
time <- c(1:109)

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
# Set the working directory
setwd("./FEWNet/dataset/russia")
getwd()

# Data
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
cpi.df$date <- as.Date(cpi.df$date)
str(cpi.df)

# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_russia <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 6)
CPI_russia
length(CPI_russia)

# Define a time period variable
time <- c(1:109)

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
# Set the working directory
setwd("./FEWNet/dataset/india")
getwd()

# Data
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
cpi.df$Date <- as.Date(cpi.df$Date)
str(cpi.df)

# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$Date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_india <- ts(cpi.df$CPI_inflation_Rate, start = 2003, end = 2021, frequency = 6)
CPI_india
length(CPI_india)

# Define a time period variable
time <- c(1:109)

# Identify the breakpoints
length(CPI_india)
CPI_india_bp <- breakpoints(CPI_india ~ time, h = 7)
CPI_india_bp

# Check for the statistical significance of the break-points
coef(CPI_india_bp)

# Check for the statistical significance of the break-points
addi_CPI_BP <- efp(CPI_india ~ breakfactor(CPI_india_bp), type = "OLS-CUSUM")
# addi_CPI_BP <- efp(CPI_india ~ breakfactor(CPI_india_bp), type = "Score-MOSUM")
plot(addi_CPI_BP)

################## China ############################
# Set the working directory
setwd("./FEWNet/dataset/china")
getwd()

# Data
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
cpi.df$date <- as.Date(cpi.df$date)
str(cpi.df)


# Convert Date into a numeric quantity
cpi.df$date_n <- as.numeric(cpi.df$date)
str(cpi.df)

# Convert CPI inflation into a time series object
CPI_china <- ts(cpi.df$cpi_inflation_rate, start = 2003, end = 2021, frequency = 6)
CPI_china
length(CPI_china)

# Define a time period variable
time <- c(1:109)

# Identify the breakpoints
length(CPI_china)
CPI_china_bp <- breakpoints(CPI_china ~ time, h = 7)
CPI_china_bp

# Check for the statistical significance of the break-points
coef(CPI_china_bp)

# Check for the statistical significance of the break-points
addi_CPI_BP <- efp(CPI_china ~ breakfactor(CPI_china_bp), type = "OLS-CUSUM")
plot(addi_CPI_BP)
####################### End of Code ##############################



























str(cpi.df)
