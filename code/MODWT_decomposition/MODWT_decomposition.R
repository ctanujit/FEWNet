############################ MODWT Decomposition of CPI Inflation series for BRIC Countries #################

######################## Brazil ###########################
# Read the base Table
setwd("/Github/dataset/brazil")
getwd()

cpi.df.bzl<-read.csv("Brazil_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
str(cpi.df.bzl)

# Install the required library
# install.packages('wavelets')

# Load the package
library(wavelets)

# Convert the series into numeric vector: optional
x <- as.vector(cpi.df.bzl$CPI_inflation_rate)
# MODWT decomposition
modwtobj.bzl <- modwt(x, filter="haar", n.levels = 5, boundary = "periodic", fast = TRUE)
# Summary of the decomosition
summary(modwtobj.bzl)
# Generate the Plot
plot(modwtobj.bzl)

######################## Russia ###########################
# Working directory
# Read the base Table
setwd("/Users/shovonsengupta/Desktop/All/Time_Series_Forecasting_Research/Inflation_Forecasting_BRIC_Paper/data/BRIC_CPI_INF_UNCERT_data/")
getwd()

# Read the base Table
setwd("/Github/dataset/russia")
getwd()

cpi.df.rus<-read.csv("RUS_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
str(cpi.df.rus)

# Install the required library
# install.packages('wavelets')

# Load the package
library(wavelets)

# Convert the series into numeric vector: optional
x <- as.vector(cpi.df.rus$cpi_inflation_rate)
# MODWT decomposition
modwtobj.rus <- modwt(x, filter="haar", n.levels = 5, boundary = "periodic", fast = TRUE)
# Summary of the decomosition
summary(modwtobj.rus)
# Generate the Plot
plot(modwtobj.rus)

######################## India ###########################
# Working directory
# Read the base Table
setwd("/Github/dataset/india")
getwd()

cpi.df.ind<-read.csv("India_CPI_inf_rate_Monthly_202201.csv", header=TRUE)
str(cpi.df.ind)

# Install the required library
# install.packages('wavelets')

# Load the package
library(wavelets)

# Convert the series into numeric vector: optional
x <- as.vector(cpi.df.ind$CPI_inflation_Rate)
# MODWT decomposition
modwtobj.ind <- modwt(x, filter="haar", n.levels = 5, boundary = "periodic", fast = TRUE)
# Summary of the decomosition
summary(modwtobj.ind)
# Generate the Plot
plot(modwtobj.ind)

######################## China ###########################
# Working directory
# Read the base Table
setwd("/Github/dataset/india")
getwd()

cpi.df.chn<-read.csv("China_CPI_inf_rate_Monthly_202201.csv", header=TRUE)
str(cpi.df.chn)

# Install the required library
# install.packages('wavelets')

# Load the package
library(wavelets)

# Convert the series into numeric vector: optional
x <- as.vector(cpi.df.chn$cpi_inflation_rate)
# MODWT decomposition
modwtobj.chn <- modwt(x, filter="haar", n.levels = 5, boundary = "periodic", fast = TRUE)
# Summary of the decomosition
summary(modwtobj.chn)
# Generate the Plot
plot(modwtobj.chn)

############################### END of Code ##################################
