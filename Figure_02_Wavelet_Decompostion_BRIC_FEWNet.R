###################### Figure2 - Wavelet Decompostion: BRIC Countries ###############
############################ MODWT Decomposition of CPI Inflation series for BRIC Countries #################
# Install the required library
# install.packages('wavelets')

# Load the package
library(wavelets)

set.seed(20240101) # For reproducibility, we are using this seed value

########################### Brazil ####################
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()


# Read the base Table
cpi.df.bzl<-read.csv("Brazil_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
str(cpi.df.bzl)

# Convert the series into numeric vector: optional
x <- as.vector(cpi.df.bzl$CPI_inflation_rate)
# MODWT decomposition
modwtobj.bzl <- modwt(x, filter="haar", n.levels = 5, boundary = "periodic", fast = TRUE)
# Summary of the decomosition
summary(modwtobj.bzl)
# Generate the Plot
plot(modwtobj.bzl)

######################## Russia ###########################
# Set the working directory
setwd("/FEWNet/dataset/russia")
getwd()

# Read the base Table
cpi.df.rus<-read.csv("RUS_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
str(cpi.df.rus)

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
setwd("/FEWNet/dataset/india")
getwd()

# Read the base Table
cpi.df.ind<-read.csv("India_CPI_inf_rate_Monthly_202201.csv", header=TRUE)
str(cpi.df.ind)

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
setwd("/FEWNet/dataset/china")
getwd()

# Read the base Table
cpi.df.chn<-read.csv("China_CPI_inf_rate_Monthly_202201.csv", header=TRUE)
str(cpi.df.chn)

# Convert the series into numeric vector: optional
x <- as.vector(cpi.df.chn$cpi_inflation_rate)
# MODWT decomposition
modwtobj.chn <- modwt(x, filter="haar", n.levels = 5, boundary = "periodic", fast = TRUE)
# Summary of the decomosition
summary(modwtobj.chn)
# Generate the Plot
plot(modwtobj.chn)
############################### END of Code ##################################
