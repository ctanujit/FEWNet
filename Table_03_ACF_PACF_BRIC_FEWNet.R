######################### ACF - PACF Plots: BRIC ####################
set.seed(20240101) # For reproducibility, we are using this seed value

# Load the necessary libraries
library(forecast)
library(ggplot2)
library(gridExtra)

################## Brazil ############################
# Set the working directory
setwd("/FEWNet/dataset/brazil")
getwd()

# Dataset
cpi.df <- read.csv("Brazil_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# lets remove the last 24 observations (test data) -- Time frame considered: 2003M01 - 2019M11
cpi.train.df<-cpi.df[1:203,1:4]
str(cpi.train.df)

# Create ACF plot
acf_plot <- ggAcf(cpi.train.df$CPI_inflation_rate, lag.max = 36) +
  ggtitle("ACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create PACF plot
pacf_plot <- ggPacf(cpi.train.df$CPI_inflation_rate, lag.max = 36) +
  ggtitle("PACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange both plots side by side
grid.arrange(acf_plot, pacf_plot, ncol = 2)


################## Russia ############################
# Set the working directory
setwd("/FEWNet/dataset/russia")
getwd()

# Dataset
cpi.df <- read.csv("RUS_CPI_inf_rate_Monthly_mulvar_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# lets remove the last 24 observations (test data) -- Time frame considered: 2003M01 - 2019M11
cpi.train.df<-cpi.df[1:203,1:4]
str(cpi.train.df)

# Create ACF plot
acf_plot <- ggAcf(cpi.train.df$cpi_inflation_rate, lag.max = 36) +
  ggtitle("ACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create PACF plot
pacf_plot <- ggPacf(cpi.train.df$cpi_inflation_rate, lag.max = 36) +
  ggtitle("PACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange both plots side by side
grid.arrange(acf_plot, pacf_plot, ncol = 2)

################## India ############################
# Set the working directory
setwd("/FEWNet/dataset/india")
getwd()

# Dataset
cpi.df <- read.csv("India_CPI_inf_rate_Monthly_base_mulvar_cpi_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# lets remove the last 24 observations (test data) -- Time frame considered: 2003M01 - 2019M11
cpi.train.df<-cpi.df[1:203,1:4]
str(cpi.train.df)

# Create ACF plot
acf_plot <- ggAcf(cpi.train.df$CPI_inflation_Rate, lag.max = 36) +
  ggtitle("ACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create PACF plot
pacf_plot <- ggPacf(cpi.train.df$CPI_inflation_Rate, lag.max = 36) +
  ggtitle("PACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange both plots side by side
grid.arrange(acf_plot, pacf_plot, ncol = 2)

################## China ############################
# Set the working directory
setwd("/FEWNet/dataset/china")
getwd()

# Dataset
cpi.df <- read.csv("China_CPI_inf_rate_Monthly_mulvariate_epu_gprc_202201.csv",header=TRUE)
str(cpi.df)

# lets remove the last 24 observations (test data) -- Time frame considered: 2003M01 - 2019M11
cpi.train.df<-cpi.df[1:203,1:4]
str(cpi.train.df)

# Create ACF plot
acf_plot <- ggAcf(cpi.train.df$cpi_inflation_rate, lag.max = 36) +
  ggtitle("ACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create PACF plot
pacf_plot <- ggPacf(cpi.train.df$cpi_inflation_rate, lag.max = 36) +
  ggtitle("PACF Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Arrange both plots side by side
grid.arrange(acf_plot, pacf_plot, ncol = 2)




