###################### Figure1 - Wavelet Coherence Analysis: BRIC Countries ###############
############## Wavelet Coherence Analysis (WCA) for the BRIC Countries #############

# install.packages('biwavelet')

# Import all the libraries
# library(wavelets)
# library(waveslim)
# library(wavemulcor)
# library(colorspace)
# library(W2CWM2C)

library(biwavelet)

set.seed(20240101) # For reproducibility, we are using this seed value

########################### Brazil ####################
# Set the working directory
setwd("./FEWNet/dataset/brazil")
getwd()

# Read the base Table
cpi.df.bzl<-read.csv("Brazil_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
# Convert the Date
library(lubridate)
cpi.df.bzl$date1 <- 1:length(cpi.df.bzl$CPI_inflation_rate)
# cpi.df.bzl$date <- as.Date(cpi.df.bzl$date)
str(cpi.df.bzl)

########### Wavelet Cohenrence Analysis ################
# Define two sets of variables with time stamps
# Figure 01 WCA Plot: log(EPU) and CPI inflation
t1.cpi = cbind(cpi.df.bzl$date1, cpi.df.bzl$CPI_inflation_rate)
t2.epu = cbind(cpi.df.bzl$date1, cpi.df.bzl$log_epu)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.epu, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs log(EPU)")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))

#Figure 01 WCA Plot:GPRC and CPI inflation
t1.cpi = cbind(cpi.df.bzl$date1, cpi.df.bzl$CPI_inflation_rate)
t2.gprc = cbind(cpi.df.bzl$date1, cpi.df.bzl$gprc_bra)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.gprc, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs GPRC")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))
######################################################################################

########################### Russia ####################
# Set the working directory
setwd("./FEWNet/dataset/russia")
getwd()

# Read the base Table
cpi.df.rus<-read.csv("RUS_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
cpi.df.rus$date1 <- 1:length(cpi.df.rus$cpi_inflation_rate)
str(cpi.df.rus)

########### Wavelet Cohenrence Analysis ################
# Define two sets of variables with time stamps
# Figure 01 WCA Plot:log(EPU) and CPI inflation
t1.cpi = cbind(cpi.df.rus$date1, cpi.df.rus$cpi_inflation_rate)
t2.epu = cbind(cpi.df.rus$date1, cpi.df.rus$log_epu)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.epu, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs log(EPU)")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))

# Figure 01 WCA Plot:GPRC and CPI inflation
t1.cpi = cbind(cpi.df.rus$date1, cpi.df.rus$cpi_inflation_rate)
t2.gprc = cbind(cpi.df.rus$date1, cpi.df.rus$gprc_rus)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.gprc, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs GPRC")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))
#######################################################################################

########################### India ####################
# Set the working directory
setwd("./FEWNet/dataset/india")
getwd()

# Read the base Table
cpi.df.ind<-read.csv("India_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
cpi.df.ind$date1 <- 1:length(cpi.df.ind$CPI_inflation_Rate)
str(cpi.df.ind)

########### Wavelet Cohenrence Analysis ################
# Define two sets of variables with time stamps
# Figure 01 WCA Plot:log(EPU) and CPI inflation
t1.cpi = cbind(cpi.df.ind$date1, cpi.df.ind$CPI_inflation_Rate)
t2.epu = cbind(cpi.df.ind$date1, cpi.df.ind$log_epu)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.epu, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs log(EPU)")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))

# Figure 01 WCA Plot:GPRC and CPI inflation
t1.cpi = cbind(cpi.df.ind$date1, cpi.df.ind$CPI_inflation_Rate)
t2.gprc = cbind(cpi.df.ind$date1, cpi.df.ind$gprc_ind)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.gprc, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs GPRC")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))
#######################################################################

########################### China ####################
# Set the working directory
setwd("./FEWNet/dataset/china")
getwd()

# Read the base Table
cpi.df.chn<-read.csv("China_CPI_inf_rate_Monthly_202201.csv",header=TRUE)
cpi.df.chn$date1 <- 1:length(cpi.df.chn$cpi_inflation_rate)
str(cpi.df.chn)

########### Wavelet Cohenrence Analysis ################
# Define two sets of variables with time stamps
# Figure 01 WCA Plot:log(EPU) and CPI inflation
t1.cpi = cbind(cpi.df.chn$date1, cpi.df.chn$cpi_inflation_rate)
t2.epu = cbind(cpi.df.chn$date1, cpi.df.chn$log_scmp_epu)

# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.epu, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs log(EPU)")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))

# Figure 01 WCA Plot:GPRC and CPI inflation
t1.cpi = cbind(cpi.df.chn$date1, cpi.df.chn$cpi_inflation_rate)
t2.gprc = cbind(cpi.df.chn$date1, cpi.df.chn$gprc_chn)
# Specify the number of iterations. The more, the better (>1000).
nrands = 1000
wtc.AB = wtc(t1.cpi, t2.gprc, nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: CPI vs GPRC")

# Adding grid lines
n = length(t1.cpi[, 1])
abline(v = seq(12, n, 12), h = 1:16, col = "brown", lty = 1, lwd = 1)

# Defining x labels
axis(side = 3, at = c(seq(0, n, 12)), labels = c(seq(2003, 2021, 1)))
########################################### END of Code ########################################

