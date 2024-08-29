######################## Figure 04 : MCB Test ###################
#Install the required packages
# if (!require("devtools"))
#   install.packages("devtools")
# devtools::install_github("trnnick/TStools")
# 
# install.packages('PMCMRplus')
# install.packages('vioplot')
# install.packages('sm')
# install.packages('readxl')

# Check for the libraries
library(tsutils)
library(PMCMRplus)
library(vioplot)
library(readxl)

set.seed(20240101) # For reproducibility, we are using this seed value

# Set the Working Directory
setwd("./FEWNet/dataset")
getwd()

########################### MCB test: BRIC Countries ################################
rank_ew <- read_excel("mcb_test_alternative_12M_24M_paper_data.xlsx")
rank_ew = subset(rank_ew, select = -c(1,2))
rank_ew

# Generate the Plot: MCB test
par(oma = c(0, 0, 0, 1), mar = c(4, 4, 4, 4) + 0.1)
nemenyi(as.matrix(rank_ew), conf.level = 0.85,plottype = "mcb", main = "", ylab = "")
title("MCB plot for RMSE metric", line = 0.8)
mtext(expression(paste( plain("Mean rank"))),side=2,line=2.8, padj=1,at=5,cex=1.2)

###################################### END of Code ########################################################





