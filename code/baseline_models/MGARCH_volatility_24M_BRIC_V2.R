################################# BRIC: The Markov-Switching GARCH Model (MSGARCH): 24M #######################
# install.packages('MSGARCH')
library(MSGARCH)
######################### MSGARCH for Brazil: 24M Volatility Forecasting #########################

setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)


# Test Data
cpi.test.df<-read.csv("df_test_cpi_bzl_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

cpi.train.df$Date <- as.Date(cpi.train.df$date)
str(cpi.train.df)

# Creating a vector of CPI inflation numbers
str(cpi.train.df[['CPI_inflation_rate']])

# create model specification
spec <- CreateSpec()

# predict from MCMC fit --- Considered These Values
fit <- FitMCMC(spec = spec, data = cpi.train.df[['CPI_inflation_rate']])
set.seed(1234)
pred <- predict(object = fit, nahead = 24L, do.return.draw = TRUE)
pred$vol

######################### MSGARCH for Russia: 24M Volatility Forecasting #########################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_rus_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)


# Test Data
cpi.test.df<-read.csv("df_test_cpi_rus_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

cpi.train.df$Date <- as.Date(cpi.train.df$date)
str(cpi.train.df)

# Creating a vector of CPI inflation numbers
str(cpi.train.df[['cpi_inflation_rate']])

# create model specification
spec <- CreateSpec()

# predict from MCMC fit --- Considered These Values
fit <- FitMCMC(spec = spec, data = cpi.train.df[['cpi_inflation_rate']])
set.seed(1234)
pred <- predict(object = fit, nahead = 24L, do.return.draw = TRUE)
pred$vol

######################### MSGARCH for India: 24M Volatility Forecasting #########################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)


# Test Data
cpi.test.df<-read.csv("df_test_cpi_ind_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

cpi.train.df$Date <- as.Date(cpi.train.df$date)
str(cpi.train.df)

# Creating a vector of CPI inflation numbers
str(cpi.train.df[['CPI_inflation_Rate']])

# create model specification
spec <- CreateSpec()

# predict from MCMC fit --- Considered These Values
fit <- FitMCMC(spec = spec, data = cpi.train.df[['CPI_inflation_Rate']])
set.seed(1234)
pred <- predict(object = fit, nahead = 24L, do.return.draw = TRUE)
pred$vol

######################### MSGARCH for China: 24M Volatility Forecasting #########################
setwd('/FEWNet/dataset/brazil')
getwd()

# Train Data
cpi.train.df<-read.csv("df_train_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.train.df)


# Test Data
cpi.test.df<-read.csv("df_test_cpi_chn_lag_all_24M_R.csv",header=TRUE)
str(cpi.test.df)

cpi.train.df$Date <- as.Date(cpi.train.df$date)
str(cpi.train.df)

# Creating a vector of CPI inflation numbers
str(cpi.train.df[['cpi_inflation_rate']])

# create model specification
spec <- CreateSpec()

# predict from MCMC fit --- Considered These Values
fit <- FitMCMC(spec = spec, data = cpi.train.df[['cpi_inflation_rate']])
set.seed(1234)
pred <- predict(object = fit, nahead = 24L, do.return.draw = TRUE)
pred$vol
