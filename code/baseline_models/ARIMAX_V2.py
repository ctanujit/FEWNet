########### ARIMAX Model:Code for generating long-term forecasts ##################

# !pip install -U scikit-learn
# !pip install -U statsmodels
# !pip install pmdarima
# This is an example code for the ARIMAx model. The same code module can be replicated for other geographies

# Set the working directory
import os
os.chdir("/content/FEWNet/dataset/brazil")
# Load the Data
import pandas as pd
df_cpi_bzl = pd.read_csv("Brazil_CPI_inf_rate_Monthly_202201.csv",
                        date_parser=True,
                        encoding='latin1')

df_cpi_bzl.info()

# rename the date column
df_cpi_bzl.rename(columns = {'ï»¿date':'date'}, inplace = True)

df_cpi_bzl.info()

df_cpi_bzl.shape

# Check for the Missing Values
# Remove the rows with missing observation for one of the column
missing_percentage = df_cpi_bzl.isnull().sum()/len(df_cpi_bzl)* 100
missing_percentage.sort_values(ascending=True)

# Convert the Date in a datetime object
df_cpi_bzl['date'] = pd.to_datetime(df_cpi_bzl['date'])
# Creating a copy of the table for any manipulation
df_cpi_bzl_new = df_cpi_bzl.copy()

import statsmodels as sm
print(sm.__version__)

import statsmodels.api as sm
import pandas as pd
# Christiano Fitzgerald asymmetric, random walk filter.
cpi_cycle_cf, cpi_trend_cf = sm.tsa.filters.cffilter(df_cpi_bzl_new["CPI_inflation_rate"])
epu_cycle_cf, epu_trend_cf = sm.tsa.filters.cffilter(df_cpi_bzl_new["log_epu"])
gprc_ind_cycle_cf, gprc_ind_trend_cf = sm.tsa.filters.cffilter(df_cpi_bzl_new["gprc_bra"])
# Hodrick-Prescott filter.
cpi_cycle_hp, cpi_trend_hp = sm.tsa.filters.hpfilter(df_cpi_bzl_new["CPI_inflation_rate"])
epu_cycle_hp, epu_trend_hp = sm.tsa.filters.hpfilter(df_cpi_bzl_new["log_epu"])
gprc_ind_cycle_hp, gprc_ind_trend_hp = sm.tsa.filters.hpfilter(df_cpi_bzl_new["gprc_bra"])
# cpi_cycles
# cpi_trend

# Add the HP trend filter and CF cyclic filter as the exogenous factor
df_cpi_bzl_new['cpi_cycle_cf'] = cpi_cycle_cf.copy()
df_cpi_bzl_new['cpi_trend_hp'] = cpi_trend_hp.copy()
# epu
df_cpi_bzl_new['epu_cycle_cf'] = epu_cycle_cf.copy()
df_cpi_bzl_new['epu_trend_hp'] = epu_trend_hp.copy()
# gprc
df_cpi_bzl_new['gprc_ind_cycle_cf'] = gprc_ind_cycle_cf.copy()
df_cpi_bzl_new['gprc_ind_trend_hp'] = gprc_ind_trend_hp.copy()

df_cpi_bzl_new.info()

dataset = df_cpi_bzl_new.copy()
dataset.info()


dataset_new = dataset[['CPI_inflation_rate',
                       'date',
                       'epu_cycle_cf',
                       'epu_trend_hp',
                       'gprc_ind_trend_hp',
                       'gprc_ind_cycle_cf',
                       'cpi_cycle_cf',
                       'cpi_trend_hp'
                       ]]

# Create Train - Test Data
n_obs=24
# Remove first 12 data points
X_train, X_test = dataset[24:-n_obs], dataset[-n_obs:]
print(X_train.shape, X_test.shape)

# X_train.info()
X_train = X_train[['CPI_inflation_rate',
                   'date',
                   'epu_cycle_cf',
                    'epu_trend_hp',
                    'gprc_ind_trend_hp',
                    'gprc_ind_cycle_cf',
                   'cpi_cycle_cf',
                   'cpi_trend_hp'
                   ]].copy()
X_test = X_test[['CPI_inflation_rate',
                 'date',
                   'epu_cycle_cf',
                    'epu_trend_hp',
                    'gprc_ind_trend_hp',
                    'gprc_ind_cycle_cf',
                   'cpi_cycle_cf',
                   'cpi_trend_hp']].copy()

"""__Development of ARIMA model__"""

import pmdarima
print(pmdarima.__version__)

# Grid-Search and Cross Validation
from pmdarima.arima import auto_arima
stepwise_model = auto_arima(df_cpi_bzl_new['CPI_inflation_rate'],
                            start_p=1,
                            start_q=1,
                            max_p=12,
                            max_q=12,
                            m=12,
                            seasonal=False,
                            d=1,
                            trace=True,
                            error_action='ignore',
                            suppress_warnings=True,
                            stepwise=True
                            )
print(stepwise_model.aic())

dataset_new = dataset[['CPI_inflation_rate',
                       'log_epu',
                       'gprc_bra',
                       'epu_cycle_cf',
                       'epu_trend_hp',
                       'gprc_ind_trend_hp',
                       'gprc_ind_cycle_cf',
                       'cpi_cycle_cf',
                       'cpi_trend_hp'
                       ]].copy()

# Create Train - Test Data
n_obs=24
df_train, df_test = dataset_new[24:-n_obs], dataset_new[-n_obs:]
print(df_train.shape, df_test.shape)

import random
random.seed(12345)
import statsmodels.api as sm
from sklearn.metrics import r2_score

"""- Best model: ARIMA(1,1,2)
- AIC: 220.01899488432338
"""

"""__Development of SARIMA(X:6) model__"""

import random
random.seed(12345)
import statsmodels.api as sm
from sklearn.metrics import r2_score

mod_sarimax_6 = sm.tsa.statespace.SARIMAX(
    endog=df_train['CPI_inflation_rate'],
    exog=df_train[['epu_cycle_cf',
                   'epu_trend_hp',
                   'gprc_ind_trend_hp',
                   'gprc_ind_cycle_cf',
                   'cpi_cycle_cf',
                   'cpi_trend_hp'
                   ]],
    trend='c',
    order=(1,1,2),
    # seasonal_order=(2,1,0,12),
)

res_sarimax_6 = mod_sarimax_6.fit(disp=-1)
print(res_sarimax_6.summary())

df_train_exog = df_train[['epu_cycle_cf',
                   'epu_trend_hp',
                   'gprc_ind_trend_hp',
                   'gprc_ind_cycle_cf',
                   'cpi_cycle_cf',
                   'cpi_trend_hp']].copy()

fcst_sarimax_6 = res_sarimax_6.forecast(steps=24,
                                        exog = df_train_exog[0:24])

fcst_sarimax_6.values

# Combine the Test and the Forecast data for the test series
df_test['forecast_cpi_6'] = round(fcst_sarimax_6,2)
