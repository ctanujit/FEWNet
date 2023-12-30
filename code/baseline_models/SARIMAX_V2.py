########### SARIMAX Model:Code for generating long-term forecasts ##################


# Declaring the working library
import os
os.chdir('/content')

# Load the Data
import pandas as pd
df_cpi_ind = pd.read_csv("India_CPI_inf_rate_Monthly_202201.csv",
                        date_parser=True,
                        encoding='latin1')

df_cpi_ind.rename(columns = {'ï»¿Date':'date'}, inplace = True)

df_cpi_ind.info()

# Check for the Missing Values
# Remove the rows with missing observation for one of the column
missing_percentage = df_cpi_ind.isnull().sum()/len(df_cpi_ind)* 100
missing_percentage.sort_values(ascending=True)

# Convert the Date in a datetime object
df_cpi_ind['date'] = pd.to_datetime(df_cpi_ind['date'])

# Creating a copy of the table for any manipulation
df_cpi_ind_new = df_cpi_ind.copy()

df_cpi_ind_new.info()

import statsmodels as sm
print(sm.__version__)

# !pip install -U statsmodels
# !pip install -U pmdarima
# These steps are optional and once done , no need to repeat the same
import statsmodels.api as sm
import pandas as pd
# Christiano Fitzgerald asymmetric, random walk filter.
cpi_cycle_cf, cpi_trend_cf = sm.tsa.filters.cffilter(df_cpi_ind_new["CPI_inflation_Rate"])
epu_cycle_cf, epu_trend_cf = sm.tsa.filters.cffilter(df_cpi_ind_new["log_epu"])
gprc_ind_cycle_cf, gprc_ind_trend_cf = sm.tsa.filters.cffilter(df_cpi_ind_new["gprc_ind"])
# Hodrick-Prescott filter.
cpi_cycle_hp, cpi_trend_hp = sm.tsa.filters.hpfilter(df_cpi_ind_new["CPI_inflation_Rate"])
epu_cycle_hp, epu_trend_hp = sm.tsa.filters.hpfilter(df_cpi_ind_new["log_epu"])
gprc_ind_cycle_hp, gprc_ind_trend_hp = sm.tsa.filters.hpfilter(df_cpi_ind_new["gprc_ind"])
# cpi_cycles

# Add the HP trend filter and CF cyclic filter as the exogenous factor
df_cpi_ind_new['cpi_cycle_cf'] = cpi_cycle_cf.copy()
df_cpi_ind_new['cpi_trend_hp'] = cpi_trend_hp.copy()
# epu
df_cpi_ind_new['epu_cycle_cf'] = epu_cycle_cf.copy()
df_cpi_ind_new['epu_trend_hp'] = epu_trend_hp.copy()
# gprc
df_cpi_ind_new['gprc_ind_cycle_cf'] = gprc_ind_cycle_cf.copy()
df_cpi_ind_new['gprc_ind_trend_hp'] = gprc_ind_trend_hp.copy()

df_cpi_ind_new.head()

dataset = df_cpi_ind_new.copy()
dataset.info()

# Final Dataset
dataset_new = dataset[['CPI_inflation_Rate',
                       'date',
                       'epu_cycle_cf',
                       'epu_trend_hp',
                       'log_epu',
                       'gprc_ind',
                       'gprc_ind_trend_hp',
                       'gprc_ind_cycle_cf',
                       'cpi_cycle_cf',
                       'cpi_trend_hp'
                       ]]

"""__Development of SARIMA model__"""

import pmdarima
print(pmdarima.__version__)

# Grid-Search and Cross Validation
from pmdarima.arima import auto_arima
stepwise_model = auto_arima(df_cpi_ind_new['CPI_inflation_Rate'],
                            start_p=1,
                            start_q=1,
                            max_p=12,
                            max_q=12,
                            m=12,
                            start_P=0,
                            start_Q=0,
                            seasonal=True,
                            d=1,
                            D=1,
                            trace=True,
                            error_action='ignore',
                            suppress_warnings=True,
                            stepwise=True
                            )
print(stepwise_model.aic())

dataset_new = dataset[['CPI_inflation_Rate',
                       'log_epu',
                       'gprc_ind',
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

"""- Best model: SARIMA(3,1,0)(2,1,0)[12]
- AIC: 622.5270729614763
"""

"""__Development of SARIMA(X:6) model__"""

import random
random.seed(12345)
import statsmodels.api as sm
from sklearn.metrics import r2_score

mod_sarimax_6 = sm.tsa.statespace.SARIMAX(
    endog=df_train['CPI_inflation_Rate'],
    exog=df_train[['epu_cycle_cf',
                   'epu_trend_hp',
                   'gprc_ind_trend_hp',
                   'gprc_ind_cycle_cf',
                   'cpi_cycle_cf',
                   'cpi_trend_hp'
                   ]],
    trend='c',
    order=(3,1,0),
    seasonal_order=(2,1,0,12),
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

plt.plot(list(df_test['CPI_inflation_Rate']))
plt.plot(list(fcst_sarimax_6))
plt.xlabel('Steps of the test data')
plt.ylabel('CPI')
plt.legend(['test', 'forecast'])
plt.show()
r2_score(df_test['CPI_inflation_Rate'], fcst_sarimax_6)

# Check the Model Diagnostics
res_sarimax_6.plot_diagnostics(figsize=(16, 8))
plt.show()
