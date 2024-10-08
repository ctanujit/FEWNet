########### ARDL Model:Code for generating long-term forecasts ##################
# !pip install -U statsmodels
# This is an example code for the ARDL model. The same code module can be replicated for other geographies

# Set the working directory
import os
os.chdir("/content/FEWNet/dataset/russia")

# Load the Data
import pandas as pd
import statsmodels as sm
df_cpi_rus = pd.read_csv("./RUS_CPI_inf_rate_Monthly_202201.csv",
                        date_parser=True,
                        encoding='latin1')
############# Data Pre-processing: (can be ignored) Example code ##########################
df_cpi_rus.info()

df_cpi_rus.rename(columns = {'ï»¿date':'date'}, inplace = True)

df_cpi_rus.head()

df_cpi_rus.shape

# Check for the Missing Values
# Remove the rows with missing observation for one of the column
missing_percentage = df_cpi_rus.isnull().sum()/len(df_cpi_rus)* 100
missing_percentage.sort_values(ascending=True)

# Convert the Date in a datetime object
df_cpi_rus['date'] = pd.to_datetime(df_cpi_rus['date'])

# Creating a copy of the table for any manipulation
df_cpi_rus_new = df_cpi_rus.copy()

# Decomposition using HP and CF filters
import statsmodels.api as sm
import pandas as pd
# Christiano Fitzgerald asymmetric, random walk filter.
cpi_cycle_cf, cpi_trend_cf = sm.tsa.filters.cffilter(df_cpi_rus_new["cpi_inflation_rate"])
epu_cycle_cf, epu_trend_cf = sm.tsa.filters.cffilter(df_cpi_rus_new["log_epu"])
gprc_rus_cycle_cf, gprc_rus_trend_cf = sm.tsa.filters.cffilter(df_cpi_rus_new["gprc_rus"])
# Hodrick-Prescott filter.
cpi_cycle_hp, cpi_trend_hp = sm.tsa.filters.hpfilter(df_cpi_rus_new["cpi_inflation_rate"])
epu_cycle_hp, epu_trend_hp = sm.tsa.filters.hpfilter(df_cpi_rus_new["log_epu"])
gprc_rus_cycle_hp, gprc_rus_trend_hp = sm.tsa.filters.hpfilter(df_cpi_rus_new["gprc_rus"])
# cpi_cycles
# cpi_trend

# type(cpi_cycle)
cpi_cycle_cf.head()

# Add the HP trend filter and CF cyclic filter as the exogenous factor
# cpi
df_cpi_rus_new['cpi_cycle_cf'] = cpi_cycle_cf.copy()
df_cpi_rus_new['cpi_trend_hp'] = cpi_trend_hp.copy()
# epu
df_cpi_rus_new['epu_cycle_cf'] = epu_cycle_cf.copy()
df_cpi_rus_new['epu_trend_hp'] = epu_trend_hp.copy()
# gprc
df_cpi_rus_new['gprc_rus_cycle_cf'] = gprc_rus_cycle_cf.copy()
df_cpi_rus_new['gprc_rus_trend_hp'] = gprc_rus_trend_hp.copy()

df_cpi_rus_new.info()

df_cpi_rus_new.head()

# Commented out IPython magic to ensure Python compatibility.
# Plot
import matplotlib.pyplot as plt
# %matplotlib inline
fig, ax = plt.subplots()
cpi_cycle_cf.plot()
cpi_trend_hp.plot()
df_cpi_rus_new['cpi_inflation_rate'].plot()
plt.show()

# Commented out IPython magic to ensure Python compatibility.
# Plot
import matplotlib.pyplot as plt
# %matplotlib inline
fig, ax = plt.subplots()
epu_cycle_cf.plot()
epu_trend_hp.plot()
df_cpi_rus_new['log_epu'].plot()
plt.show()

df_cpi_rus_new.info()
###################### Creation of Final Dataset ###################
dataset = df_cpi_rus_new.copy()
dataset_new = dataset[['cpi_inflation_rate',
                       'epu_cycle_cf',
                       'epu_trend_hp',
                       'gprc_rus_cycle_cf',
                       'gprc_rus_trend_hp',
                       'cpi_cycle_cf',
                       'cpi_trend_hp'
                       ]].copy()

dataset_new = dataset_new[['cpi_inflation_rate',
                       'epu_cycle_cf',
                       'epu_trend_hp',
                       'gprc_rus_cycle_cf',
                       'gprc_rus_trend_hp',
                       'cpi_cycle_cf',
                       'cpi_trend_hp']].copy()

dataset_new_lag = dataset_new.copy()
dataset_new_lag.info()

################## Lag Generation ############################
# Create a dataset with all the Lags - 2versions [One with the original series of EPU & GPR]
# And other with the log transformations for EPU and GPR
dataset_new_lag = dataset_new.copy()
#CPI lags
#cpi_inflation_rate lags
dataset_new_lag['cpi_inflation_rate_l1'] = dataset_new_lag['cpi_inflation_rate'].shift(1)
dataset_new_lag['cpi_inflation_rate_l2'] = dataset_new_lag['cpi_inflation_rate'].shift(2)
dataset_new_lag['cpi_inflation_rate_l3'] = dataset_new_lag['cpi_inflation_rate'].shift(3)
dataset_new_lag['cpi_inflation_rate_l4'] = dataset_new_lag['cpi_inflation_rate'].shift(4)
dataset_new_lag['cpi_inflation_rate_l5'] = dataset_new_lag['cpi_inflation_rate'].shift(5)
dataset_new_lag['cpi_inflation_rate_l6'] = dataset_new_lag['cpi_inflation_rate'].shift(6)
dataset_new_lag['cpi_inflation_rate_l7'] = dataset_new_lag['cpi_inflation_rate'].shift(7)
dataset_new_lag['cpi_inflation_rate_l8'] = dataset_new_lag['cpi_inflation_rate'].shift(8)
dataset_new_lag['cpi_inflation_rate_l9'] = dataset_new_lag['cpi_inflation_rate'].shift(9)
dataset_new_lag['cpi_inflation_rate_l10'] = dataset_new_lag['cpi_inflation_rate'].shift(10)
dataset_new_lag['cpi_inflation_rate_l11'] = dataset_new_lag['cpi_inflation_rate'].shift(11)
dataset_new_lag['cpi_inflation_rate_l12'] = dataset_new_lag['cpi_inflation_rate'].shift(12)

# EPU Cycle Lag
dataset_new_lag['epu_cycle_cf_l1'] = dataset_new_lag['epu_cycle_cf'].shift(1)
dataset_new_lag['epu_cycle_cf_l2'] = dataset_new_lag['epu_cycle_cf'].shift(2)
dataset_new_lag['epu_cycle_cf_l3'] = dataset_new_lag['epu_cycle_cf'].shift(3)
dataset_new_lag['epu_cycle_cf_l4'] = dataset_new_lag['epu_cycle_cf'].shift(4)
dataset_new_lag['epu_cycle_cf_l5'] = dataset_new_lag['epu_cycle_cf'].shift(5)
dataset_new_lag['epu_cycle_cf_l6'] = dataset_new_lag['epu_cycle_cf'].shift(6)
dataset_new_lag['epu_cycle_cf_l7'] = dataset_new_lag['epu_cycle_cf'].shift(7)
dataset_new_lag['epu_cycle_cf_l8'] = dataset_new_lag['epu_cycle_cf'].shift(8)
dataset_new_lag['epu_cycle_cf_l9'] = dataset_new_lag['epu_cycle_cf'].shift(9)
dataset_new_lag['epu_cycle_cf_l10'] = dataset_new_lag['epu_cycle_cf'].shift(10)
dataset_new_lag['epu_cycle_cf_l11'] = dataset_new_lag['epu_cycle_cf'].shift(11)
dataset_new_lag['epu_cycle_cf_l12'] = dataset_new_lag['epu_cycle_cf'].shift(12)

# EPU Trend Lag
dataset_new_lag['epu_trend_hp_l1'] = dataset_new_lag['epu_trend_hp'].shift(1)
dataset_new_lag['epu_trend_hp_l2'] = dataset_new_lag['epu_trend_hp'].shift(2)
dataset_new_lag['epu_trend_hp_l3'] = dataset_new_lag['epu_trend_hp'].shift(3)
dataset_new_lag['epu_trend_hp_l4'] = dataset_new_lag['epu_trend_hp'].shift(4)
dataset_new_lag['epu_trend_hp_l5'] = dataset_new_lag['epu_trend_hp'].shift(5)
dataset_new_lag['epu_trend_hp_l6'] = dataset_new_lag['epu_trend_hp'].shift(6)
dataset_new_lag['epu_trend_hp_l7'] = dataset_new_lag['epu_trend_hp'].shift(7)
dataset_new_lag['epu_trend_hp_l8'] = dataset_new_lag['epu_trend_hp'].shift(8)
dataset_new_lag['epu_trend_hp_l9'] = dataset_new_lag['epu_trend_hp'].shift(9)
dataset_new_lag['epu_trend_hp_l10'] = dataset_new_lag['epu_trend_hp'].shift(10)
dataset_new_lag['epu_trend_hp_l11'] = dataset_new_lag['epu_trend_hp'].shift(11)
dataset_new_lag['epu_trend_hp_l12'] = dataset_new_lag['epu_trend_hp'].shift(12)

# gprc_rus Cycle Lag
dataset_new_lag['gprc_rus_cycle_cf_l1'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(1)
dataset_new_lag['gprc_rus_cycle_cf_l2'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(2)
dataset_new_lag['gprc_rus_cycle_cf_l3'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(3)
dataset_new_lag['gprc_rus_cycle_cf_l4'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(4)
dataset_new_lag['gprc_rus_cycle_cf_l5'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(5)
dataset_new_lag['gprc_rus_cycle_cf_l6'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(6)
dataset_new_lag['gprc_rus_cycle_cf_l7'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(7)
dataset_new_lag['gprc_rus_cycle_cf_l8'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(8)
dataset_new_lag['gprc_rus_cycle_cf_l9'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(9)
dataset_new_lag['gprc_rus_cycle_cf_l10'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(10)
dataset_new_lag['gprc_rus_cycle_cf_l11'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(11)
dataset_new_lag['gprc_rus_cycle_cf_l12'] = dataset_new_lag['gprc_rus_cycle_cf'].shift(12)

# gprc_rus Trend Lag
dataset_new_lag['gprc_rus_trend_hp_l1'] = dataset_new_lag['gprc_rus_trend_hp'].shift(1)
dataset_new_lag['gprc_rus_trend_hp_l2'] = dataset_new_lag['gprc_rus_trend_hp'].shift(2)
dataset_new_lag['gprc_rus_trend_hp_l3'] = dataset_new_lag['gprc_rus_trend_hp'].shift(3)
dataset_new_lag['gprc_rus_trend_hp_l4'] = dataset_new_lag['gprc_rus_trend_hp'].shift(4)
dataset_new_lag['gprc_rus_trend_hp_l5'] = dataset_new_lag['gprc_rus_trend_hp'].shift(5)
dataset_new_lag['gprc_rus_trend_hp_l6'] = dataset_new_lag['gprc_rus_trend_hp'].shift(6)
dataset_new_lag['gprc_rus_trend_hp_l7'] = dataset_new_lag['gprc_rus_trend_hp'].shift(7)
dataset_new_lag['gprc_rus_trend_hp_l8'] = dataset_new_lag['gprc_rus_trend_hp'].shift(8)
dataset_new_lag['gprc_rus_trend_hp_l9'] = dataset_new_lag['gprc_rus_trend_hp'].shift(9)
dataset_new_lag['gprc_rus_trend_hp_l10'] = dataset_new_lag['gprc_rus_trend_hp'].shift(10)
dataset_new_lag['gprc_rus_trend_hp_l11'] = dataset_new_lag['gprc_rus_trend_hp'].shift(11)
dataset_new_lag['gprc_rus_trend_hp_l12'] = dataset_new_lag['gprc_rus_trend_hp'].shift(12)

# cpi_cycle_cf Lag
dataset_new_lag['cpi_cycle_cf_l1'] = dataset_new_lag['cpi_cycle_cf'].shift(1)
dataset_new_lag['cpi_cycle_cf_l2'] = dataset_new_lag['cpi_cycle_cf'].shift(2)
dataset_new_lag['cpi_cycle_cf_l3'] = dataset_new_lag['cpi_cycle_cf'].shift(3)
dataset_new_lag['cpi_cycle_cf_l4'] = dataset_new_lag['cpi_cycle_cf'].shift(4)
dataset_new_lag['cpi_cycle_cf_l5'] = dataset_new_lag['cpi_cycle_cf'].shift(5)
dataset_new_lag['cpi_cycle_cf_l6'] = dataset_new_lag['cpi_cycle_cf'].shift(6)
dataset_new_lag['cpi_cycle_cf_l7'] = dataset_new_lag['cpi_cycle_cf'].shift(7)
dataset_new_lag['cpi_cycle_cf_l8'] = dataset_new_lag['cpi_cycle_cf'].shift(8)
dataset_new_lag['cpi_cycle_cf_l9'] = dataset_new_lag['cpi_cycle_cf'].shift(9)
dataset_new_lag['cpi_cycle_cf_l10'] = dataset_new_lag['cpi_cycle_cf'].shift(10)
dataset_new_lag['cpi_cycle_cf_l11'] = dataset_new_lag['cpi_cycle_cf'].shift(11)
dataset_new_lag['cpi_cycle_cf_l12'] = dataset_new_lag['cpi_cycle_cf'].shift(12)

# cpi_trend_hp Lag
dataset_new_lag['cpi_trend_hp_l1'] = dataset_new_lag['cpi_trend_hp'].shift(1)
dataset_new_lag['cpi_trend_hp_l2'] = dataset_new_lag['cpi_trend_hp'].shift(2)
dataset_new_lag['cpi_trend_hp_l3'] = dataset_new_lag['cpi_trend_hp'].shift(3)
dataset_new_lag['cpi_trend_hp_l4'] = dataset_new_lag['cpi_trend_hp'].shift(4)
dataset_new_lag['cpi_trend_hp_l5'] = dataset_new_lag['cpi_trend_hp'].shift(5)
dataset_new_lag['cpi_trend_hp_l6'] = dataset_new_lag['cpi_trend_hp'].shift(6)
dataset_new_lag['cpi_trend_hp_l7'] = dataset_new_lag['cpi_trend_hp'].shift(7)
dataset_new_lag['cpi_trend_hp_l8'] = dataset_new_lag['cpi_trend_hp'].shift(8)
dataset_new_lag['cpi_trend_hp_l9'] = dataset_new_lag['cpi_trend_hp'].shift(9)
dataset_new_lag['cpi_trend_hp_l10'] = dataset_new_lag['cpi_trend_hp'].shift(10)
dataset_new_lag['cpi_trend_hp_l11'] = dataset_new_lag['cpi_trend_hp'].shift(11)
dataset_new_lag['cpi_trend_hp_l12'] = dataset_new_lag['cpi_trend_hp'].shift(12)

# dataset_new_lag.head()
#Create the final dataset after removing all the null values
dataset_new_lag
# making new data frame with dropped NA values
dataset_new_lag2 = dataset_new_lag.dropna(axis = 0, how ='any')
print(dataset_new_lag.shape)
print(dataset_new_lag2.shape)
############# End of Data Pre-processing: (can be ignored) Example code ##########################

########################## Train - Test data creation ######################
# Create Train - Test Data
n_obs=24
df_train, df_test = dataset_new_lag2[0:-n_obs], dataset_new_lag2[-n_obs:]
print(df_train.shape, df_test.shape)

from statsmodels.datasets.danish_data import load
from statsmodels.tsa.api import ARDL
from statsmodels.tsa.ardl import ardl_select_order

# data = load().data
data_train = df_train[["cpi_inflation_rate",
                       "epu_cycle_cf",
                       "epu_trend_hp",
                       "gprc_rus_cycle_cf",
                       "gprc_rus_trend_hp",
                       "cpi_trend_hp",
                       "cpi_cycle_cf"
                       ]]
data_train.tail()

data_test = df_test[["cpi_inflation_rate",
                       "epu_cycle_cf",
                       "epu_trend_hp",
                       "gprc_rus_cycle_cf",
                       "gprc_rus_trend_hp",
                       "cpi_trend_hp",
                       "cpi_cycle_cf"
                       ]]
data_test.tail()

print(data_test.shape)
print(data_train.shape)
###################### Model Development ############################
# Model Selection
sel_res = ardl_select_order(
    data_train.cpi_inflation_rate, 3,
    data_train[["epu_cycle_cf",
          "epu_trend_hp",
          "gprc_rus_cycle_cf",
          "gprc_rus_trend_hp",
          "cpi_trend_hp",
          "cpi_cycle_cf"
          ]], 3, ic="aic", trend="c"
)
print(f"The optimal order is: {sel_res.model.ardl_order}")

res = sel_res.model.fit()
print(res.summary())

# Use direct parametarization
res_ARDL = ARDL(
    data_train.cpi_inflation_rate,
    3,
    data_train[["epu_cycle_cf",
                "epu_trend_hp",
                "gprc_rus_cycle_cf",
                "gprc_rus_trend_hp",
                "cpi_trend_hp",
                "cpi_cycle_cf"]], {"epu_cycle_cf": 0,
                                   "epu_trend_hp": 3,
                                   "gprc_rus_trend_hp": 3,
                                   "cpi_trend_hp": 2,
                                   "cpi_cycle_cf": 2
                                   },
                trend="c"
).fit()
print(res_ARDL.summary())

data_train_new = df_train[['cpi_inflation_rate',
                          'cpi_inflation_rate_l1',
                          'cpi_inflation_rate_l2',
                          'cpi_inflation_rate_l3',
                          'epu_cycle_cf',
                          'epu_trend_hp',
                          'epu_trend_hp_l1',
                          'epu_trend_hp_l2',
                          'epu_trend_hp_l3',
                          'gprc_rus_trend_hp',
                          'gprc_rus_trend_hp_l1',
                           'gprc_rus_trend_hp_l2',
                           'gprc_rus_trend_hp_l3',
                          'gprc_rus_cycle_cf_l1',
                           'cpi_trend_hp',
                          'cpi_trend_hp_l1',
                          'cpi_trend_hp_l2',
                          'cpi_trend_hp_l3',
                           'cpi_cycle_cf',
                          'cpi_cycle_cf_l1',
                          'cpi_cycle_cf_l2'
                          ]].copy()

data_test_new = df_test[[ 'cpi_inflation_rate',
                          'cpi_inflation_rate_l1',
                          'cpi_inflation_rate_l2',
                          'cpi_inflation_rate_l3',
                          'epu_cycle_cf',
                          'epu_trend_hp',
                          'epu_trend_hp_l1',
                          'epu_trend_hp_l2',
                          'epu_trend_hp_l3',
                          'gprc_rus_trend_hp',
                          'gprc_rus_trend_hp_l1',
                           'gprc_rus_trend_hp_l2',
                           'gprc_rus_trend_hp_l3',
                          'gprc_rus_cycle_cf_l1',
                           'cpi_trend_hp',
                          'cpi_trend_hp_l1',
                          'cpi_trend_hp_l2',
                          'cpi_trend_hp_l3',
                           'cpi_cycle_cf',
                          'cpi_cycle_cf_l1',
                          'cpi_cycle_cf_l2'
                          ]].copy()

print(data_train_new.shape)
print(data_test_new.shape)

from statsmodels.tsa.api import AutoReg
import numpy as np
import pandas as pd

# 5 lags and exogenous regressors
res_ardl_1 = AutoReg(data_train_new['cpi_inflation_rate'],
              3,
              exog=data_train_new[['epu_cycle_cf',
                          'epu_trend_hp',
                          'epu_trend_hp_l1',
                          'epu_trend_hp_l2',
                          'epu_trend_hp_l3',
                          'gprc_rus_trend_hp',
                          'gprc_rus_trend_hp_l1',
                          'gprc_rus_trend_hp_l2',
                          'gprc_rus_trend_hp_l3',
                          'cpi_trend_hp',
                          'cpi_trend_hp_l1',
                          'cpi_trend_hp_l2',
                          'cpi_cycle_cf',
                          'cpi_cycle_cf_l1',
                          'cpi_cycle_cf_l2'
                          ]]).fit()
print(res_ardl_1.summary())

fcst = res_ardl_1.forecast(steps=24, exog = data_train_new[['epu_cycle_cf',
                          'epu_trend_hp',
                          'epu_trend_hp_l1',
                          'epu_trend_hp_l2',
                          'epu_trend_hp_l3',
                          'gprc_rus_trend_hp',
                          'gprc_rus_trend_hp_l1',
                          'gprc_rus_trend_hp_l2',
                          'gprc_rus_trend_hp_l3',
                          'cpi_trend_hp',
                          'cpi_trend_hp_l1',
                          'cpi_trend_hp_l2',
                          'cpi_cycle_cf',
                          'cpi_cycle_cf_l1',
                          'cpi_cycle_cf_l2'
                          ]])

fcst.values

plt.plot(list(data_test_new['cpi_inflation_rate']))
plt.plot(list(fcst))
plt.xlabel('Steps of the test data')
plt.ylabel('CPI')
plt.legend(['test', 'forecast'])
plt.show()
# r2_score(data_test_new['CPI_inflation_rate'], fcst)

# Check the Model Diagnostics
res_ardl_1.plot_diagnostics(figsize=(16, 8))
plt.show()

data_test_new['cpi_inflation_rate'].values
