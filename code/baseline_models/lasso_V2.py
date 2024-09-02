# This is an example code for the Lasso (LR) model. The same code module can be replicated for other geographies and other forecast horizons

# Data processing
# ==============================================================================
import numpy as np
import pandas as pd

# Plots
# ==============================================================================
import matplotlib.pyplot as plt
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.graphics.tsaplots import plot_pacf
import plotly.express as px
plt.style.use('fivethirtyeight')
plt.rcParams['lines.linewidth'] = 1.5
%matplotlib inline

# Modelling and Forecasting
# ==============================================================================
# from xgboost import XGBRegressor
# from lightgbm import LGBMRegressor
# from catboost import CatBoostRegressor

from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import Lasso

from skforecast.ForecasterAutoreg import ForecasterAutoreg
from skforecast.ForecasterAutoregMultiOutput import ForecasterAutoregMultiOutput
from skforecast.model_selection import grid_search_forecaster
from skforecast.model_selection import backtesting_forecaster

from joblib import dump, load

# Configuration
# ==============================================================================
import warnings
warnings.filterwarnings('ignore')
%config Completer.use_jedi = False

def generate_forecasts_lasso(train_csv_path,test_csv_path, exog_variables):
    # Load train and test data
    train = pd.read_csv(train_csv_path)
    test = pd.read_csv(test_csv_path)

    # Create a full dataset using train and test variables
    y_train = train[['date',
                 'cpi_inflation_rate',
                 'cpi_inflation_rate_l1',
                 'epu_cycle_cf_l1',
                 'epu_trend_hp_l1',
                 'gprc_rus_cycle_cf_l1',
                 'gprc_rus_trend_hp_l1',
                 'cpi_cycle_cf_l1',
                 'cpi_trend_hp_l1']].copy()

    y_test = test[['date',
                 'cpi_inflation_rate',
                 'cpi_inflation_rate_l1',
                 'epu_cycle_cf_l1',
                 'epu_trend_hp_l1',
                 'gprc_rus_cycle_cf_l1',
                 'gprc_rus_trend_hp_l1',
                 'cpi_cycle_cf_l1',
                 'cpi_trend_hp_l1']].copy()

    y_full_data = pd.concat([y_train, y_test], axis=0)
    print(y_train.shape)
    print(y_test.shape)
    print(y_full_data.shape)

    # Rename columns
    y_data = y_full_data[['date',
                 'cpi_inflation_rate',
                 'cpi_inflation_rate_l1',
                 'epu_cycle_cf_l1',
                 'epu_trend_hp_l1',
                 'gprc_rus_cycle_cf_l1',
                 'gprc_rus_trend_hp_l1',
                 'cpi_cycle_cf_l1',
                 'cpi_trend_hp_l1']].copy()

    y_data = y_data.rename(columns={"cpi_inflation_rate": "y"})

    y = y_data[['date',
            'y',
            'cpi_inflation_rate_l1',
            'epu_cycle_cf_l1',
            'epu_trend_hp_l1',
            'gprc_rus_cycle_cf_l1',
            'gprc_rus_trend_hp_l1',
            'cpi_cycle_cf_l1',
            'cpi_trend_hp_l1']].copy()
    # y = y.reset_index(drop=False)
    y.columns = ['date',
             'y',
            'cpi_inflation_rate_l1',
            'epu_cycle_cf_l1',
            'epu_trend_hp_l1',
            'gprc_rus_cycle_cf_l1',
            'gprc_rus_trend_hp_l1',
            'cpi_cycle_cf_l1',
            'cpi_trend_hp_l1']
    # Convert object to DateTimeStamp
    y['date'] = y['date'].astype('datetime64[ns]')
    y = y.set_index('date')

    # Split train-val-test
    end_train = '2018-12-01'
    end_validation = '2019-11-01'
    data_train = y.loc[:end_train, :]
    data_val = y.loc[end_train:end_validation, :]
    data_test = y.loc[end_validation:, :]

    print(f"Dates train      : {data_train.index.min()} --- {data_train.index.max()}  (n={len(data_train)})")
    print(f"Dates validation : {data_val.index.min()} --- {data_val.index.max()}  (n={len(data_val)})")
    print(f"Dates test       : {data_test.index.min()} --- {data_test.index.max()}  (n={len(data_test)})")

    # Select exogenous variables
    exog_variables = [column for column in y.columns if column.startswith(exog_variables)]

    # Create forecaster
    forecaster = ForecasterAutoregMultiOutput(
                    regressor = make_pipeline(StandardScaler(), Lasso(random_state=123)),
                    steps     = 24,
                    lags      = 6
             )

    forecaster

    # Grid search of hyperparameters and lags
    param_grid = {'lasso__alpha': np.logspace(-5, 5, 10)}
    lags_grid = [3, 6, 12]

    results_grid = grid_search_forecaster(
                        forecaster         = forecaster,
                        y                  = y_train['cpi_inflation_rate'],
                        exog               = y_train[exog_variables],
                        param_grid         = param_grid,
                        lags_grid          = lags_grid,
                        steps              = 24,
                        refit              = False,
                        metric             = 'mean_squared_error',
                        initial_train_size = int(len(data_train)*0.90),
                        fixed_train_size   = False,
                        return_best        = True,
                        verbose            = False
                        )

    # Predictions
    predictions = forecaster.predict(steps=24, exog=y_train[exog_variables])
    return predictions.values

# Example usage:
# Set the working directory
import os
os.chdir("/content/FEWNet/dataset/brazil")

train_csv_path = 'df_train_cpi_ind_lag_all_24M_R.csv'
test_csv_path = 'df_test_cpi_bzl_lag_all_24M_R.csv'
exog_variables = ['epu_cycle_cf_l1',
                  'epu_trend_hp_l1',
                  'gprc_rus_cycle_cf_l1',
                  'gprc_rus_trend_hp_l1',
                  'cpi_cycle_cf_l1',
                  'cpi_trend_hp_l1']
generate_forecasts_lasso(train_csv_path,test_csv_path, exog_variables)
