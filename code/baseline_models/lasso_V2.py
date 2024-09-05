# This is an example code for the Lasso (LR) model. The same code module can be replicated for other geographies and other forecast horizons
# Install relevant packages
# !pip install skforecast==0.1.9
# !pip install scikit-learn==1.5.1

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import Lasso
from skforecast.ForecasterAutoregMultiOutput import ForecasterAutoregMultiOutput
from skforecast.model_selection import grid_search_forecaster
import warnings

# Configuration
warnings.filterwarnings('ignore')

def generate_forecasts_lasso(train_csv_path, test_csv_path, exog_variables):
    # Load train and test data
    train = pd.read_csv(train_csv_path)
    test = pd.read_csv(test_csv_path)

    # Prepare the dataset by selecting the required columns
    y_train = train[['date',
                     'CPI_inflation_Rate',
                     'CPI_inflation_Rate_l1',
                     'epu_cycle_cf_l1',
                     'epu_trend_hp_l1',
                     'gprc_ind_cycle_cf_l1',
                     'gprc_ind_trend_hp_l1',
                     'cpi_cycle_cf_l1',
                     'cpi_trend_hp_l1']].copy()

    y_test = test[['date',
                   'CPI_inflation_Rate',
                   'CPI_inflation_Rate_l1',
                   'epu_cycle_cf_l1',
                   'epu_trend_hp_l1',
                   'gprc_ind_cycle_cf_l1',
                   'gprc_ind_trend_hp_l1',
                   'cpi_cycle_cf_l1',
                   'cpi_trend_hp_l1']].copy()

    # Concatenate train and test data
    y_full_data = pd.concat([y_train, y_test], axis=0)
    print(y_train.shape)
    print(y_test.shape)
    print(y_full_data.shape)

    # Rename the target column for clarity
    y_data = y_full_data.rename(columns={"CPI_inflation_Rate": "y"})

    # Set the date as the index and ensure proper datetime format
    y_data['date'] = y_data['date'].astype('datetime64[ns]')
    y_data.set_index('date', inplace=True)

    # Split train, validation, and test datasets based on dates
    end_train = '2018-12-01'
    end_validation = '2019-11-01'
    data_train = y_data.loc[:end_train, :]
    data_val = y_data.loc[end_train:end_validation, :]
    data_test = y_data.loc[end_validation:, :]

    print(f"Dates train      : {data_train.index.min()} --- {data_train.index.max()}  (n={len(data_train)})")
    print(f"Dates validation : {data_val.index.min()} --- {data_val.index.max()}  (n={len(data_val)})")
    print(f"Dates test       : {data_test.index.min()} --- {data_test.index.max()}  (n={len(data_test)})")

    # Select exogenous variables
    exog_train = data_train[exog_variables]
    exog_test = data_test[exog_variables]

    # Ensure that exog data has enough rows for predictions
    print(f"Exogenous variable shape: {exog_train.shape}")
    
    # Convert exogenous variables from DataFrame to NumPy array for compatibility with Forecaster
    exog_train_np = exog_train.values  # Convert to NumPy array
    exog_test_np = exog_test.values  # Convert to NumPy array

    # Create the forecaster model
    forecaster = ForecasterAutoregMultiOutput(
        regressor=make_pipeline(StandardScaler(), Lasso(random_state=123)),
        steps=24,  # Number of steps to forecast
        lags=6  # Number of lags to consider
    )

    # Grid search for hyperparameters
    param_grid = {'lasso__alpha': np.logspace(-5, 5, 10)}
    lags_grid = [3, 6, 12]

    results_grid = grid_search_forecaster(
        forecaster=forecaster,
        y=data_train['y'],  # Target variable
        exog=exog_train_np,  # Exogenous variables as NumPy array
        param_grid=param_grid,
        lags_grid=lags_grid,
        steps=24,  # Number of steps to predict
        metric='mean_squared_error',
        initial_train_size=int(len(data_train)*0.90),
        return_best=True,
        verbose=False
    )

    # Predictions
    available_steps = min(len(exog_train_np), 24)  # Ensure we don't exceed available rows
    predictions = forecaster.predict(steps=available_steps, exog=exog_train_np[:available_steps])

    return predictions

# Example usage:
# Set the working directory
import os
os.chdir("/content/FEWNet/dataset/india")

# Example usage
train_csv_path = 'df_train_cpi_ind_lag_all_24M_R.csv'
test_csv_path = 'df_test_cpi_ind_lag_all_24M_R.csv'
exog_variables = ['epu_cycle_cf_l1',
                  'epu_trend_hp_l1',
                  'gprc_ind_cycle_cf_l1',
                  'gprc_ind_trend_hp_l1',
                  'cpi_cycle_cf_l1',
                  'cpi_trend_hp_l1']

# Generate forecasts
predictions = generate_forecasts_lasso(train_csv_path, test_csv_path, exog_variables)
print(predictions)
