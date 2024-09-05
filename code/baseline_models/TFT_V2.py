# This is an example code for the TFT model. The same code module can be replicated for other geographies
# Install the required package
!pip install darts
# Import the required libraries
import pandas as pd
import numpy as np
import torch
from darts import TimeSeries
from darts.models import TFTModel

def generate_tft_forecasts(train_csv_path, test_csv_path, output_csv_path):
    # Check for GPU availability
    gpu_info = !nvidia-smi
    gpu_info = '\n'.join(gpu_info)
    if gpu_info.find('failed') >= 0:
        print('Select the Runtime > "Change runtime type" menu to enable a GPU accelerator, ')
        print('and then re-execute this cell.')
    else:
        print(gpu_info)

    # Load training and test data
    train = pd.read_csv(train_csv_path)
    test = pd.read_csv(test_csv_path)

    # Extract target and exogenous variables
    train_target_ts = TimeSeries.from_series(train["CPI_inflation_rate"])
    train_cpi_lag = TimeSeries.from_series(train["CPI_inflation_rate_l1"])
    train_epu_cyc = TimeSeries.from_series(train["epu_cycle_cf_l1"])
    train_epu_trend = TimeSeries.from_series(train["epu_trend_hp_l1"])
    train_grpc_cyc = TimeSeries.from_series(train["gprc_ind_cycle_cf_l1"])
    train_grpc_trend = TimeSeries.from_series(train["gprc_ind_trend_hp_l1"])
    train_cpi_cyc = TimeSeries.from_series(train["cpi_cycle_cf_l1"])
    train_cpi_trend = TimeSeries.from_series(train["cpi_trend_hp_l1"])

    train_exogenous = train_cpi_lag.stack(train_epu_cyc).stack(train_epu_trend).stack(train_grpc_cyc).stack(train_grpc_trend).stack(train_cpi_cyc).stack(train_cpi_trend)

    test_target_ts = TimeSeries.from_series(test["CPI_inflation_rate"])

    # Create TFT model
    model = TFTModel(
        input_chunk_length=48,
        output_chunk_length=24,
        hidden_size=12,
        lstm_layers=3,
        num_attention_heads=12,
        full_attention=True,
        dropout=0.001,
        hidden_continuous_size=4,
        n_epochs=500,
        random_state=0,
        add_relative_index=True
    )

    # Fit the model to the training data
    model.fit(
        series=train_target_ts,
        past_covariates=train_exogenous,
        verbose=True,
    )

    # Generate forecasts for the test data
    pred = model.predict(n=24, series=train_target_ts, past_covariates=train_exogenous)

    # Save the forecasts to a CSV file
    pred.to_csv(output_csv_path)

    return pred

# Example usage:
# Set the working directory
import os
os.chdir("/content/FEWNet/dataset/brazil")

train_csv_path = 'df_train_cpi_bzl_lag_all_24M_R.csv'
test_csv_path = 'df_test_cpi_bzl_lag_all_24M_R.csv'
output_csv_path = 'brazil_6.csv'
generate_tft_forecasts(train_csv_path, test_csv_path, output_csv_path)
