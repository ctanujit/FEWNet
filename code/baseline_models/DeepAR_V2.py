# This is an example code for the DeepAR model. The same code module can be replicated for other geographies
# Install the required packages
!pip install gluonts==0.15.1
!pip install mxnet==1.9.1
!pip install numpy==1.23.5
!pip install pandas==2.1.4

import pandas as pd
import numpy as np
import mxnet as mx
# from gluonts.model.deepar import DeepAREstimator
from gluonts.mx.model.deepar import DeepAREstimator # import DeepAREstimator from gluonts.mx.model.deepar 
# from gluonts.trainer import Trainer
from gluonts.mx.trainer import Trainer
from gluonts.dataset.common import ListDataset

# set the seed for reproducibility
np.random.seed(7)
mx.random.seed(7)

def train_and_predict_deepar(train_path, test_path):
    # Load the datasets
    train = pd.read_csv(train_path)
    test = pd.read_csv(test_path)

    # Prepare the target and features
    y_train = train[['date',
                     'cpi_inflation_rate',
                     'cpi_inflation_rate_l1',
                     'epu_cycle_cf_l1',
                     'epu_trend_hp_l1',
                     'gprc_chn_cycle_cf_l1',
                     'gprc_chn_trend_hp_l1',
                     'cpi_cycle_cf_l1',
                     'cpi_trend_hp_l1']].copy()
    y_test = test[['date',
                   'cpi_inflation_rate',
                   'cpi_inflation_rate_l1',
                   'epu_cycle_cf_l1',
                   'epu_trend_hp_l1',
                   'gprc_chn_cycle_cf_l1',
                   'gprc_chn_trend_hp_l1',
                   'cpi_cycle_cf_l1',
                   'cpi_trend_hp_l1']].copy()
    y_full_data = pd.concat([y_train, y_test], axis=0)

    # Rename the Dependent Series as 'y' series
    y_data = y_full_data[['date',
                          'cpi_inflation_rate',
                          'cpi_inflation_rate_l1',
                          'epu_cycle_cf_l1',
                          'epu_trend_hp_l1',
                          'gprc_chn_cycle_cf_l1',
                          'gprc_chn_trend_hp_l1',
                          'cpi_cycle_cf_l1',
                          'cpi_trend_hp_l1']].copy()
    y_data = y_data.rename(columns={"cpi_inflation_rate": "y"})

    # Create the Full Dataset
    y = y_data[['date',
                'y',
                'cpi_inflation_rate_l1',
                'epu_cycle_cf_l1',
                'epu_trend_hp_l1',
                'gprc_chn_cycle_cf_l1',
                'gprc_chn_trend_hp_l1',
                'cpi_cycle_cf_l1',
                'cpi_trend_hp_l1']]

    # Convert object to DateTimeStamp
    # y['date'] = pd.to_datetime(y['date'])
    y['date'] = y['date'].astype('datetime64[ns]')
    y = y.set_index('date')

    # Train dataset: cut the last window of length "prediction_length", add "target" and "start" fields
    # start = pd.Timestamp("01-01-2004", freq="M")
    start = pd.Timestamp("01-01-2004")
    train_ds = ListDataset([{
        'target': y.loc[:'2019-11-01', 'y'],
        'start': start,
        'feat_dynamic_real': y.loc[:'2019-11-01', ['cpi_inflation_rate_l1',
                                                    'epu_cycle_cf_l1',
                                                    'epu_trend_hp_l1',
                                                    'gprc_chn_cycle_cf_l1',
                                                    'gprc_chn_trend_hp_l1',
                                                    'cpi_cycle_cf_l1',
                                                    'cpi_trend_hp_l1']].values
    }], freq='M')

    

    # Test dataset: use the whole dataset, add "target" and "start" fields
    test_ds = ListDataset([{
        'target': y['y'],
        'start': start,
        'feat_dynamic_real': y.loc[:, ['cpi_inflation_rate_l1',
                                       'epu_cycle_cf_l1',
                                       'epu_trend_hp_l1',
                                       'gprc_chn_cycle_cf_l1',
                                       'gprc_chn_trend_hp_l1',
                                       'cpi_cycle_cf_l1',
                                       'cpi_trend_hp_l1']].values
    }], freq='M')

    # Build and fit model
    estimator = DeepAREstimator(
        prediction_length=24,
        context_length=180,
        freq='M',
        trainer=Trainer(epochs=5,
                        learning_rate=1e-3,
                        num_batches_per_epoch=100)
    )
    predictor = estimator.train(train_ds)

    # Get Predictions/Forecasts for 24M forecast horizon
    predictions = predictor.predict(test_ds)
    predictions = list(predictions)[0]
    predictions = predictions.quantile(0.5)

    return predictions

# Example usage:
# Set the working directory
import os
os.chdir("/content/FEWNet/dataset/china")

train_file_path = './df_train_cpi_chn_lag_all_24M_R.csv'
test_file_path = './df_test_cpi_chn_lag_all_24M_R.csv'
result = train_and_predict_deepar(train_file_path, test_file_path)
print(result)
