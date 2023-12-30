################### Performance Evaluation: Different Metrics ###################

# Alternative Presentation of the code Module
import numpy as np
import pandas as pd
from sklearn.metrics import mean_squared_log_error, r2_score

# EPSILON = 1e-10

class CustomMetrics:

  EPSILON = 1e-10

  @staticmethod
  def _error(actual: np.ndarray, predicted: np.ndarray):
    """ Simple error """
    return actual - predicted

  @staticmethod
  def _percentage_error(actual: np.ndarray, predicted: np.ndarray):
    """
    Percentage error
    Note: result is NOT multiplied by 100
    """
    return _error(actual, predicted) / (actual + EPSILON)

  @staticmethod
  def _naive_forecasting(actual, seasonality=1):
    """ Naive forecasting method which just repeats previous samples """
    return actual[:-seasonality]

  @staticmethod
  def _relative_error(actual, predicted, benchmark=None):
    """ Relative Error """
    if benchmark is None or isinstance(benchmark, int):
      # If no benchmark prediction provided - use naive forecasting
      if not isinstance(benchmark, int):
        seasonality = 1
      else:
        seasonality = benchmark
      return CustomMetrics._error(actual[seasonality:], predicted[seasonality:]) / \
                  (CustomMetrics._error(actual[seasonality:], CustomMetrics._naive_forecasting(actual, seasonality)) +
                  CustomMetrics.EPSILON)
    return CustomMetrics._error(actual, predicted) / (CustomMetrics._error(actual, benchmark) + CustomMetrics.EPSILON)

  @staticmethod
  def _bounded_relative_error(actual, predicted, benchmark=None):
    """ Bounded Relative Error """
    if benchmark is None or isinstance(benchmark, int):
      # If no benchmark prediction provided - use naive forecasting
      if not isinstance(benchmark, int):
        seasonality = 1
      else:
        seasonality = benchmark
        abs_err = np.abs(CustomMetrics._error(actual[seasonality:], predicted[seasonality:]))
        abs_err_bench = np.abs(CustomMetrics._error(actual[seasonality:], CustomMetrics._naive_forecasting(actual, seasonality)))
    else:
      abs_err = np.abs(CustomMetrics._error(actual, predicted))
      abs_err_bench = np.abs(CustomMetrics._error(actual, benchmark))

    return abs_err / (abs_err + abs_err_bench + CustomMetrics.EPSILON)

  @staticmethod
  def _geometric_mean(a, axis=0, dtype=None):
    """ Geometric mean """
    if not isinstance(a, np.ndarray):
      # if not an ndarray object attempt to convert it
      log_a = np.log(np.array(a, dtype=dtype))
    elif dtype:
      # Must change the default dtype allowing array type
      if isinstance(a, np.ma.MaskedArray):
        log_a = np.log(np.ma.asarray(a, dtype=dtype))
      else:
        log_a = np.log(np.asarray(a, dtype=dtype))
    else:
        log_a = np.log(a)
    return np.exp(log_a.mean(axis=axis))

  @staticmethod
  def rmsle(y_true, y_pred):
    """
    The Root Mean Squared Log Error (RMSLE) metric
    :param y_true: The ground truth labels given in the dataset/actual values
    :param y_pred: Our predictions/forecasts
    :return: The RMSLE score --> Evaluation metric
    """
    return np.sqrt(mean_squared_log_error(y_true, y_pred))

  @staticmethod
  def mse(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Squared Error """
    return np.mean(np.square(_error(actual, predicted)))

  @staticmethod
  def rmse(actual: np.ndarray, predicted: np.ndarray):
    """ Root Mean Squared Error """
    return np.sqrt(mse(actual, predicted))

  @staticmethod
  def nrmse(actual: np.ndarray, predicted: np.ndarray):
    """ Normalized Root Mean Squared Error """
    return rmse(actual, predicted) / (actual.max() - actual.min())

  @staticmethod
  def me(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Error """
    return np.mean(_error(actual, predicted))

  @staticmethod
  def r2_scr(actual:np.ndarray, predicted: np.ndarray):
    """Calculating the r2 value: perasonian correlation coefficient"""
    return metrics.r2_score(actual, predicted)

  @staticmethod
  def mae(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Absolute Error """
    return np.mean(np.abs(_error(actual, predicted)))


  mad = mae  # Mean Absolute Deviation (it is the same as MAE)

  @staticmethod
  def gmae(actual: np.ndarray, predicted: np.ndarray):
    """ Geometric Mean Absolute Error """
    return _geometric_mean(np.abs(_error(actual, predicted)))

  @staticmethod
  def mdae(actual: np.ndarray, predicted: np.ndarray):
    """ Median Absolute Error """
    return np.median(np.abs(_error(actual, predicted)))

  @staticmethod
  def mpe(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Percentage Error """
    return np.mean(_percentage_error(actual, predicted))

  @staticmethod
  def mape(actual: np.ndarray, predicted: np.ndarray):
    """
    Mean Absolute Percentage Error
    Properties:
        + Easy to interpret
        + Scale independent
        - Biased, not symmetric
        - Undefined when actual[t] == 0
    Note: result is NOT multiplied by 100
    """
    return np.mean(np.abs(_percentage_error(actual, predicted)))

  @staticmethod
  def mdape(actual: np.ndarray, predicted: np.ndarray):
    """
    Median Absolute Percentage Error
    Note: result is NOT multiplied by 100
    """
    return np.median(np.abs(_percentage_error(actual, predicted)))

  @staticmethod
  def smape(actual: np.ndarray, predicted: np.ndarray):
    """
    Symmetric Mean Absolute Percentage Error
    Note: result is NOT multiplied by 100
    """
    return np.mean(2.0 * np.abs(actual - predicted) / ((np.abs(actual) + np.abs(predicted)) + EPSILON))

  @staticmethod
  def smdape(actual: np.ndarray, predicted: np.ndarray):
    """
    Symmetric Median Absolute Percentage Error
    Note: result is NOT multiplied by 100
    """
    return np.median(2.0 * np.abs(actual - predicted) / ((np.abs(actual) + np.abs(predicted)) + EPSILON))

  @staticmethod
  def maape(actual: np.ndarray, predicted: np.ndarray):
    """
    Mean Arctangent Absolute Percentage Error
    Note: result is NOT multiplied by 100
    """
    return np.mean(np.arctan(np.abs((actual - predicted) / (actual + EPSILON))))

  @staticmethod
  def mase(actual: np.ndarray, predicted: np.ndarray, seasonality: int = 1):
    """
    Mean Absolute Scaled Error
    Baseline (benchmark) is computed with naive forecasting (shifted by @seasonality)
    """
    return mae(actual, predicted) / mae(actual[seasonality:], _naive_forecasting(actual, seasonality))

  @staticmethod
  def std_ae(actual: np.ndarray, predicted: np.ndarray):
    """ Normalized Absolute Error """
    __mae = mae(actual, predicted)
    return np.sqrt(np.sum(np.square(_error(actual, predicted) - __mae))/(len(actual) - 1))

  @staticmethod
  def std_ape(actual: np.ndarray, predicted: np.ndarray):
    """ Normalized Absolute Percentage Error """
    __mape = mape(actual, predicted)
    return np.sqrt(np.sum(np.square(_percentage_error(actual, predicted) - __mape))/(len(actual) - 1))

  @staticmethod
  def rmspe(actual: np.ndarray, predicted: np.ndarray):
    """
    Root Mean Squared Percentage Error
    Note: result is NOT multiplied by 100
    """
    return np.sqrt(np.mean(np.square(_percentage_error(actual, predicted))))

  @staticmethod
  def rmdspe(actual: np.ndarray, predicted: np.ndarray):
    """
    Root Median Squared Percentage Error
    Note: result is NOT multiplied by 100
    """
    return np.sqrt(np.median(np.square(_percentage_error(actual, predicted))))

  @staticmethod
  def rmsse(actual: np.ndarray, predicted: np.ndarray, seasonality: int = 1):
    """ Root Mean Squared Scaled Error """
    q = np.abs(_error(actual, predicted)) / mae(actual[seasonality:], _naive_forecasting(actual, seasonality))
    return np.sqrt(np.mean(np.square(q)))

  @staticmethod
  def inrse(actual: np.ndarray, predicted: np.ndarray):
    """ Integral Normalized Root Squared Error """
    return np.sqrt(np.sum(np.square(_error(actual, predicted))) / np.sum(np.square(actual - np.mean(actual))))

  @staticmethod
  def rrse(actual: np.ndarray, predicted: np.ndarray):
    """ Root Relative Squared Error """
    return np.sqrt(np.sum(np.square(actual - predicted)) / np.sum(np.square(actual - np.mean(actual))))

  @staticmethod
  def mre(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Mean Relative Error """
    return np.mean(_relative_error(actual, predicted, benchmark))

  @staticmethod
  def rae(actual: np.ndarray, predicted: np.ndarray):
    """ Relative Absolute Error (aka Approximation Error) """
    return np.sum(np.abs(actual - predicted)) / (np.sum(np.abs(actual - np.mean(actual))) + EPSILON)

  @staticmethod
  def mrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Mean Relative Absolute Error """
    return np.mean(np.abs(_relative_error(actual, predicted, benchmark)))

  @staticmethod
  def mdrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Median Relative Absolute Error """
    return np.median(np.abs(_relative_error(actual, predicted, benchmark)))

  @staticmethod
  def gmrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Geometric Mean Relative Absolute Error """
    return _geometric_mean(np.abs(_relative_error(actual, predicted, benchmark)))

  @staticmethod
  def mbrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Mean Bounded Relative Absolute Error """
    return np.mean(_bounded_relative_error(actual, predicted, benchmark))

  @staticmethod
  def umbrae(actual: np.ndarray, predicted: np.ndarray, benchmark: np.ndarray = None):
    """ Unscaled Mean Bounded Relative Absolute Error """
    __mbrae = mbrae(actual, predicted, benchmark)
    return __mbrae / (1 - __mbrae)

  @staticmethod
  def mda(actual: np.ndarray, predicted: np.ndarray):
    """ Mean Directional Accuracy """
    return np.mean((np.sign(actual[1:] - actual[:-1]) == np.sign(predicted[1:] - predicted[:-1])).astype(int))

  # https://www.kaggle.com/carlolepelaars/understanding-the-metric-rmsle
  @staticmethod
  def rmsle(y_true:np.ndarray, y_pred:np.ndarray):
    """
        The Root Mean Squared Log Error (RMSLE) metric

        :param y_true: The ground truth labels given in the dataset/actual values
        :param y_pred: Our predictions/forecasts
        :return: The RMSLE score --> Evaluation metric
    """
    return np.sqrt(mean_squared_log_error(y_true, y_pred))

  @staticmethod
  def theils_u1(f:np.ndarray , y:np.ndarray):
    df = pd.DataFrame({'f_i':f, 'y_i': y})
    df['(f_i - y_i)^2'] = np.square(df['f_i'] - df['y_i'])
    df['y_i^2'] = np.square(df['y_i'])
    df['f_i^2'] = np.square(df['f_i'])
    return (np.sqrt(np.mean(df['(f_i - y_i)^2'])))/(np.sqrt(np.mean(df['y_i^2']))+np.sqrt(np.mean(df['f_i^2'])))

  METRICS = {
      # 'mse': mse,
      'rmse': rmse,
      # 'nrmse': nrmse,
      # 'me': me,
      # 'mae': mae,
      # 'mad': mad,
      # 'gmae': gmae,
      # 'mdae': mdae,
      # 'mpe': mpe,
      # 'mape': mape,
      'mdape': mdape,
      'smape': smape,
      # 'smdape': smdape,
      # 'maape': maape,
      'mase': mase,
      # 'std_ae': std_ae,
      # 'std_ape': std_ape,
      # 'rmspe': rmspe,
      # 'rmdspe': rmdspe,
      # 'rmsse': rmsse,
      # 'inrse': inrse,
      # 'rrse': rrse,
      # 'mre': mre,
      # 'rae': rae,
      # 'r2_scr': r2_scr,
      # 'mrae': mrae,
      'mdrae': mdrae,
      # 'gmrae': gmrae,
      # 'mbrae': mbrae,
      # 'umbrae': umbrae,
      # 'mda': mda,
      # 'rmsle': rmsle,
      'theils_u1': theils_u1
  }

  @staticmethod
  def evaluate(actual, predicted, metrics=('mae', 'mse', 'smape', 'umbrae')):
    results = {}
    for name in metrics:
      try:
        results[name] = CustomMetrics.METRICS[name](actual, predicted)
      except Exception as err:
        results[name] = np.nan
        print('Unable to compute metric {0}: {1}'.format(name, err))
    return results

  @staticmethod
  def evaluate_all(actual, predicted):
    return CustomMetrics.evaluate(actual, predicted, metrics=set(CustomMetrics.METRICS.keys()))


# Example usage:
actual = np.array([4.306152, 4.191771, 4.005114, 3.303158, 2.399279, 1.877727, 2.132417, 2.305625, 2.438465, 3.135329,
                   3.918350, 4.311223, 4.517457, 4.559198, 5.195379, 6.099479, 6.759116, 8.056819, 8.347072, 8.994823,
                   9.679774, 10.246209, 10.672622, 10.738501])

TFTX_multiexog_7 = np.array([3.63315753442842, 5.12170744345272, 4.89672967381515, 4.96016798959929, 5.85757030430114,
                             5.44922027786225, 6.56799182390867, 6.13146461660933, 5.58974159956751, 6.81890408079133,
                             6.55276039976514, 6.45109167403215, 6.43984768562119, 6.40542650978144, 5.37913271220488,
                             5.91492552423069, 5.44590132351503, 5.28661890267048, 5.31431174609325, 4.44724601311194,
                             5.08895407944217, 4.0286948007462, 3.52093641113502, 3.62811658712853])

results = CustomMetrics.evaluate_all(actual, TFTX_multiexog_7)
print("Custom Evaluation Metrics:")
for metric, value in results.items():
    print(f"{metric}: {value}")
