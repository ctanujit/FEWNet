############################## Economic Filters: Christiano-Fitzgerald (CF) & Hodrick-Prescott(HP) ##################
# Code Module
import statsmodels.api as sm
import pandas as pd

def apply_filters_to_dataframe(dataframe, columns, cf=True, hp=True):
    """
    Apply Christiano Fitzgerald asymmetric random walk filter (CF) and Hodrick-Prescott filter (HP)
    to specified columns in a DataFrame.

    Parameters:
    dataframe (pd.DataFrame): The DataFrame containing the data.
    columns (list): List of column names to which filters will be applied.
    cf (bool): Apply the CF filter if True.
    hp (bool): Apply the HP filter if True.

    Returns:
    pd.DataFrame: The original DataFrame with filtered columns added.
    """

    filtered_dataframe = dataframe.copy()

    for column in columns:
        if cf:
            cf_cycle, cf_trend = sm.tsa.filters.cffilter(dataframe[column])
            filtered_dataframe[f'{column}_cycle_cf'] = cf_cycle.copy()
            filtered_dataframe[f'{column}_trend_cf'] = cf_trend.copy()
        if hp:
            hp_cycle, hp_trend = sm.tsa.filters.hpfilter(dataframe[column])
            filtered_dataframe[f'{column}_cycle_hp'] = hp_cycle.copy()
            filtered_dataframe[f'{column}_trend_hp'] = hp_trend.copy()

    return filtered_dataframe

# Example usage:
df_cpi_ind_new = pd.DataFrame({
    "CPI_inflation_Rate": [1.2, 1.5, 1.8, 2.1],
    "log_epu": [0.5, 0.7, 0.9, 1.2],
    "gprc_ind": [100, 105, 110, 115]
})

columns_to_filter = ["CPI_inflation_Rate", "log_epu", "gprc_ind"]

filtered_df = apply_filters_to_dataframe(df_cpi_ind_new, columns_to_filter)
filtered_df
