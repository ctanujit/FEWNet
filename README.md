# Reproducibility Checks:
* For a complete list of packages and their versions (both R and Python), please refer to the following files [R_package_version.txt](https://github.com/ctanujit/FEWNet/blob/main/R_package_version.txt) and [Python_Package_Version.txt](https://github.com/ctanujit/FEWNet/blob/main/Python_Package_Version.txt)
* To view "Table 1: Summary Statistics of the Datasets Used in This Study" please refer to [Table_01_Summary_Statistics_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Table_01_Summary_Statistics_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To view "Table 2: Global characteristics of the economic time series under study for BRIC countries.", please refer to [Table_02_Global_Characteristics_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Table_02_Global_Characteristics_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To view "Figure 1: Wavelet coherence analysis plots of CPI inflation with log-transformed EPU (left) and CPI inflation with GPRC (right) for (a) Brazil, (b) Russia, (c) India, and (d) China.", please refer to [Figure_01_WCA_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_01_WCA_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To view "Table 3: Training data for FEWNet including target series CPI inflation (blue), and exogenous variables logtransformed EPU (red) and GPRC (green). ACF, PACF, and OLS-based CUSUM test plots of CPI inflation series of the BRIC countries.", please refer to [Table_03_ACF_PACF_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Table_03_ACF_PACF_BRIC_FEWNet.R) and [Table_03_OLS_CUSUM_test_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Table_03_OLS_CUSUM_test_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To view "Figure 2: MODWT decomposition of the CPI Inflation series for (a) Brazil, (b) Russia, (c) India, and (d) China between the period Jan-2003 and Nov-2021. In these figures, δ1, δ2, . . . , δ5 and S5 represent the details and smooth of the series generated by the MODWT-based MRA approach. The bottom chart in all the figures represents the original time series (CPI inflation) in actual frequency scale", please refer to [Figure_02_Wavelet_Decompostion_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_02_Wavelet_Decompostion_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To generate "Table 5: Performance of the proposed FEWNet model in comparison to baseline forecasting techniques for 12 months
ahead forecasts with exogenous factors EPU and GPRC (best results are made bold)." and "Table 6: Performance of the proposed FEWNet model in comparison to baseline forecasting techniques for 24 months ahead forecasts with exogenous factors EPU, and GPRC (best results are made bold).", please refer to [Table_05_06_Evaluation_Module_Comparison_FEWNet_Baselines.ipynb](https://github.com/ctanujit/FEWNet/blob/main/Table_05_06_Evaluation_Module_Comparison_FEWNet_Baselines.ipynb). To check the forecasts across various algorithms, please refer to the following dataset [Table_05_06_Forecasts_FEWNet_Baselines_12M_24M_BRIC_Comparison.xlsx](https://github.com/ctanujit/FEWNet/blob/main/dataset/Table_05_06_Forecasts_FEWNet_Baselines_12M_24M_BRIC_Comparison.xlsx). The performance evaluation summary can be found in [dataset/Figure_05_06_FEWNet_Variants_Baselines_Performance_Evaluation.xlsx](https://github.com/ctanujit/FEWNet/blob/main/dataset/Figure_05_06_FEWNet_Variants_Baselines_Performance_Evaluation.xlsx)
* To view "Figure 4: Visualization of the multiple comparisons with best (MCB) analysis for BRIC countries. In the figure, for example, FEWNet - 2.75 means that the average rank of the proposed algorithm FEWNet based on the RMSE error metric is 2.75; the same explanation applies to other algorithms.", please refer to [Figure_04_MCB_test_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_04_MCB_test_BRIC_FEWNet.R). The input dataset is located in the [mcb_test_alternative_12M_24M_paper_data.xlsx](https://github.com/ctanujit/FEWNet/blob/main/dataset/mcb_test_alternative_12M_24M_paper_data.xlsx).
* To view "Figure 5: Results of the GR test (Giacomini & Rossi Test) of forecasting ability of the FEWNet with baselines (AR (left) and XGBoost (right)) for 12 months ahead with (α=0.90) for (a) Brazil, (b) Russia, (c) India, and (d) China. Points outside the CV have been marked under the red dotted circles representing that FEWNet and the other two
top-performing benchmarks differ significantly.", please refer to [Figure_05_GR_tests_12M_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_05_GR_tests_12M_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To view "Figure 6: Results of the GR test (Giacomini & Rossi Test) of forecasting ability of the FEWNet with baselines (AR (left) and XGBoost (right)) for 24 months ahead with (α=0.90) for (a) Brazil, (b) Russia, (c) India, and (d) China.Points outside the CV have been marked under the red dotted circles representing that FEWNet and the other two
top-performing benchmarks differ significantly", please refer to [Figure_06_GR_tests_24M_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_06_GR_tests_24M_BRIC_FEWNet.R). All datasets are located in the "/FEWNet/dataset/[country]" folder. Please replace ,<country> with the corresponding country name.
* To view "Table 8: Performance comparison between the proposed FEWNet model and their variants for 12 months ahead forecasts with exogenous factors EPU and GPRC (best results are made bold)." and "Table 9: Performance comparison between the proposed FEWNet model and their variants for 24 months ahead forecasts with exogenous factors EPU and GPRC (best results are made bold).", please refer to [Table_08_09_Evaluation_Module_Comparison_FEWNet_Variants_V2.ipynb](https://github.com/ctanujit/FEWNet/blob/main/Table_08_09_Evaluation_Module_Comparison_FEWNet_Variants_V2.ipynb). The input dataset is located in the [Table_08_09_Forecasts_Wavelet_Variants_12M_24M_BRIC_FEWNet_Comparison_V2.xlsx](https://github.com/ctanujit/FEWNet/blob/main/dataset/Table_08_09_Forecasts_Wavelet_Variants_12M_24M_BRIC_FEWNet_Comparison_V2.xlsx)
* To view "Figure 7: Visualization of the MCB analysis for BRIC countries for different FEWNet variants of the wavelet filters along with the proposed FEWNet algorithm. In the figure, for example, FEWNet - 1.62 means that the average rank of the FEWNet based on the RMSE error metric is 1.62; the same explanation applies to its variants based on different wavelet filters.", please refer to [Figure_07_MCB_test_BRIC_FEWNet.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_07_MCB_test_BRIC_FEWNet.R). The input dataset can be accessed : [mcb_test_alternative_12M_24M_FEWNet_Variants.xlsx](https://github.com/ctanujit/FEWNet/blob/main/dataset/mcb_test_alternative_12M_24M_FEWNet_Variants.xlsx).
* To view "Figure 8: The plot shows the ground truth CPI inflation data (red dots), 24 months point forecasts generated by FEWNet (blue line), AR (green line), XGBoost (purple line), and the conformal prediction interval of FEWNet (yellow shaded) for BRIC Countries." and "Figure 9: The plot shows the ground truth CPI inflation data (red dots), 12 months point forecasts generated by FEWNet (blue line), AR (green line), XGBoost (purple line), and the conformal prediction interval of FEWNet (yellow shaded) for BRIC Countries", please refer to [Figure_08_09_Conformal_PIs_BRIC_FEWNet.xlsx](https://github.com/ctanujit/FEWNet/blob/main/Figure_08_09_Conformal_PIs_BRIC_FEWNet.xlsx).
* For Conformalized Prediction Intervals, and forecasts (12M and 24M) from the proposed FEWNet model along with ERM (Emperical Risk Minimization), please refer to the code module [Figure_08_09_Forecasts_ERM_ConformalPI_12M_24M_FEWNet_BRIC.R](https://github.com/ctanujit/FEWNet/blob/main/Figure_08_09_Forecasts_ERM_ConformalPI_12M_24M_FEWNet_BRIC.R)

While it is not required for the reproducibility checks:
* Example code of various baseline models can be found at [code/baseline_models](https://github.com/ctanujit/FEWNet/tree/main/code/baseline_models).
* Example codes of the FEWNet, its variants (model with different wavelet transforms) and EWNet are located in [code/FEWNet](https://github.com/ctanujit/FEWNet/tree/main/code/FEWNet)


# FEWNet: Filtered Ensemble Wavelet Neural Network

This repository is the source code for the research paper ["Forecasting CPI Inflation under Economic Policy and Geo-political Uncertainties"](https://arxiv.org/abs/2401.00249). The paper introduces a new approach called the **Filtered Ensemble Wavelet Neural Network (FEWNet)**, which can generate accurate long-term predictions for CPI inflation. The idea utilizes a maximum overlapping discrete wavelet transform on the CPI inflation data to extract high-frequency and low-frequency signals. The wavelet-transformed series and filtered exogenous variables are fed into autoregressive neural networks downstream to get the ultimate ensemble forecast. Our theoretical analysis demonstrates that FEWNet effectively minimizes the empirical risk compared to individual, fully connected neural networks. Furthermore, we provide evidence that the real-time forecasts generated by the suggested algorithm, using a rolling-window approach, are notably superior in accuracy compared to standard forecasting methods used for comparison. In addition, we utilize conformal prediction intervals to measure the level of uncertainty linked to the projections produced by the suggested method. The outstanding performance of FEWNet can be ascribed to its ability to efficiently capture non-linearities and long-range relationships in the data via its flexible architecture.

* FEWNet integrates the maximum overlapping discrete wavelet transformation (MODWT) approach, economic filtering methods including the Hodrick-Prescott (HP) filter and Christiano-Fitzgerald (CF) filter, and the autoregressive neural network with exogenous variables (ARNNx).

* The code module [economic filters](https://github.com/ctanujit/FEWNet/blob/main/code/data_analysis/economic_filters.py) contains generic code to derive the trend and cycle components using HP and CF economic filters.

* The global characteristics of the CPI Inflation, EPU, and GPRC can be derived using [global_characteristics](https://github.com/ctanujit/FEWNet/blob/main/code/data_analysis/Global_characteristics.R) code module. This R code module calculates key statistical properties like skewness, kurtosis, long-range dependency, seasonality, stationarity, and non-linearity of the above-mentioned time series.

* The application of Wavelet Coherence Analysis (WCA) for analyzing the spatio-temporal association among variables is available in [WCA](https://github.com/ctanujit/FEWNet/blob/main/code/Wavelet_Coherence_Analysis/WCA_BRIC.R). The WCA methodology offers a valuable method for examining the interconnection and simultaneous movement of two non-stationary signals in the time-frequency domain.

* The code module to perform MODWT (Maximal Overlap Discrete Wavelet Transform) - based decomposition of the CPI Inflation series for the BRIC countries is available in [MODWT](https://github.com/ctanujit/FEWNet/blob/main/code/MODWT_decomposition/MODWT_decomposition.R)

* The code modules for all the baseline models: DeepAR (python script), ARNNx (R script), NBeatsx (python script), ARFIMAx (R script), SARIMAx (python script), ARIMAx (python script), Lasso Regression (LR) (python script), ARDL (python script), MSGARCH (R script), AR (R script), RW/RWD (R script), XGBoost (python script), TFT (python script), WARIMAx (R script) are available in [baseline_models](/code/baseline_models). These code modules share a generic framework for all algorithms highlighted above. The specific model and its hyper-parameters may vary slightly for different countries and forecast horizons. Please take note of the fact that all the deep learning models like NBeats and TFT are implemented using **Darts** package, SARIMAx/ARIMAx models are implemented using __statsmodels__ and __pmdarima__ packages in Python, and XGBoost models have been implemented using __skforecast__ and __xgboost__ packages in python. The relevant libraries/packages for different algorithms are highlighted in the subsequent R/Python scripts.
  
* The overall architectural design of the algorithm for generating long-term forecasts is shared below:
![architecture_FEWNet](https://github.com/ctanujit/FEWNet/blob/main/images/architecture_FEWNet_V2.png)

* The following image illustrates the key steps for generating the forecasts using the FEWNet algorithm:
![Pseudo_Code_FEWNet](https://github.com/ctanujit/FEWNet/blob/main/images/PseudoCode_FEWNet.jpg)

* This repository contains the datasets for the CPI Inflation numbers along with the EPU and GPRC indices for the BRIC countries, and these datasets have been used in the downstream model development exercise. To access the dataset, please refer to the [dataset](https://github.com/ctanujit/FEWNet/tree/main/dataset) section in GitHub. For more details about the datasets, please refer to the paper [Forecasting CPI inflation under economic policy and geopolitical
uncertainties](https://arxiv.org/pdf/2401.00249). In our study, we consider the monthly CPI numbers for BRIC countries from 2003-01 to 2021-11 released by FRED (Federal Reserve Bank of St. Louis) [FRED - BRAZIL CPI](https://fred.stlouisfed.org/series/BRACPIALLMINMEI). 

* The source code for the proposed FEWNet model can be accessed through [FEWNet](https://github.com/ctanujit/FEWNet/blob/main/code/FEWNet/FEWNet_BRIC_12M_24M_with_ConformalPI_calc_V2.R). The proposed framework consists of two hyper-parameters $(p,k)$, where $p$ denotes the number of lagged input observations and $k$ indicates the number of nodes in the hidden layer of the proposed EWNet model. The hyper-parameter $p$ is tuned by minimizing the Symmetric Mean Absolute Percentage Error (SMAPE) metric on the validation set. The optimized values of these hyper-parameters for different economies are highlighted in the paper and can be accessed through the source code for the FEWNet. Moreover, we also share a **framework for Grid-search CV** to determine the optimal values of the hyper-parameters (HPs) at the end of the source code. Interested users can experiment with the code for different ranges of values for the HPs or other data types.

* The source code [FEWNet](https://github.com/ctanujit/FEWNet/blob/main/code/FEWNet/FEWNet_BRIC_12M_24M_with_ConformalPI_calc_V2.R) also contains a code module to derive the **conformalised prediction intervals (CPI)** for the forecasts generated by the FEWNet Model. Similarly, the CPIs can be calculated using the same code module for other models. The ground truth, forecasted values of CPI inflation series along with the CPI of the proposed FEWNet and forecasts from the __top two baseline models__ are shared below for the 24M forecast horizon:
![24M_forecasts_CPI](https://github.com/ctanujit/FEWNet/blob/main/images/CPI_conformal_PI.png)

* A generic Python code module performance evaluation and MCB test are available in [Performance_evaluation](https://github.com/ctanujit/FEWNet/blob/main/code/Performance_evaluation/evaluation.py) and  [MCB](https://github.com/ctanujit/FEWNet/blob/main/code/Performance_evaluation/MCB_test.R). The dataset used for the MCB test can be accessed in [Performance_evaluation](https://github.com/ctanujit/FEWNet/tree/main/dataset/performance_evaluation). The R code module for [Giacomini and Rossi (2010)](https://onlinelibrary.wiley.com/doi/10.1002/jae.1177) forecasting ability test is also available in [Performance_evaluation](https://github.com/ctanujit/FEWNet/blob/main/code/Performance_evaluation/GR_V2.R). The corresponding datasets for GR tests for respective countries can be accessed in [dataset](https://github.com/ctanujit/FEWNet/tree/main/dataset).

* The code module and the datasets can be referred to reproduce the tables, charts, and final performance evaluation.

* To cite this work, please use

  Sengupta, Shovon, Tanujit Chakraborty, and Sunny Kumar Singh. "Forecasting CPI inflation under economic policy and geo-political uncertainties." International Journal of Forecasting (2024).

  @article{sengupta2023forecasting,
  
  title={Forecasting CPI inflation under economic policy and geo-political uncertainties},
  
  author={Sengupta, Shovon and Chakraborty, Tanujit and Singh, Sunny Kumar},
  
  journal={International Journal of Forecasting},
  
  year={2024},
  
  note={https://www.sciencedirect.com/science/article/pii/S016920702400092X}
  
## References
* <a id="1">[1]</a> [CPI Data: Brazil](https://fred.stlouisfed.org/series/BRACPIALLMINMEI)
* <a id="2">[2]</a> [CPI Data: Russia](https://fred.stlouisfed.org/series/RUSCPIALLMINMEI)
* <a id="3">[3]</a> [CPI Data: India](https://fred.stlouisfed.org/series/INDCPIALLMINMEI)
* <a id="4">[4]</a> [CPI Data: China](https://fred.stlouisfed.org/series/CHNCPIALLMINMEI)
* <a id="5">[5]</a> [EPU Data](https://www.policyuncertainty.com/index.html)
* <a id="6">[6]</a> [GPRC Data](https://www.policyuncertainty.com/gpr.html)
* <a id="7">[7]</a> Pratap, Bhanu and Sengupta, Shovon. Rbi working paper series no. 04 Macroeconomic forecasting in India: Does machine learning hold the key to better forecasts? 2019.
* <a id="8">[8]</a> Panja, Madhurima, Chakraborty, Tanujit, Kumar, Uttam and Liu, Nan. Epicasting: An ensemble wavelet neural network for forecasting epidemics. Neural Networks, 2023.
* <a id="9">[9]</a> Herzen, Julien, et al. "Darts: User-friendly modern machine learning for time series." Journal of Machine Learning Research 23.124 (2022): 1-6.
* <a id="10">[10]</a> Garza, Federico et al. "StatsForecast: Lightning fast forecasting with statistical and econometric models." PyCon, Salt Lake City, Utah, US (2022).
* <a id="11">[11]</a> Hyndman, Rob J., and Yeasmin Khandakar. "Automatic time series forecasting: the forecast package for R." Journal of statistical software 27 (2008): 1-22.
* <a id="12">[12]</a> Di Narzo, A. F., Aznarte, J. L., & Stigler, M. (2022). Package ‘tsDyn’.

