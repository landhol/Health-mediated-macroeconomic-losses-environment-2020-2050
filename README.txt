README
======

Title
-------------
Health-augmented macroeconomic estimates and projections of health-mediated economic losses of environmental risks from 2020 to 2050

Repository purpose
------------------
This repository contains the cleaned R code used to estimate the projected health-mediated
macroeconomic burden attributable to three environmental risks:

1. Air pollution
2. Unsafe water, sanitation, and handwashing (UWSH)
3. Non-optimal temperature

The analysis covers 204 countries and territories over 2020-2050. The workflow combines
GBD 2021 epidemiological estimates with a health-augmented macroeconomic model incorporating
effective labor supply, human capital, physical capital accumulation, treatment expenditure,
and aggregate production.

Methodological overview
-----------------------
The primary attribution procedure has two stages.

First, disease-specific macroeconomic losses are estimated by comparing:
- a status-quo scenario; and
- a counterfactual scenario in which the entire disease is eliminated.

The cumulative difference in aggregate output between these two scenarios over 2020-2050
is the disease-specific macroeconomic burden.

Second, the corresponding risk-specific population-attributable fraction (PAF) is applied
once to the disease-specific macroeconomic burden to estimate the share attributable to
air pollution, UWSH, or non-optimal temperature.

Thus, the PAF is applied exactly once, at the disease-level macroeconomic-burden stage.

The model includes:
- mortality-related changes in the surviving population and labor force;
- morbidity-related changes in effective labor supply;
- human capital estimated using a Mincer earnings function;
- disease-specific treatment expenditure;
- physical capital accumulation through saving and depreciation; and
- a Cobb-Douglas production function.

Primary lower and upper bounds are propagated from the corresponding GBD epidemiological
lower and upper estimates through the model. These are separate from the Monte Carlo-based
economic-parameter sensitivity analysis described below.


Repository files and execution order
------------------------------------
Run the main-analysis scripts in the following order.

01. 1_Mortality_morbidity_UI_clean.R
    Projects disease-specific mortality, YLL, and YLD inputs through 2050 and prepares
    point, lower, and upper epidemiological estimates.

02. 2_population_UI_clean.R
    Constructs population trajectories and disease-elimination counterfactual population
    estimates.

03. 3_Labor_participation_UI_clean.R
    Constructs disease-elimination counterfactual labor-force participation using
    morbidity-related inputs.

04. 4_Aggregate_human_capital_Ht_clean.R
    Calculates aggregate human capital for the status-quo and disease-elimination
    counterfactual scenarios using educational attainment and the Mincer function.

05. 5_treatment_cost_clean.R
    Estimates disease-specific treatment expenditure for 2019-2050.

06. 6_Y_new_clean.R
    Reconstructs physical capital, calibrates total factor productivity, simulates
    disease-elimination counterfactual output, and calculates cumulative disease-specific
    macroeconomic burden over 2020-2050.

07. 7_imputation_clean.R
    Imputes missing country-level disease-specific macroeconomic burdens using the
    relationship between burden as a share of cumulative GDP and projected DALY rates.

08. 8_PAF_main_analysis_clean.R
    Applies GBD 2021 PAFs to disease-specific macroeconomic burdens for air pollution,
    UWSH, and non-optimal temperature.

09. 9_table_main_analysis_clean.R
    Generates country-, regional-, income-group-, risk-subcategory-, and disease-specific
    summary tables.

10. 10_by_country_map_main_analysis_clean.R
    Generates country-level maps of absolute burden, burden as a share of GDP, and selected
    leading disease contributors.

11. 11_heatmap_main_analysis_clean.R
    Generates disease-specific heatmaps for air pollution and non-optimal temperature.

12. 12_monte_carlo_sensitivity_clean.R
    Runs the 1,000-simulation Monte Carlo-based economic-parameter sensitivity analysis
    and generates sensitivity-analysis outputs for the three environmental risks.

The sensitivity-analysis script should be run after the upstream base inputs used by the
main-analysis workflow are available.


Software environment
--------------------
R version: 4.2.1

Directly used R packages and versions:
- dplyr 1.1.4
- data.table 1.14.10
- readxl 1.4.3
- tidyr 1.3.0
- progress 1.2.3
- ggplot2 3.4.4
- patchwork 1.1.3
- sf 1.0-20
- future 1.33.0
- future.apply 1.11.1
- progressr 0.14.0

Users wishing to reproduce the analysis in a new environment should install compatible
versions of these packages before running the scripts.


Data sources
------------
All source datasets listed below were downloaded on 2026-02-06 unless otherwise stated.

1. Gross domestic product
   Source: Institute for Health Metrics and Evaluation (IHME)
   DOI: 10.6069/HHKW-4F29

2. Physical capital stock and capital share
   Source: Penn World Table version 10.0.1
   Website: https://www.rug.nl/ggdc/productivity/pwt/

3. Saving rates and health expenditure
   Source: World Bank Group, World Development Indicators
   Website: https://databank.worldbank.org/source/world-development-indicators

4. Labor-force participation rates by 5-year age-sex group
   Source: International Labour Organization
   Website: https://www.ilo.org/global/statistics-and-databases/lang--en/index.htm
   Exact dataset/version: not separately recorded in the current repository metadata.

5. Population projections
   Source: United Nations, Department of Economic and Social Affairs
   Website: https://population.un.org/wpp/Download/Standard/Population/
   Exact World Population Prospects release/version: not separately recorded in the current
   repository metadata.

6. Age-specific educational attainment
   Source: Barro-Lee Educational Attainment Dataset
   Website: https://barrolee.github.io/BarroLeeDataSet/
   Exact release/version: not separately recorded in the current repository metadata.

7. Epidemiological estimates and PAFs
   Source: Global Burden of Disease Study 2021
   Website: https://vizhub.healthdata.org/gbd-results

   GBD inputs used across the workflow include mortality, YLLs, YLDs, DALYs, PAFs, and
   additional disease-frequency inputs used by selected scripts. Users reproducing the
   analysis should retain the original GBD export metadata associated with each downloaded
   file.

8. U.S. disease-specific treatment expenditure
   Source:
   Dieleman JL, Cao J, Chapin A, Chen C, Li Z, Liu A, et al.
   US Health Care Spending by Payer and Health Condition, 1996-2016.
   JAMA. 2020;323(9):863.
   DOI: 10.1001/jama.2020.0734

9. Income-group and regional classification
   Input file: CLASS.xlsx
   Source/version: [TO FILL: exact source and classification version before public release]
   Download date: 2026-02-06


Treatment-cost calculation
--------------------------
For each included disease, U.S. disease-specific treatment expenditure is taken from
Dieleman et al. (2020). For non-U.S. countries, disease-specific treatment expenditure is
scaled using national health expenditure and the country-to-U.S. disease prevalence ratio.
Future disease-specific treatment expenditure is projected using growth in per-capita
health expenditure.


Population-attributable fractions
---------------------------------
The main analysis uses age-standardized GBD 2021 PAF estimates for the three environmental
risks.

The PAF is applied once to the corresponding disease-specific macroeconomic burden.

As implemented in the analysis:
- if the lower PAF uncertainty bound is below zero, the PAF point estimate used in the
  calculation is set to zero;
- the same PAF point estimate is applied to the disease-burden point, lower, and upper
  estimates in the primary analysis.

Risk-specific and sub-risk outputs are not additive across overlapping risk hierarchies and
should not be interpreted as mutually exclusive components unless explicitly stated.


Monte Carlo-based sensitivity analysis
--------------------------------------
The economic-parameter sensitivity analysis uses 1,000 simulations.

Parameters varied from 50% to 150% of their baseline values:
- capital share (alpha);
- depreciation rate (delta);
- saving rate; and
- Mincer-function parameters m1, m2, and m3.

Baseline Mincer coefficients:
- m1 = 0.091
- m2 = 0.1301
- m3 = -0.0023

Baseline annual depreciation rate:
- delta = 0.05

Uncertainty interpretation
--------------------------
Primary model bounds and economic-parameter sensitivity intervals represent different
sources of uncertainty.

Primary analysis:
- point, lower, and upper disease inputs are propagated from GBD epidemiological estimates
  through the macroeconomic model.

Economic-parameter sensitivity:
- 1,000 Monte Carlo simulations vary selected macroeconomic parameters over 50%-150% of
  baseline values.

These two uncertainty analyses should not be interpreted as the same interval.

For any reported sum across the three environmental risks, arithmetic sums of separate
risk-specific bounds are not a formally propagated joint 95% uncertainty interval.


Contact
-------
For questions about the analysis or repository, please contact the corresponding author of
the associated manuscript.
