# ============================================================
# Step 9. Main-analysis summary tables
# ============================================================
#
# Purpose:
#   Generate country-, region-, income-group-, risk-subcategory-, and
#   disease-specific summary tables for the three environmental risks
#   evaluated in the manuscript:
#     1. Air pollution
#     2. Unsafe water, sanitation, and handwashing (UWSH)
#     3. Non-optimal temperature
#
# Data sources:
#
#   1. Population projections
#      Source: United Nations, Department of Economic and Social Affairs
#      Identifier:
#      https://population.un.org/wpp/Download/Standard/Population/
#      Download date: 2026-02-06
#
#   2. Gross domestic product
#      Source: Institute for Health Metrics and Evaluation (IHME)
#      Identifier: DOI: 10.6069/HHKW-4F29
#      Download date: 2026-02-06
#
#   3. Income-group and regional classification
#      Input file: CLASS.xlsx
#      Source/version: World bank  
#      Identifier: https://databank.worldbank.org/
#      Download date: 2026-02-06
#
#   4. Risk-attributable macroeconomic burden
#      Derived in Step 8 using GBD 2021 population-attributable fractions.
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       readxl: 1.4.3
#       dplyr:  1.1.4
#       tidyr:  1.3.0
#
# Notes:
#   - Monetary burden inputs from Step 8 are in millions.
#   - Cumulative GDP is calculated over 2020-2050 and converted to millions.
#   - Population is the mean annual population over 2020-2050.
#   - Country-level per-capita burden is calculated as:
#       burden (million) / population (million persons).
#   - Group-level burden and GDP are reported in billions.
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)


# ------------------------------------------------------------
# 2. Mean population over 2020-2050
# ------------------------------------------------------------

population_file <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/",
  "data/data/population/population_both.xlsx"
)

population_estimates <- read_xlsx(
  population_file,
  sheet = "Estimates",
  col_names = FALSE
)

population_colnames <- read_xlsx(
  population_file,
  sheet = "Estimates",
  col_names = FALSE,
  n_max = 1
)

colnames(population_estimates) <- population_colnames
population_estimates <- population_estimates[-1, ]

population_projection <- read_xlsx(
  population_file,
  sheet = "Medium variant",
  col_names = FALSE
)

colnames(population_projection) <- population_colnames
population_projection <- population_projection[-1, ]

population <- rbind(
  population_estimates,
  population_projection
)

population_mean <- population %>%
  subset(
    Year >= 2020 &
      Year <= 2050
  ) %>%
  rename(
    country = `Region, subregion, country or area *`,
    WBcode = `ISO3 Alpha-code`,
    year = Year
  ) %>%
  select(
    country,
    WBcode,
    11:32
  ) %>%
  arrange(
    country,
    year
  ) %>%
  mutate(
    across(
      3:24,
      as.numeric
    )
  ) %>%
  pivot_longer(
    cols = c(
      "0-4", "5-9", "10-14", "15-19",
      "20-24", "25-29", "30-34", "35-39",
      "40-44", "45-49", "50-54", "55-59",
      "60-64", "65-69", "70-74", "75-79",
      "80-84", "85-89", "90-94", "95-99",
      "100+"
    ),
    names_to = "age",
    values_to = "number"
  ) %>%
  group_by(
    country,
    year
  ) %>%
  mutate(
    population_total = sum(number)
  ) %>%
  distinct(
    country,
    year,
    .keep_all = TRUE
  ) %>%
  group_by(country) %>%
  mutate(
    population_mean = mean(population_total) * 1000
  ) %>%
  select(
    country,
    WBcode,
    population_mean
  ) %>%
  distinct(
    country,
    .keep_all = TRUE
  )

write.csv(
  population_mean,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/pop_country_mean.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 3. Main risk-attributable burden inputs
# ------------------------------------------------------------

cost_air <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_rei.csv"
)

cost_water <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_rei.csv"
)

cost_temperature <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_rei.csv"
)


# ------------------------------------------------------------
# 4. Country mapping, cumulative GDP, and classifications
# ------------------------------------------------------------

country_lookup <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv"
)

country_id_map <- country_lookup %>%
  select(c(3, 4)) %>%
  rename(
    WBcode = Country.Code,
    country = location_id
  )

country_name_map <- country_lookup %>%
  select(c(2, 3)) %>%
  rename(
    WBcode = Country.Code,
    country_name = location_name
  )

gdp <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/gdp_161950_n.csv"
)

# Cumulative GDP over 2020-2050, expressed in millions.
gdp_country <- gdp %>%
  select(c(2:4)) %>%
  subset(year > 2019) %>%
  merge(
    country_id_map,
    by = "WBcode",
    all.y = TRUE
  ) %>%
  group_by(WBcode) %>%
  mutate(
    gdp = sum(val.gdp) / 1e6
  ) %>%
  select(
    -year,
    -val.gdp
  ) %>%
  distinct(
    WBcode,
    country,
    .keep_all = TRUE
  )

income_group <- read_xlsx(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/income group/CLASS.xlsx",
  sheet = 1
)

income_group_n <- income_group %>%
  select(c(2:4)) %>%
  head(n = 218) %>%
  rename(
    WBcode = Code,
    income = `Income group`
  ) %>%
  merge(
    country_id_map,
    by = "WBcode",
    all.y = TRUE
  ) %>%
  mutate(
    income = ifelse(
      WBcode %in% c("COK", "NIU", "TKL", "VEN"),
      "Others",
      income
    ),
    Region = ifelse(
      WBcode %in% c("COK", "NIU", "TKL", "VEN"),
      "Others",
      Region
    )
  )


# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

drop_index_column <- function(data) {

  if ("X" %in% names(data)) {
    data <- data %>% select(-X)
  }

  return(data)
}


format_country_values <- function(data) {

  # Preserve the original reporting sequence:
  # GDP shares are calculated from unrounded monetary values, whereas
  # per-capita values use burden estimates rounded to the nearest million.
  data %>%
    mutate(
      percent_val = round(burden / gdp * 100, 3),
      burden = round(burden),
      percent_lower = round(lower / gdp * 100, 3),
      lower = round(lower),
      percent_upper = round(upper / gdp * 100, 3),
      upper = round(upper),
      capita_val = round(burden / population),
      capita_lower = round(lower / population),
      capita_upper = round(upper / population),
      cost = paste0(
        burden,
        " (",
        lower,
        "-",
        upper,
        ")"
      ),
      percent = paste0(
        percent_val,
        " (",
        percent_lower,
        "-",
        percent_upper,
        ")"
      ),
      capita = paste0(
        capita_val,
        " (",
        capita_lower,
        "-",
        capita_upper,
        ")"
      )
    )
}


format_group_values <- function(data) {

  # Preserve the original table logic: aggregated burdens are first
  # converted from millions to billions and rounded to the nearest billion;
  # GDP shares and per-capita values are then calculated from those rounded
  # burden estimates.
  data %>%
    mutate(
      burden = round(burden / 1e3),
      lower = round(lower / 1e3),
      upper = round(upper / 1e3),
      gdp = gdp / 1e3,
      population = population / 1e9,
      percent_val = round(burden / gdp * 100, 3),
      percent_lower = round(lower / gdp * 100, 3),
      percent_upper = round(upper / gdp * 100, 3),
      capita_val = round(burden / population),
      capita_lower = round(lower / population),
      capita_upper = round(upper / population),
      cost = paste0(
        burden,
        " (",
        lower,
        "-",
        upper,
        ")"
      ),
      percent = paste0(
        percent_val,
        " (",
        percent_lower,
        "-",
        percent_upper,
        ")"
      ),
      capita = paste0(
        capita_val,
        " (",
        capita_lower,
        "-",
        capita_upper,
        ")"
      )
    )
}


# Country-level summary for one risk or risk subcategory.
summarize_country <- function(data, risk_name) {

  data <- drop_index_column(data)

  data %>%
    merge(
      income_group_n,
      by = "country"
    ) %>%
    merge(
      gdp_country,
      by = c("country", "WBcode")
    ) %>%
    merge(
      population_mean,
      by = "WBcode"
    ) %>%
    arrange(
      Region,
      country
    ) %>%
    filter(
      rei_name == risk_name
    ) %>%
    mutate(
      population = population_mean / 1e6
    ) %>%
    format_country_values() %>%
    merge(
      country_name_map,
      by = "WBcode"
    ) %>%
    select(
      Region,
      country_name,
      cost,
      percent,
      capita
    )
}


# Global, income-group, and regional summaries for one risk.
summarize_group <- function(data, risk_name) {

  data <- drop_index_column(data)

  merged_data <- data %>%
    merge(
      income_group_n,
      by = "country"
    ) %>%
    merge(
      gdp_country,
      by = c("country", "WBcode")
    ) %>%
    merge(
      population_mean,
      by = "WBcode"
    ) %>%
    filter(
      rei_name == risk_name
    )

  # Global summary.
  data_global <- merged_data %>%
    summarise(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(gdp),
      population = sum(population_mean),
      .groups = "drop"
    ) %>%
    format_group_values() %>%    transmute(
      group = "Global",
      cost,
      percent,
      capita
    )

  # Income-group summary.
  data_income <- merged_data %>%
    group_by(income) %>%
    summarise(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(gdp),
      population = sum(population_mean),
      .groups = "drop"
    ) %>%
    format_group_values() %>%    transmute(
      group = income,
      cost,
      percent,
      capita
    )

  # Regional summary.
  data_region <- merged_data %>%
    group_by(Region) %>%
    summarise(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(gdp),
      population = sum(population_mean),
      .groups = "drop"
    ) %>%
    format_group_values() %>%    transmute(
      group = Region,
      cost,
      percent,
      capita
    )

  bind_rows(
    data_region,
    data_income,
    data_global
  )
}


# Global summary by risk hierarchy level.
summarize_rei <- function(data) {

  data <- drop_index_column(data)

  data %>%
    merge(
      gdp_country,
      by = "country"
    ) %>%
    merge(
      population_mean,
      by = "WBcode"
    ) %>%
    group_by(rei_name) %>%
    summarise(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(gdp),
      population = sum(population_mean),
      .groups = "drop"
    ) %>%
    format_group_values() %>%    select(
      rei_name,
      cost,
      percent,
      capita
    )
}


# Global disease-specific summary for a selected risk level.
summarize_disease <- function(file_path) {

  data <- read.csv(file_path)
  data <- drop_index_column(data)

  data %>%
    merge(
      gdp_country,
      by = "country"
    ) %>%
    merge(
      population_mean,
      by = "WBcode"
    ) %>%
    group_by(cause_name) %>%
    summarise(
      burden = round(sum(burden), 2),
      lower = round(sum(lower), 2),
      upper = round(sum(upper), 2),
      gdp = sum(gdp),
      population = sum(population_mean),
      .groups = "drop"
    ) %>%
    format_group_values() %>%    select(
      cause_name,
      cost,
      percent,
      capita
    )
}


# ------------------------------------------------------------
# 6. Country-level summaries
# ------------------------------------------------------------

country_outputs <- list(
  air_country_all = list(
    data = cost_air,
    risk = "Air pollution",
    file = "air_country_all.csv"
  ),
  air_country_apm = list(
    data = cost_air,
    risk = "Ambient particulate matter pollution",
    file = "air_country_apm.csv"
  ),
  air_country_hap = list(
    data = cost_air,
    risk = "Household air pollution from solid fuels",
    file = "air_country_hap.csv"
  ),
  air_country_aop = list(
    data = cost_air,
    risk = "Ambient ozone pollution",
    file = "air_country_aop.csv"
  ),
  water_country_all = list(
    data = cost_water,
    risk = "Unsafe water, sanitation, and handwashing",
    file = "wat_country_all.csv"
  ),
  water_country_source = list(
    data = cost_water,
    risk = "Unsafe water source",
    file = "wat_country_uws.csv"
  ),
  water_country_sanitation = list(
    data = cost_water,
    risk = "Unsafe sanitation",
    file = "wat_country_us.csv"
  ),
  water_country_handwashing = list(
    data = cost_water,
    risk = "No access to handwashing facility",
    file = "wat_country_nahf.csv"
  ),
  temperature_country_all = list(
    data = cost_temperature,
    risk = "Non-optimal temperature",
    file = "tem_country_all.csv"
  ),
  temperature_country_high = list(
    data = cost_temperature,
    risk = "High temperature",
    file = "tem_country_ht.csv"
  ),
  temperature_country_low = list(
    data = cost_temperature,
    risk = "Low temperature",
    file = "tem_country_lt.csv"
  )
)

output_dir <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/",
  "outcome/rural outcome/air&met/"
)

for (name in names(country_outputs)) {

  item <- country_outputs[[name]]

  result <- summarize_country(
    item$data,
    item$risk
  )

  assign(
    name,
    result
  )

  write.csv(
    result,
    file.path(
      output_dir,
      item$file
    ),
    row.names = FALSE
  )
}


# ------------------------------------------------------------
# 7. Global, income-group, and regional summaries
# ------------------------------------------------------------

group_outputs <- list(
  air_group_all = list(
    data = cost_air,
    risk = "Air pollution",
    file = "air_inc&reg_all.csv"
  ),
  air_group_apm = list(
    data = cost_air,
    risk = "Ambient particulate matter pollution",
    file = "air_inc&reg_apm.csv"
  ),
  air_group_hap = list(
    data = cost_air,
    risk = "Household air pollution from solid fuels",
    file = "air_inc&reg_hap.csv"
  ),
  air_group_aop = list(
    data = cost_air,
    risk = "Ambient ozone pollution",
    file = "air_inc&reg_aop.csv"
  ),
  water_group_all = list(
    data = cost_water,
    risk = "Unsafe water, sanitation, and handwashing",
    file = "wat_inc&reg_all.csv"
  ),
  water_group_source = list(
    data = cost_water,
    risk = "Unsafe water source",
    file = "wat_inc&reg_uws.csv"
  ),
  water_group_sanitation = list(
    data = cost_water,
    risk = "Unsafe sanitation",
    file = "wat_inc&reg_us.csv"
  ),
  water_group_handwashing = list(
    data = cost_water,
    risk = "No access to handwashing facility",
    file = "wat_inc&reg_nahf.csv"
  ),
  temperature_group_all = list(
    data = cost_temperature,
    risk = "Non-optimal temperature",
    file = "tem_inc&reg_all.csv"
  ),
  temperature_group_high = list(
    data = cost_temperature,
    risk = "High temperature",
    file = "tem_inc&reg_ht.csv"
  ),
  temperature_group_low = list(
    data = cost_temperature,
    risk = "Low temperature",
    file = "tem_inc&reg_lt.csv"
  )
)

for (name in names(group_outputs)) {

  item <- group_outputs[[name]]

  result <- summarize_group(
    item$data,
    item$risk
  )

  assign(
    name,
    result
  )

  write.csv(
    result,
    file.path(
      output_dir,
      item$file
    ),
    row.names = FALSE
  )
}


# ------------------------------------------------------------
# 8. Global summaries by risk subcategory
# ------------------------------------------------------------

air_rei_summary <- summarize_rei(
  cost_air
)

water_rei_summary <- summarize_rei(
  cost_water
)

temperature_rei_summary <- summarize_rei(
  cost_temperature
)

write.csv(
  air_rei_summary,
  file.path(
    output_dir,
    "air_rei_grp.csv"
  ),
  row.names = FALSE
)

write.csv(
  water_rei_summary,
  file.path(
    output_dir,
    "wat_rei_grp.csv"
  ),
  row.names = FALSE
)

write.csv(
  temperature_rei_summary,
  file.path(
    output_dir,
    "tem_rei_grp.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. Global disease-specific summaries
# ------------------------------------------------------------

disease_outputs <- list(
  air_allcause_grp = "air_allcause.csv",
  air_apm_grp = "air_apm.csv",
  air_hap_grp = "air_hap.csv",
  air_aop_grp = "air_aop.csv",
  water_allcause_grp = "wat_allcause.csv",
  water_source_grp = "wat_uws.csv",
  water_sanitation_grp = "wat_us.csv",
  water_handwashing_grp = "wat_nahf.csv",
  temperature_allcause_grp = "tem_allcause.csv",
  temperature_high_grp = "tem_ht.csv",
  temperature_low_grp = "tem_lt.csv"
)

for (name in names(disease_outputs)) {

  input_file <- file.path(
    output_dir,
    disease_outputs[[name]]
  )

  result <- summarize_disease(
    input_file
  )

  assign(
    name,
    result
  )

  output_file <- paste0(
    sub(
      "_grp$",
      "",
      name
    ),
    "_grp.csv"
  )

  # Preserve the original output filenames used by downstream workflows.
  output_file <- switch(
    name,
    water_allcause_grp = "wat_allcause_grp.csv",
    water_source_grp = "wat_uws_grp.csv",
    water_sanitation_grp = "wat_us_grp.csv",
    water_handwashing_grp = "wat_nahf_grp.csv",
    temperature_allcause_grp = "tem_allcause_grp.csv",
    temperature_high_grp = "tem_ht_grp.csv",
    temperature_low_grp = "tem_lt_grp.csv",
    output_file
  )

  write.csv(
    result,
    file.path(
      output_dir,
      output_file
    ),
    row.names = FALSE
  )
}
