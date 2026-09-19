# ==============================================================================
# Script 1: Death, YLL, and YLD projection with GBD uncertainty bounds
# ==============================================================================
# Purpose:
#   Project cause-, location-, sex-, and age-specific mortality, YLL, and YLD
#   rates to 2050 using historical average annual growth rates.
#
# Epidemiological input:
#   Dataset: Global Burden of Disease Study 2021 (GBD 2021)
#   Measures: Deaths, years of life lost (YLLs), and years lived with disability (YLDs)
#   Input years: 2010-2021
#   Rate unit: per 100,000 population
#   Stratification: 5-year age groups, sex, cause, and location
#   source:https://vizhub.healthdata.org/gbd-results
#   Dataset version/source: GBD 2021
#   Download date: 2026-02-06
#
# Projection approach:
#   - Historical average annual growth rates are calculated using 2010-2019 data.
#   - Point estimates and the lower/upper bounds of the GBD 95% uncertainty
#     intervals are projected separately.
#   - Positive annual growth rates are capped at 2%.
#   - 2019-2021 observed data are retained; 2022-2050 values are projected.
#
# Reproducibility:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       data.table: 1.14.10
#       dplyr: 1.1.4
#
#
# Notes:
#   - Country/location mapping follows GBD 2021 location identifiers.
#   - Missing country-age-sex-year-cause combinations are filled with zero after
#     expansion to the complete analysis grid.
# ==============================================================================

# ---- Packages ----------------------------------------------------------------
library(data.table)
library(dplyr)

# ---- Read mortality data ------------------------------------------------------
dirname <- dir("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/mortality/air&met/")
file <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/mortality/air&met/",
  dirname
)

death_1021 <- as.data.frame(matrix(nrow = 0, ncol = 16))
for (a in file) {
  data <- fread(a)
  death_1021 <- rbind(death_1021, data)
}

# ---- Read YLL data ------------------------------------------------------------
dirname <- dir("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/YLL/air&met/")
file <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/YLL/air&met/",
  dirname
)

YLL_1021 <- as.data.frame(matrix(nrow = 0, ncol = 16))
for (a in file) {
  data <- fread(a)
  YLL_1021 <- rbind(YLL_1021, data)
}

# ---- Read YLD data ------------------------------------------------------------
dirname <- dir("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/YLD/air&met/")
file <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/YLD/air&met/",
  dirname
)

YLD_1021 <- as.data.frame(matrix(nrow = 0, ncol = 16))
for (a in file) {
  data <- fread(a)
  YLD_1021 <- rbind(YLD_1021, data)
}

# ---- Projection function ------------------------------------------------------
proj <- function(data) {
  data_1021_n <- data %>%
    select(
      .,
      c(
        "measure_name", "cause_name", "location_id", "sex_name", "age_name",
        "year", "val", "lower", "upper"
      )
    ) %>%
    arrange(., cause_name, location_id, sex_name, age_name, year) %>%
    group_by(., cause_name, location_id, sex_name, age_name) %>%
    # Use 2010-2019 data to calculate historical annual growth rates.
    filter(!(year == 2021 | year == 2020)) %>%
    mutate(
      rate_v = (val - lag(val)) / lag(val),
      rate_l = (lower - lag(lower)) / lag(lower),
      rate_u = (upper - lag(upper)) / lag(upper)
    ) %>%
    # Calculate the mean annual growth rate for point, lower, and upper estimates.
    mutate(
      rate_v_m = mean(rate_v, na.rm = TRUE),
      rate_l_m = mean(rate_l, na.rm = TRUE),
      rate_u_m = mean(rate_u, na.rm = TRUE)
    ) %>%
    # Cap positive annual growth rates at 2%.
    mutate(
      rate_v_m = ifelse(rate_v_m > 0.02, 0.02, rate_v_m),
      rate_l_m = ifelse(rate_l_m > 0.02, 0.02, rate_l_m),
      rate_u_m = ifelse(rate_u_m > 0.02, 0.02, rate_u_m)
    )

  # Diagnostic object retained from the original analysis workflow.
  high_growth_data <- data_1021_n %>%
    group_by(cause_name, location_id, sex_name, age_name) %>%
    filter(any(rate_v_m > 0.02 | rate_l_m > 0.02 | rate_u_m > 0.02))

  # Retain observed data for 2019-2021.
  data_1921 <- data_1021_n %>%
    subset(year == 2019 | year == 2020 | year == 2021) %>%
    select(!c(rate_v, rate_l, rate_u))

  data_n <- data %>%
    select(
      .,
      c(
        "measure_name", "cause_name", "location_id", "sex_name", "age_name",
        "year", "val", "lower", "upper"
      )
    ) %>%
    arrange(., cause_name, location_id, sex_name, age_name, year)

  # Attach the historical mean growth rates to the 2021 baseline observations.
  data_21 <- data_1021_n %>%
    select(!c(rate_v, rate_l, rate_u, year, val, lower, upper)) %>%
    distinct(
      cause_name, location_id, sex_name, age_name,
      rate_v_m, rate_l_m, rate_u_m,
      .keep_all = TRUE
    ) %>%
    merge(
      data_n,
      by = c("measure_name", "cause_name", "location_id", "sex_name", "age_name")
    ) %>%
    subset(year == 2021) %>%
    group_by(cause_name, location_id, sex_name, age_name)

  # Project annual values from 2022 through 2050.
  result_list <- list()

  for (i in 1:29) {
    mutated_data <- data_21 %>%
      mutate(
        val = val * ((1 + rate_v_m)^i),
        lower = lower * ((1 + rate_l_m)^i),
        upper = upper * ((1 + rate_u_m)^i),
        year = 2021 + i
      ) %>%
      select(-c(rate_v_m, rate_l_m, rate_u_m))

    result_list[[i]] <- mutated_data
  }

  # Combine observed 2019-2021 data with projected 2022-2050 data.
  final_data <- do.call(rbind, result_list)
  final_data <- data_1921 %>%
    rbind(final_data) %>%
    select(-c(rate_v_m, rate_l_m, rate_u_m))

  return(final_data)
}

# ---- Generate projections -----------------------------------------------------
death_2050 <- proj(death_1021)
YLL_2050 <- proj(YLL_1021)
YLD_2050 <- proj(YLD_1021)

# The projected datasets retain observed values for 2019-2021 and projected
# values for 2022-2050.

# ---- Country/location mapping -------------------------------------------------
# The country mapping file was updated to align with GBD 2021 location names.
# Location IDs are used as the primary matching key where possible.
country <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv"
)

country_n <- country %>%
  select(., c(3, 4)) %>%
  rename(country = location_id, WBcode = Country.Code)

country_name <- country$location_id

disease_name <- unique(YLD_2050$cause_name)

# ---- Expand to the complete analysis grid ------------------------------------
fill <- function(data) {
  data_1950_n <- data %>%
    arrange(., cause_name, location_id, sex_name, age_name, year) %>%
    # Convert GBD rates from per 100,000 population to proportions.
    mutate(
      val = val / 100000,
      lower = lower / 100000,
      upper = upper / 100000
    )

  data_n <- data_1950_n %>%
    select(., c(2:9)) %>%
    rename(country = location_id, sex = sex_name, age = age_name) %>%
    # Harmonize age- and sex-group labels.
    mutate(
      age = ifelse(age == "<5 years", "0-4", age),
      age = ifelse(age == "5-9 years", "5-9", age),
      age = ifelse(age == "10-14 years", "10-14", age),
      age = ifelse(age == "15-19 years", "15-19", age),
      age = ifelse(age == "20-24 years", "20-24", age),
      age = ifelse(age == "25-29 years", "25-29", age),
      age = ifelse(age == "30-34 years", "30-34", age),
      age = ifelse(age == "35-39 years", "35-39", age),
      age = ifelse(age == "40-44 years", "40-44", age),
      age = ifelse(age == "45-49 years", "45-49", age),
      age = ifelse(age == "50-54 years", "50-54", age),
      age = ifelse(age == "55-59 years", "55-59", age),
      age = ifelse(age == "60-64 years", "60-64", age),
      age = ifelse(age == "65-69 years", "65-69", age),
      age = ifelse(age == "70-74 years", "70-74", age),
      age = ifelse(age == "75-79 years", "75-79", age),
      age = ifelse(age == "80-84 years", "80-84", age),
      age = ifelse(age == "85-89 years", "85-89", age),
      age = ifelse(age == "90-94 years", "90-94", age),
      age = ifelse(age == "95+ years", "95+", age),
      sex = ifelse(sex == "Female", "female", sex),
      sex = ifelse(sex == "Male", "male", sex)
    ) %>%
    arrange(cause_name, country, year, sex, age)

  # Create all cause-location-year-sex-age combinations required by the analysis.
  all_combinations <- expand.grid(
    cause_name = disease_name,
    country = country_name,
    year = 2019:2050,
    sex = c("female", "male"),
    age = c(
      "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
      "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
      "70-74", "75-79", "80-84", "85-89", "90-94", "95+"
    )
  )

  # Merge observed/projected data into the complete grid and set missing values to zero.
  result_df <- all_combinations %>%
    merge(
      ., data_n,
      by = c("cause_name", "country", "year", "sex", "age"),
      all.x = TRUE
    ) %>%
    merge(country_n, by = "country") %>%
    select(cause_name, WBcode, year, sex, age, val, lower, upper) %>%
    arrange(cause_name, WBcode, year, sex, age) %>%
    replace(., is.na(.), 0)

  return(result_df)
}

# ---- Final projected input datasets ------------------------------------------
death_1950_n <- death_2050 %>% fill()
YLL_1950_n <- YLL_2050 %>% fill()
YLD_1950_n <- YLD_2050 %>% fill()

# ---- Write outputs ------------------------------------------------------------
write.csv(
  death_1950_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/death_1950_n.csv"
)
write.csv(
  YLL_1950_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/YLL_1950_n.csv"
)
write.csv(
  YLD_1950_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/YLD_1950_n.csv"
)
