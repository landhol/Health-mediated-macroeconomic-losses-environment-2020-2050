# ============================================================
# Step 8. Risk-attributable macroeconomic burden
# ============================================================
#
# Purpose:
#   Apply GBD 2021 population-attributable fractions (PAFs) to the
#   disease-specific macroeconomic burden estimates from Step 7.
#
#   This main-analysis script is restricted to the three environmental
#   risks evaluated in the manuscript:
#     1. Air pollution
#     2. Unsafe water, sanitation, and handwashing (UWSH)
#     3. Non-optimal temperature
#
# Data sources:
#
#   1. Disease-specific macroeconomic burden
#      Derived in Step 7.
#
#   2. Population-attributable fractions
#      Source: Global Burden of Disease Study 2021 (GBD 2021)
#      Identifier: https://vizhub.healthdata.org/gbd-results
#      Download date: 2026-02-06
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       dplyr: 1.1.4
#
# Notes:
#   - Age-standardized 2021 PAFs are used.
#   - The PAF is applied once to the disease-specific macroeconomic burden.
#   - The same point-estimate PAF is applied to the point, lower, and upper
#     disease-burden estimates.
#   - As in the original analysis, PAF point estimates are set to zero when
#     the corresponding lower uncertainty bound is below zero.
#   - Output monetary values are converted to millions by dividing by 1e6.
#
# ============================================================


# ------------------------------------------------------------
# 1. Package
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Input data
# ------------------------------------------------------------

burden_data <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/impburden_cause_country_ui.csv"
) %>%
  select(
    country,
    cause_name,
    val,
    lower,
    upper
  )

paf_air <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/PAF/air&met/air/IHME-GBD_2021_DATA-b54e5652-1.csv"
)

paf_water <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/PAF/air&met/water/IHME-GBD_2021_DATA-e0be27d9-1.csv"
)

paf_temperature <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/PAF/air&met/temperature/IHME-GBD_2021_DATA-39726ee5-1.csv"
)


# ------------------------------------------------------------
# 3. Prepare PAF datasets
# ------------------------------------------------------------

paf_air_n <- paf_air %>%
  mutate(
    val = ifelse(lower < 0, 0, val)
  ) %>%
  select(c(12, 10, 3, 16)) %>%
  rename(
    country = location_id,
    paf = val
  ) %>%
  arrange(
    rei_name,
    cause_name,
    country
  ) %>%
  filter(
    !cause_name %in% c(
      "Asthma",
      "Blindness and vision loss",
      "Otitis media",
      "Upper respiratory infections"
    )
  )

paf_water_n <- paf_water %>%
  mutate(
    val = ifelse(lower < 0, 0, val)
  ) %>%
  select(c(12, 10, 3, 16)) %>%
  rename(
    country = location_id,
    paf = val
  ) %>%
  mutate(
    cause_name = ifelse(
      cause_name == "Diarrheal diseases",
      "Enteric infections",
      cause_name
    )
  ) %>%
  arrange(
    rei_name,
    cause_name,
    country
  ) %>%
  filter(
    cause_name != "Lower respiratory infections"
  )

paf_temperature_n <- paf_temperature %>%
  mutate(
    val = ifelse(lower < 0, 0, val)
  ) %>%
  select(c(12, 10, 3, 16)) %>%
  rename(
    country = location_id,
    paf = val
  ) %>%
  arrange(
    rei_name,
    cause_name,
    country
  ) %>%
  filter(
    !cause_name %in% c(
      "Exposure to mechanical forces",
      "Other unintentional injuries"
    )
  )


# ------------------------------------------------------------
# 4. Helper functions
# ------------------------------------------------------------

# Calculate risk-attributable burden for every risk hierarchy level,
# then aggregate across causes within each country.
calculate_by_rei_country <- function(burden, paf_data) {

  burden %>%
    merge(
      paf_data,
      by = c("cause_name", "country")
    ) %>%
    mutate(
      burden = val * paf,
      lower_n = lower * paf,
      upper_n = upper * paf
    ) %>%
    arrange(
      rei_name,
      country,
      cause_name
    ) %>%
    group_by(
      rei_name,
      country
    ) %>%
    summarise(
      burden = sum(burden) / 1e6,
      lower = sum(lower_n) / 1e6,
      upper = sum(upper_n) / 1e6,
      .groups = "drop"
    )
}


# Calculate cause-specific attributable burden for one selected risk level.
calculate_by_cause <- function(burden, paf_data, risk_name) {

  burden %>%
    merge(
      paf_data,
      by = c("cause_name", "country")
    ) %>%
    filter(
      rei_name == risk_name
    ) %>%
    mutate(
      burden = val * paf / 1e6,
      lower = lower * paf / 1e6,
      upper = upper * paf / 1e6
    ) %>%
    arrange(
      cause_name,
      country
    ) %>%
    select(
      cause_name,
      country,
      burden,
      lower,
      upper
    )
}


# ------------------------------------------------------------
# 5. Air pollution
# ------------------------------------------------------------

air_rei <- calculate_by_rei_country(
  burden_data,
  paf_air_n
)

write.csv(
  air_rei,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_rei.csv"
)

air_allcause <- calculate_by_cause(
  burden_data,
  paf_air_n,
  "Air pollution"
)

write.csv(
  air_allcause,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_allcause.csv"
)

air_apm <- calculate_by_cause(
  burden_data,
  paf_air_n,
  "Ambient particulate matter pollution"
)

write.csv(
  air_apm,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_apm.csv"
)

air_hap <- calculate_by_cause(
  burden_data,
  paf_air_n,
  "Household air pollution from solid fuels"
)

write.csv(
  air_hap,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_hap.csv"
)

air_aop <- calculate_by_cause(
  burden_data,
  paf_air_n,
  "Ambient ozone pollution"
)

write.csv(
  air_aop,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_aop.csv"
)


# ------------------------------------------------------------
# 6. Unsafe water, sanitation, and handwashing
# ------------------------------------------------------------

water_rei <- calculate_by_rei_country(
  burden_data,
  paf_water_n
)

write.csv(
  water_rei,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_rei.csv"
)

water_allcause <- calculate_by_cause(
  burden_data,
  paf_water_n,
  "Unsafe water, sanitation, and handwashing"
)

write.csv(
  water_allcause,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_allcause.csv"
)

water_source <- calculate_by_cause(
  burden_data,
  paf_water_n,
  "Unsafe water source"
)

write.csv(
  water_source,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_uws.csv"
)

water_sanitation <- calculate_by_cause(
  burden_data,
  paf_water_n,
  "Unsafe sanitation"
)

write.csv(
  water_sanitation,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_us.csv"
)

water_handwashing <- calculate_by_cause(
  burden_data,
  paf_water_n,
  "No access to handwashing facility"
)

write.csv(
  water_handwashing,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_nahf.csv"
)


# ------------------------------------------------------------
# 7. Non-optimal temperature
# ------------------------------------------------------------

temperature_rei <- calculate_by_rei_country(
  burden_data,
  paf_temperature_n
)

write.csv(
  temperature_rei,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_rei.csv"
)

temperature_allcause <- calculate_by_cause(
  burden_data,
  paf_temperature_n,
  "Non-optimal temperature"
)

write.csv(
  temperature_allcause,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_allcause.csv"
)

temperature_high <- calculate_by_cause(
  burden_data,
  paf_temperature_n,
  "High temperature"
)

write.csv(
  temperature_high,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_ht.csv"
)

temperature_low <- calculate_by_cause(
  burden_data,
  paf_temperature_n,
  "Low temperature"
)

write.csv(
  temperature_low,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_lt.csv"
)
