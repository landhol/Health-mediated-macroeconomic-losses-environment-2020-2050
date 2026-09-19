# ============================================================
# Step 5. Treatment costs
# ============================================================
#
# Purpose:
#   Estimate disease-specific treatment expenditures for 2019-2050.
#   U.S. disease-specific treatment expenditures are used as the
#   reference and are scaled to other countries according to:
#     (1) national health expenditure,
#     (2) the country-to-U.S. prevalence ratio for each disease, and
#     (3) projected growth in per-capita health expenditure.
#
# Data sources:
#
#   1. Gross domestic product / GDP inputs
#      Source: Institute for Health Metrics and Evaluation (IHME)
#      Identifier: DOI: 10.6069/HHKW-4F29
#      Download date: 2026-02-06
#
#   2. Health expenditure
#      Source: World Bank Group, World Development Indicators
#      Identifier:
#      https://databank.worldbank.org/source/world-development-indicators
#      Download date: 2026-02-06
#
#   3. Disease prevalence
#      Source: Global Burden of Disease Study 2021 (GBD 2021)
#      Identifier: https://vizhub.healthdata.org/gbd-results
#      Download date: 2026-02-06
#
#   4. U.S. disease-specific treatment expenditure
#      Input file: USA_cost.CSV
#      Source: Dieleman JL, Cao J, Chapin A, Chen C, Li Z, Liu A, et al.
#              US Health Care Spending by Payer and Health Condition, 1996-2016.
#              JAMA. 2020;323(9):863.
#      DOI: 10.1001/jama.2020.0734
#      Download date: 2026-02-06
#
#   5. Projected health-expenditure shares
#      Input file: healthexp_161950.csv
#      Derived input used by the macroeconomic model.
#      source: World Bank Group, World Development Indicators
#      Download date: 2026-02-06
#
# Currency conversion factors retained from the original analysis:
#   2017 GDP deflator index = 102.76
#   2019 GDP deflator index = 106.85
#   2021 GDP deflator index = 113.21
#   2017 index used for the 2021-US$ GDP series = 102.92
#   Source for these deflator values: World Bank 
#   Download date: 2026-02-06
#
# Software:
#   R version: 4.2.1
#   Package versions:
#       dplyr: 1.1.4
#
# ============================================================


# ------------------------------------------------------------
# 1. Package
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Country mapping and 2019 GDP
# ------------------------------------------------------------

gdp <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/gdp_161950.csv"
)

country <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id.csv"
)

country_old <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv"
)

# Convert GDP to billions and retain 2019 values.
gdp_2019 <- gdp %>%
  mutate(gdp_b = val / 1e9) %>%
  select(c(2, 4, 5)) %>%
  subset(year == 2019)


# ------------------------------------------------------------
# 3. National health expenditure in 2019
# ------------------------------------------------------------

health <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/health expenditure/API_SH.XPD.CHEX.GD.ZS_DS2_en_csv_v2_5715587.csv"
)

health_2019 <- health %>%
  select(c(1, 2, 64)) %>%
  rename(
    WBcode = Country.Code,
    location_name = Country.Name
  )

# Total health expenditure in 2019 US dollars, billions.
# The original analysis converts the 2017-dollar GDP series to 2019 dollars
# using GDP-deflator index values 102.76 (2017) and 106.85 (2019).
national_health_expenditure <- gdp_2019 %>%
  merge(health_2019, by = "WBcode", all.x = TRUE) %>%
  filter(!is.na(X2019)) %>%
  mutate(
    cost = gdp_b * X2019 / 100 / 102.76 * 106.85
  )


# ------------------------------------------------------------
# 4. U.S. disease-specific treatment-cost shares
# ------------------------------------------------------------

us_treatment <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/treatment/IHME 2021/USA_cost.CSV"
)

us_treatment_2019 <- us_treatment %>%
  subset(
    year_id == 2019 &
      location_name == "United States" &
      payer == "All payers" &
      toc == "All toc" &
      age_name == "Age/sex-standardized"
  ) %>%
  select(
    cause_name,
    spend_mean,
    spend_lower,
    spend_upper
  ) %>%
  mutate(
    spend_mean = spend_mean / 1e9,
    spend_lower = spend_lower / 1e9,
    spend_upper = spend_upper / 1e9
  )

# Total U.S. health expenditure used in the original analysis
# (billions of 2019 US dollars).
US_total_health_expenditure <- 2433.27

disease_cost_share_us <- us_treatment_2019 %>%
  mutate(
    share = spend_mean / US_total_health_expenditure
  ) %>%
  filter(!is.na(share))


# ------------------------------------------------------------
# 5. Disease prevalence in 2019
# ------------------------------------------------------------

prevalence <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/prevalence/air&met/IHME-GBD_2021_DATA-9000cc9a-1.csv"
)

prevalence_n <- prevalence %>%
  merge(country, by = "location_id", all.x = TRUE) %>%
  rename(WBcode = Country.Code)

country_disease_data <- national_health_expenditure %>%
  merge(
    prevalence_n,
    by = c("WBcode", "year"),
    all.y = TRUE
  ) %>%
  select(
    WBcode,
    cause_name,
    year,
    val,
    cost
  )


# ------------------------------------------------------------
# 6. Scale U.S. treatment-cost shares to other countries
# ------------------------------------------------------------

country_id <- unique(country_disease_data$WBcode)
disease_name <- unique(disease_cost_share_us$cause_name)

scaled_shares <- data.frame()

for (i in seq_along(country_id)) {

  current_country <- country_id[i]

  # U.S. expenditures are taken directly from the U.S. spending dataset.
  if (current_country == "USA") next

  for (j in seq_along(disease_name)) {

    current_disease <- disease_name[j]

    prev_country <- country_disease_data$val[
      country_disease_data$cause_name == current_disease &
        country_disease_data$WBcode == current_country
    ]

    prev_USA <- country_disease_data$val[
      country_disease_data$cause_name == current_disease &
        country_disease_data$WBcode == "USA"
    ]

    share_USA <- disease_cost_share_us$share[
      disease_cost_share_us$cause_name == current_disease
    ]

    if (
      length(prev_country) > 0 &
      length(prev_USA) > 0 &
      length(share_USA) > 0
    ) {

      # Assumption:
      # The disease-specific share of health expenditure is proportional
      # to the country-to-U.S. prevalence ratio.
      share_est <- prev_country * share_USA / prev_USA

      scaled_shares <- rbind(
        scaled_shares,
        data.frame(
          WBcode = current_country,
          cause_name = current_disease,
          share = share_est
        )
      )
    }
  }
}


# ------------------------------------------------------------
# 7. Disease-specific treatment expenditure in 2019
# ------------------------------------------------------------

# Non-U.S. countries: estimated share multiplied by national health expenditure.
non_USA_expense <- country_disease_data %>%
  filter(WBcode != "USA") %>%
  merge(
    scaled_shares,
    by = c("WBcode", "cause_name")
  ) %>%
  mutate(
    expense = cost * share
  ) %>%
  select(
    WBcode,
    cause_name,
    year,
    expense
  )

# United States: use the observed disease-specific mean spending directly.
USA_expense <- disease_cost_share_us %>%
  filter(!is.na(spend_mean)) %>%
  mutate(
    WBcode = "USA",
    year = 2019,
    expense = spend_mean
  ) %>%
  select(
    WBcode,
    cause_name,
    year,
    expense
  )

health_expense <- rbind(
  non_USA_expense,
  USA_expense
)

write.csv(
  health_expense,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/direct cost_2019_n2.csv"
)


# ------------------------------------------------------------
# 8. Project per-capita health expenditure to 2050
# ------------------------------------------------------------

# Per-capita GDP series in 2021 US dollars.
gdp_per_capita <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/GDP/GDP_per capital_in 2021 usd.CSV"
)

gdp_per_capita_n <- gdp_per_capita %>%
  merge(
    country_old,
    by = "location_name",
    all.y = TRUE
  ) %>%
  select(
    Country.Code,
    year,
    gdp_ppp_mean
  ) %>%
  rename(
    WBcode = Country.Code,
    gdp21 = gdp_ppp_mean
  ) %>%
  arrange(WBcode, year) %>%
  subset(year >= 2019) %>%
  # Convert from 2021 US dollars to 2017 US dollars.
  mutate(
    gdp17 = gdp21 / 113.21 * 102.92
  ) %>%
  select(
    WBcode,
    year,
    gdp17
  )

# Projected health expenditure as a share of GDP.
health_exp_share <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/healthexp_161950.csv"
)
health_exp_share <- health_exp_share[, c(2:4)]

# Per-capita health expenditure.
health_exp_per_capita <- health_exp_share %>%
  merge(
    gdp_per_capita_n,
    by = c("WBcode", "year")
  ) %>%
  mutate(
    exp = val * gdp17
  )


# ------------------------------------------------------------
# 9. Project disease-specific treatment expenditure, 2020-2050
# ------------------------------------------------------------

# Assume that disease-specific treatment expenditure grows at the same
# annual rate as per-capita health expenditure.
health_exp_growth <- health_exp_per_capita %>%
  group_by(WBcode) %>%
  mutate(
    exp_growth_rate =
      (exp - lag(exp, default = first(exp))) /
      lag(exp, default = first(exp))
  ) %>%
  select(-exp)

# Extend the 2019 disease-specific treatment-cost dataset through 2050.
cost_extended <- health_expense

for (year in 2020:2050) {

  new_data <- health_expense
  new_data$year <- year
  new_data$expense <- 0

  cost_extended <- rbind(
    cost_extended,
    new_data
  )
}

cost_1950 <- cost_extended %>%
  merge(
    health_exp_growth,
    by = c("WBcode", "year")
  ) %>%
  arrange(cause_name, WBcode, year) %>%
  group_by(cause_name, WBcode) %>%
  mutate(
    grp = cur_group_id()
  ) %>%
  select(c(1:4, 7, 8))

projected_cost <- as.data.frame(
  matrix(nrow = 0, ncol = 6)
)

for (i in 1:n_distinct(cost_1950$grp)) {

  tmp <- subset(
    cost_1950,
    cost_1950$grp == i
  )

  for (j in 2:nrow(tmp)) {

    tmp[j, 4] <- tmp[j - 1, 4] * (1 + tmp[j, 5])
  }

  projected_cost <- rbind(
    projected_cost,
    tmp
  )
}


# ------------------------------------------------------------
# 10. Convert projected costs to 2017 US dollars and save
# ------------------------------------------------------------

projected_cost_n <- projected_cost %>%
  select(c(1:4)) %>%
  subset(year >= 2019 & year <= 2050) %>%
  arrange(WBcode, cause_name, year) %>%
  mutate(
    # Convert from 2019 US dollars to 2017 US dollars.
    expense = expense / 106.85 * 102.76
  )

write.csv(
  projected_cost_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/treatment cost_1950_n2.csv"
)

# Output unit: billions of 2017 US dollars.
# Downstream scripts should convert from billions where required.
