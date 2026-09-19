# ============================================================
# Step 4. Aggregate human capital
# ============================================================
#
# Purpose:
#   Calculate aggregate human capital under the status quo and
#   disease-elimination counterfactual scenarios for 2020-2050.
#
# Data sources:
#   1. Labor-force participation rates by 5-year age-sex group
#      Source: International Labour Organization (ILO)
#      Identifier: https://www.ilo.org/global/statistics-and-databases/lang--en/index.htm
#      Download date: 2026-02-06
#
#   2. Age-specific educational attainment
#      Source: Barro-Lee Educational Attainment Dataset
#      Identifier: https://barrolee.github.io/BarroLeeDataSet/
#      Download date: 2026-02-06
#
#   3. Population projections
#      Source: United Nations, Department of Economic and Social Affairs
#      Identifier: https://population.un.org/wpp/Download/Standard/Population/
#      Download date: 2026-02-06
#
#   4. Counterfactual labor-force participation and population
#      Derived in Steps 2-3 from GBD 2021 epidemiological inputs and the
#      population/labor-force inputs listed above.
#
# Model parameters:
#   eta1 =  0.0910  : Mincer elasticity of education
#   eta2 =  0.1301  : first-degree Mincer elasticity of experience
#   eta3 = -0.0023  : second-degree Mincer elasticity of experience
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       dplyr: 1.1.4
#
#
# ============================================================


# ------------------------------------------------------------
# 1. Package
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Input data
# ------------------------------------------------------------

# Status-quo labor-force participation by country, sex, age, and year.
labor <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/labor participation.csv"
)

# Age-specific educational attainment.
edu <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/edu level_m.csv"
)


# ------------------------------------------------------------
# 3. Individual human-capital endowment
# ------------------------------------------------------------

eta1 <- 0.091
eta2 <- 0.1301
eta3 <- -0.0023

# Calculate individual human-capital endowment (ht).
# Interpolated_Data represents average years of education.
# The midpoint of each age group is used in the experience term.
ht <- edu %>%
  mutate(
    ageto = ifelse(ageto == 999, 70, ageto),
    ht = exp(
      eta1 * Interpolated_Data +
        eta2 * (((ageto + agefrom) / 2) - Interpolated_Data - 5) +
        eta3 * ((((ageto + agefrom) / 2) - Interpolated_Data - 5)^2)
    )
  ) %>%
  subset(year_n >= 2019 & year_n <= 2050) %>%
  select(c(2:4, 6:8)) %>%
  mutate(
    sex = ifelse(sex == "FALSE", "female", sex),
    sex = ifelse(sex == "M", "male", sex),
    agefrom = ifelse(agefrom == "15", "15-19", agefrom),
    agefrom = ifelse(agefrom == "20", "20-24", agefrom),
    agefrom = ifelse(agefrom == "25", "25-29", agefrom),
    agefrom = ifelse(agefrom == "30", "30-34", agefrom),
    agefrom = ifelse(agefrom == "35", "35-39", agefrom),
    agefrom = ifelse(agefrom == "40", "40-44", agefrom),
    agefrom = ifelse(agefrom == "45", "45-49", agefrom),
    agefrom = ifelse(agefrom == "50", "50-54", agefrom),
    agefrom = ifelse(agefrom == "55", "55-59", agefrom),
    agefrom = ifelse(agefrom == "60", "60-64", agefrom),
    agefrom = ifelse(agefrom == "65", "65+", agefrom)
  ) %>%
  rename(
    age = agefrom,
    year = year_n
  ) %>%
  select(-5)


# ------------------------------------------------------------
# 4. Aggregate human capital under the status quo
# ------------------------------------------------------------

lt <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/labor participation.csv"
)
lt_n <- lt[, c(2:6)]

nt <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/number_1950.csv"
)
nt_n <- nt[, c(2:6)]

data_merged <- merge(
  ht,
  lt_n,
  by = c("WBcode", "sex", "age", "year"),
  all.y = TRUE
)

data_merged <- merge(
  data_merged,
  nt_n,
  by = c("WBcode", "sex", "age", "year"),
  all.x = TRUE
)

# Aggregate human capital:
# H_t = sum_a,s (h_t,a,s * labor_participation_t,a,s * population_t,a,s)
data_n <- data_merged %>%
  arrange(WBcode, year, sex, age) %>%
  group_by(WBcode, year) %>%
  mutate(
    Ht = ht * val * number,
    Ht_a = sum(Ht)
  ) %>%
  select(c(1, 4, 9)) %>%
  distinct(WBcode, year, .keep_all = TRUE)

write.csv(
  data_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/Ht.csv"
)


# ------------------------------------------------------------
# 5. Aggregate human capital under the counterfactual scenario
# ------------------------------------------------------------

# Counterfactual labor-force participation from Step 3.
lt_c <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/labor_cf_1950_ui.csv"
)
lt_cn <- lt_c[, c(2:9)]

# Counterfactual population from Step 2.
nt_c <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/number_cf_1950_ui.csv"
)
nt_cn <- nt_c[, c(2:9)]

data_merged <- merge(
  lt_cn,
  ht,
  by = c("WBcode", "sex", "age", "year"),
  all.x = TRUE
)

data_merged <- merge(
  data_merged,
  nt_cn,
  by = c("cause_name", "WBcode", "sex", "age", "year"),
  all.x = TRUE
)

# Counterfactual aggregate human capital is calculated in parallel for
# the GBD point estimate and its lower and upper epidemiological bounds.
data_c <- data_merged %>%
  arrange(cause_name, WBcode, year, sex, age) %>%
  group_by(cause_name, WBcode, year) %>%
  mutate(
    Ht_val = ht * L_val * N_val,
    Ht_val_a = sum(Ht_val),
    Ht_val_a = ifelse(Ht_val_a == 0, NA, Ht_val_a),

    Ht_lower = ht * L_lower * N_lower,
    Ht_lower_a = sum(Ht_lower),
    Ht_lower_a = ifelse(Ht_lower_a == 0, NA, Ht_lower_a),

    Ht_upper = ht * L_upper * N_upper,
    Ht_upper_a = sum(Ht_upper),
    Ht_upper_a = ifelse(Ht_upper_a == 0, NA, Ht_upper_a)
  ) %>%
  select(c(1, 2, 5, 14, 16, 18)) %>%
  distinct(cause_name, WBcode, year, .keep_all = TRUE)

write.csv(
  data_c,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/Ht_cf.csv"
)
