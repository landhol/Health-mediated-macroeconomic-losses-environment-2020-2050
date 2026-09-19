# ============================================================
# Step 6. Aggregate output and disease-specific macroeconomic burden
# ============================================================
#
# Purpose:
#   1. Reconstruct the status-quo capital stock path.
#   2. Calibrate country-year total factor productivity (A_t).
#   3. Simulate disease-elimination counterfactual output using
#      counterfactual aggregate human capital and avoided treatment costs.
#   4. Calculate cumulative disease-specific macroeconomic losses over
#      2020-2050 as the difference between counterfactual and status-quo
#      aggregate output.
#
# Data sources:
#
#   1. Gross domestic product
#      Source: Institute for Health Metrics and Evaluation (IHME)
#      Identifier: DOI: 10.6069/HHKW-4F29
#      Download date: 2026-02-06
#
#   2. Physical capital stock and capital share
#      Source: Penn World Table version 10.0.1
#      Identifier: https://www.rug.nl/ggdc/productivity/pwt/
#      Download date: 2026-02-06
#
#   3. Saving rates
#      Source: World Bank Group, World Development Indicators
#      Identifier:
#      https://databank.worldbank.org/source/world-development-indicators
#      Download date: 2026-02-06
#
#   4. Aggregate human capital
#      Derived in Step 4 from population, labor-force participation,
#      educational attainment, and the Mincer earnings function.
#
#   5. Disease-specific treatment costs
#      Derived in Step 5.
#      U.S. disease-specific treatment expenditure source:
#      Dieleman JL, Cao J, Chapin A, Chen C, Li Z, Liu A, et al.
#      US Health Care Spending by Payer and Health Condition, 1996-2016.
#      JAMA. 2020;323(9):863.
#      DOI: 10.1001/jama.2020.0734
#      Download date: 2026-02-06
#
# Model parameter:
#   delta = 0.05  (annual depreciation rate)
#
# Software:
#   R version: 4.2.1
#   Package versions:
#       dplyr:   1.1.4
#       progress: 1.2.3
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(dplyr)
library(progress)


# ------------------------------------------------------------
# 2. Status-quo macroeconomic inputs
# ------------------------------------------------------------

GDP <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/gdp_161950_n.csv"
)

GDP_n <- GDP %>%
  select(c(2:4)) %>%
  subset(year > 2016)

# Aggregate human capital, 2019-2050.
Ht <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/Ht.csv"
)
Ht <- Ht[, c(2:4)]

# Physical capital stock in 2019.
Kt <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/physical capital stock_2019.csv"
)
Kt <- Kt[, c(2:4)]

# Country-specific saving rate, held constant over the projection horizon.
st <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/saving rate.csv"
)
st <- st[, c(2:3)]

# Country-specific capital share, held constant over the projection horizon.
alpha <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/capital share.csv"
)
alpha_n <- alpha[, c(2, 4)]

# Annual depreciation rate.
delta <- 0.05


# ------------------------------------------------------------
# 3. Reconstruct the status-quo capital stock path
# ------------------------------------------------------------

# Extend the 2019 capital-stock dataset through 2050.
Kt_n <- Kt

for (year in 2020:2050) {

  new_data <- Kt
  new_data$year <- year
  new_data$cn <- 0

  Kt_n <- rbind(
    Kt_n,
    new_data
  )
}

# Merge GDP and saving-rate inputs.
output <- Kt_n %>%
  merge(
    GDP_n,
    by = c("WBcode", "year"),
    all.y = TRUE
  ) %>%
  merge(
    st,
    by = "WBcode"
  ) %>%
  arrange(WBcode, year) %>%
  group_by(WBcode) %>%
  mutate(
    grp = cur_group_id()
  )

# Capital accumulation:
# K_t = s_{t-1} * Y_{t-1} + (1 - delta) * K_{t-1}
proj_K <- as.data.frame(
  matrix(nrow = 0, ncol = 6)
)

for (i in 1:n_distinct(output$grp)) {

  tmp <- subset(
    output,
    output$grp == i
  )

  for (j in 2:nrow(tmp)) {

    tmp[j, 3] <-
      tmp[j - 1, 5] * tmp[j - 1, 4] +
      tmp[j - 1, 3] * (1 - delta)
  }

  proj_K <- rbind(
    proj_K,
    tmp
  )
}


# ------------------------------------------------------------
# 4. Calibrate total factor productivity A_t
# ------------------------------------------------------------

# Production function:
# Y_t = A_t * K_t^alpha * H_t^(1-alpha)
#
# Rearranged to calibrate A_t:
# A_t = Y_t / [K_t^alpha * H_t^(1-alpha)]
At <- proj_K %>%
  merge(
    Ht,
    by = c("WBcode", "year"),
    all.x = TRUE
  ) %>%
  merge(
    alpha_n,
    by = "WBcode"
  ) %>%
  arrange(WBcode, year) %>%
  mutate(
    tech =
      val.gdp /
      (cn^alpha) /
      (Ht_a^(1 - alpha))
  ) %>%
  select(c(1, 2, 9))


# ------------------------------------------------------------
# 5. Counterfactual human capital and treatment-cost inputs
# ------------------------------------------------------------

Ht_c <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/Ht_cf.csv"
)

Ht_val <- Ht_c %>%
  select(c(2:4, "Ht_val_a"))

Ht_lower <- Ht_c %>%
  select(c(2:4, "Ht_lower_a"))

Ht_upper <- Ht_c %>%
  select(c(2:4, "Ht_upper_a"))

TCt <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/treatment cost_1950_n2.csv"
)

TCt_n <- TCt[, c(2:5)]

# Treatment costs were stored in billions of 2017 US dollars.
# Convert to absolute 2017 US dollars before entering the model.
TCt_n$expense <- TCt_n$expense * 1e9


# ------------------------------------------------------------
# 6. Simulate disease-elimination counterfactual output
# ------------------------------------------------------------

Yfunc <- function(data) {

  output_c <- Kt_n %>%
    merge(
      data,
      by = c("WBcode", "year")
    ) %>%
    merge(
      TCt_n,
      by = c("cause_name", "WBcode", "year")
    ) %>%
    merge(
      At,
      by = c("WBcode", "year")
    ) %>%
    merge(
      st,
      by = "WBcode"
    ) %>%
    merge(
      alpha_n,
      by = "WBcode"
    ) %>%
    select(c(3, 1, 2, 4:9)) %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(
      Yt = 0,
      grp = cur_group_id()
    )

  total_iterations <- sum(
    sapply(
      unique(output_c$grp),
      function(i) {
        nrow(subset(output_c, grp == i)) + 1
      }
    )
  )

  pb <- progress_bar$new(
    format = "Calculating [:bar] :percent | :current/:total | eta: :eta",
    total = total_iterations,
    width = 60
  )

  proj_output_c <- as.data.frame(
    matrix(nrow = 0, ncol = 11)
  )

  for (i in 1:n_distinct(output_c$grp)) {

    tmp <- subset(
      output_c,
      output_c$grp == i
    )

    for (j in 2:(nrow(tmp) + 1)) {

      pb$tick()

      # Counterfactual output:
      # Y_t = A_t * K_t^alpha * H_t^(1-alpha)
      tmp[j - 1, 10] <-
        tmp[j - 1, 7] *
        (tmp[j - 1, 4]^tmp[j - 1, 9]) *
        (tmp[j - 1, 5]^(1 - tmp[j - 1, 9]))

      if (j == nrow(tmp) + 1) {

        break

      } else {

        # Counterfactual capital accumulation:
        # K_{t+1} =
        #   (1-delta)K_t + s_t*Y_t + s_t*TC_t
        #
        # TC_t represents disease-specific treatment expenditure avoided
        # under the disease-elimination counterfactual and therefore adds
        # to resources available for saving/investment.
        tmp[j, 4] <-
          (1 - delta) * tmp[j - 1, 4] +
          tmp[j - 1, 8] * tmp[j - 1, 10] +
          tmp[j - 1, 8] * tmp[j - 1, 6]
      }
    }

    proj_output_c <- rbind(
      proj_output_c,
      tmp
    )
  }

  proj_output_cn <- proj_output_c %>%
    select(c(1:3, 10)) %>%
    mutate(
      Yt = ifelse(Yt == 0, NA, Yt)
    )

  return(proj_output_cn)
}


# Propagate the GBD point estimate and lower/upper epidemiological bounds
# through the counterfactual macroeconomic model in parallel.
proj_output_val <- Yfunc(Ht_val)
proj_output_lower <- Yfunc(Ht_lower)
proj_output_upper <- Yfunc(Ht_upper)

proj_output_n <- proj_output_val %>%
  merge(
    proj_output_lower,
    by = c("cause_name", "WBcode", "year")
  ) %>%
  merge(
    proj_output_upper,
    by = c("cause_name", "WBcode", "year")
  ) %>%
  rename(
    Y_val = Yt.x,
    Y_lower = Yt.y,
    Y_upper = Yt
  )

write.csv(
  proj_output_val,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/output_cf_1950_val.csv"
)

write.csv(
  proj_output_lower,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/output_cf_1950_lower.csv"
)

write.csv(
  proj_output_upper,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/output_cf_1950_upper.csv"
)

write.csv(
  proj_output_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/output_cf_1950_ui.csv"
)


# ------------------------------------------------------------
# 7. Cumulative macroeconomic burden, 2020-2050
# ------------------------------------------------------------

# Main analysis: no discounting.
calculate_burden <- function(data) {

  output_tot <- data %>%
    merge(
      GDP_n,
      by = c("WBcode", "year")
    ) %>%
    select(c(3, 1, 2, 4, 5)) %>%
    subset(year >= 2020 & year <= 2050) %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(
      burden = sum(Yt - val.gdp)
    ) %>%
    select(c(1, 2, 6)) %>%
    distinct(
      cause_name,
      WBcode,
      .keep_all = TRUE
    )

  return(output_tot)
}

output_tot_val <- calculate_burden(proj_output_val)
output_tot_lower <- calculate_burden(proj_output_lower)
output_tot_upper <- calculate_burden(proj_output_upper)

output_tot_n <- output_tot_val %>%
  merge(
    output_tot_lower,
    by = c("cause_name", "WBcode")
  ) %>%
  merge(
    output_tot_upper,
    by = c("cause_name", "WBcode")
  ) %>%
  rename(
    burden_val = burden.x,
    burden_lower = burden.y,
    burden_upper = burden
  )

write.csv(
  output_tot_val,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_val.csv"
)

write.csv(
  output_tot_lower,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_lower.csv"
)

write.csv(
  output_tot_upper,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_upper.csv"
)

write.csv(
  output_tot_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_ui.csv"
)


# ------------------------------------------------------------
# 8. Optional sensitivity analysis with 2% annual discounting
# ------------------------------------------------------------

calculate_discounted_burden <- function(data) {

  r <- 0.02
  base_year <- 2020

  output_tot <- data %>%
    merge(
      GDP_n,
      by = c("WBcode", "year")
    ) %>%
    select(c(3, 1, 2, 4, 5)) %>%
    subset(year >= 2020 & year <= 2050) %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(
      discount_factor =
        1 / (1 + r)^(year - base_year),
      discounted_burden =
        (Yt - val.gdp) * discount_factor
    ) %>%
    summarise(
      burden = sum(discounted_burden, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    select(
      cause_name,
      WBcode,
      burden
    )

  return(output_tot)
}

output_tot_val2 <- calculate_discounted_burden(proj_output_val)
output_tot_lower2 <- calculate_discounted_burden(proj_output_lower)
output_tot_upper2 <- calculate_discounted_burden(proj_output_upper)

# Combine the discounted point estimate and lower/upper bounds.
output_tot_n2 <- output_tot_val2 %>%
  merge(
    output_tot_lower2,
    by = c("cause_name", "WBcode")
  ) %>%
  merge(
    output_tot_upper2,
    by = c("cause_name", "WBcode")
  ) %>%
  rename(
    burden_val = burden.x,
    burden_lower = burden.y,
    burden_upper = burden
  )

write.csv(
  output_tot_val2,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_val2.csv"
)

write.csv(
  output_tot_lower2,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_lower2.csv"
)

write.csv(
  output_tot_upper2,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_upper2.csv"
)

write.csv(
  output_tot_n2,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_ui2.csv"
)


# ------------------------------------------------------------
# 9. Aggregate macroeconomic burden by disease
# ------------------------------------------------------------

output_cause_tot <- output_tot_n %>%
  group_by(cause_name) %>%
  mutate(
    burden_val = sum(burden_val, na.rm = TRUE),
    burden_lower = sum(burden_lower, na.rm = TRUE),
    burden_upper = sum(burden_upper, na.rm = TRUE)
  ) %>%
  distinct(
    cause_name,
    .keep_all = TRUE
  )

write.csv(
  output_cause_tot,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/burden_cause_tot_ui.csv"
)
