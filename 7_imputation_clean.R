# ============================================================
# Step 7. Imputation of missing country-level macroeconomic burden
# ============================================================
#
# Purpose:
#   1. Project disease-specific DALY rates to 2050.
#   2. Calculate the average age-sex-standardized DALY rate over the
#      projection period.
#   3. Impute missing country-level disease-specific macroeconomic burdens
#      using the cross-country relationship between burden as a share of
#      cumulative GDP and the corresponding DALY rate.
#   4. For diseases with weak overall linear associations, perform
#      income-group-specific imputation and use median-based imputation
#      when the within-group slope is not statistically significant.
#
# Data sources:
#
#   1. DALYs
#      Source: Global Burden of Disease Study 2021 (GBD 2021)
#      Identifier: https://vizhub.healthdata.org/gbd-results
#      Download date: 2026-02-06
#
#   2. Population projections
#      Source: United Nations, Department of Economic and Social Affairs
#      Identifier:
#      https://population.un.org/wpp/Download/Standard/Population/
#      Download date: 2026-02-06
#
#   3. Gross domestic product
#      Source: Institute for Health Metrics and Evaluation (IHME)
#      Identifier: DOI: 10.6069/HHKW-4F29
#      Download date: 2026-02-06
#
#   4. Income-group classification
#      Input file: CLASS.xlsx
#      Source/version: [TO FILL: exact source and classification version]
#      Download date: 2026-02-06
#
#   5. Disease-specific macroeconomic burden
#      Derived in Step 6.
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       data.table: 1.14.10
#       dplyr:     1.1.4
#       readxl:    1.4.3
#       progress:  1.2.3
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(data.table)
library(dplyr)
library(readxl)
library(progress)


# ------------------------------------------------------------
# 2. Read and prepare DALY-rate inputs
# ------------------------------------------------------------

daly_dir <- "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/DALYs/air&met/daly_rate/"

daly_files <- list.files(
  daly_dir,
  full.names = TRUE
)

daly_1021 <- as.data.frame(
  matrix(nrow = 0, ncol = 16)
)

for (file_path in daly_files) {
  
  data_tmp <- fread(file_path)
  daly_1021 <- rbind(
    daly_1021,
    data_tmp
  )
}

country <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/data/data/country name/location_id_old.csv"
)

country_n <- country %>%
  select(c(3, 4)) %>%
  rename(WBcode = Country.Code)

daly_n <- daly_1021 %>%
  merge(
    country_n,
    by = "location_id"
  ) %>%
  select(c(10, 17, 13, 6, 8, 14)) %>%
  arrange(
    cause_name,
    WBcode,
    year,
    sex_name
  )


# ------------------------------------------------------------
# 3. Project DALY rates through 2050
# ------------------------------------------------------------

project_daly <- function(data) {
  
  growth_data <- data %>%
    arrange(
      cause_name,
      WBcode,
      sex_name,
      age_name,
      year
    ) %>%
    group_by(
      cause_name,
      WBcode,
      sex_name,
      age_name
    ) %>%
    # Exclude 2020 and 2021 when estimating the historical growth rate.
    filter(!(year == 2020 | year == 2021)) %>%
    mutate(
      rate = (val - lag(val)) / lag(val),
      rate_m = mean(rate, na.rm = TRUE),
      # Cap positive mean annual growth at 2%.
      rate_m = ifelse(rate_m > 0.02, 0.02, rate_m),
      rate_m = ifelse(is.na(rate_m), 0, rate_m)
    )
  
  observed_1921 <- data %>%
    subset(
      year == 2019 |
        year == 2020 |
        year == 2021
    )
  
  baseline_2021 <- growth_data %>%
    select(!(rate | year | val)) %>%
    distinct(
      cause_name,
      WBcode,
      sex_name,
      age_name,
      rate_m,
      .keep_all = TRUE
    ) %>%
    merge(
      data,
      by = c(
        "cause_name",
        "WBcode",
        "sex_name",
        "age_name"
      )
    ) %>%
    subset(year == 2021) %>%
    group_by(
      cause_name,
      WBcode,
      sex_name,
      age_name
    )
  
  pb <- progress_bar$new(
    format = "Predicting groups [:bar] :percent | Group :current/:total | eta: :eta",
    total = 29,
    width = 60
  )
  
  result_list <- list()
  
  for (i in 1:29) {
    
    pb$tick()
    
    projected_year <- baseline_2021 %>%
      mutate(
        val = val * ((1 + rate_m)^i),
        year = 2021 + i
      ) %>%
      select(-rate_m)
    
    result_list[[i]] <- projected_year
  }
  
  projected_data <- do.call(
    rbind,
    result_list
  )
  
  final_data <- observed_1921 %>%
    rbind(projected_data)
  
  return(final_data)
}


# ------------------------------------------------------------
# 4. Calculate average age-sex-standardized DALY rates
# ------------------------------------------------------------

population <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/number_1950_origin.csv"
)

# Use the 2019 population distribution as the age-sex standardization weight.
population_2019 <- population %>%
  subset(year == 2019) %>%
  select(-any_of(c("X", "year"))) %>%
  arrange(WBcode, sex) %>%
  group_by(WBcode, sex) %>%
  mutate(
    num_tot = sum(number),
    rate = number / num_tot
  ) %>%
  select(-c(number, num_tot))

daly_2050 <- daly_n %>%
  project_daly() %>%
  rename(
    sex = sex_name,
    age = age_name
  ) %>%
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
  merge(
    population_2019,
    by = c("WBcode", "sex", "age")
  ) %>%
  arrange(
    cause_name,
    WBcode,
    year,
    sex,
    age
  ) %>%
  group_by(
    cause_name,
    WBcode,
    year
  ) %>%
  mutate(
    val_stand = val * rate,
    val_tot = sum(val_stand)
  ) %>%
  select(
    cause_name,
    WBcode,
    year,
    val_tot
  ) %>%
  distinct(
    cause_name,
    WBcode,
    year,
    val_tot,
    .keep_all = TRUE
  ) %>%
  group_by(
    cause_name,
    WBcode
  ) %>%
  mutate(
    # Convert rate per 100,000 to a proportion.
    daly_m = mean(val_tot) / 100000
  ) %>%
  select(
    cause_name,
    WBcode,
    daly_m
  ) %>%
  distinct(
    cause_name,
    WBcode,
    .keep_all = TRUE
  )

write.csv(
  daly_2050,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/daly_stand.csv"
)


# ------------------------------------------------------------
# 5. Read disease-specific macroeconomic burdens and cumulative GDP
# ------------------------------------------------------------

daly_2050 <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/daly_stand.csv"
)
daly_2050 <- daly_2050 %>%
  select(-X)

data_val <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_val.csv"
)

data_lower <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_lower.csv"
)

data_upper <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/burden_cause_country_upper.csv"
)

GDP <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/gdp_161950_n.csv"
)

GDP_n <- GDP %>%
  select(c(2:4)) %>%
  subset(year > 2019) %>%
  group_by(WBcode) %>%
  mutate(
    totgdp = sum(val.gdp)
  ) %>%
  select(c(1, 4)) %>%
  distinct(
    WBcode,
    .keep_all = TRUE
  )

disease_name <- unique(data_val$cause_name)
country_code <- unique(GDP_n$WBcode)


# ------------------------------------------------------------
# 6. Complete the disease-country grid
# ------------------------------------------------------------

fill_2050 <- function(data) {
  
  all_combinations <- expand.grid(
    cause_name = disease_name,
    WBcode = country_code
  )
  
  result_df <- all_combinations %>%
    merge(
      data,
      by = c("cause_name", "WBcode"),
      all.x = TRUE
    ) %>%
    arrange(
      cause_name,
      WBcode
    )
  
  return(result_df)
}


# ------------------------------------------------------------
# 7. Primary cross-country imputation
# ------------------------------------------------------------

imputation <- function(data) {
  
  imp <- data %>%
    select(c(2:4)) %>%
    fill_2050() %>%
    merge(
      GDP_n,
      by = "WBcode",
      all.x = TRUE
    ) %>%
    # Variable name retained for compatibility with downstream code.
    # This is a fraction/share of cumulative GDP, not a percentage
    # multiplied by 100.
    mutate(
      percent = burden / totgdp
    )
  
  imp_n <- imp %>%
    merge(
      daly_2050,
      by = c("cause_name", "WBcode")
    )
  
  result_df <- data.frame()
  
  pb <- progress_bar$new(
    format = "Processing [:bar] :percent | :current/:total | eta: :eta",
    total = length(disease_name),
    width = 60
  )
  
  for (i in seq_along(disease_name)) {
    
    pb$tick()
    
    df <- imp_n %>%
      subset(
        cause_name == disease_name[i]
      )
    
    df_no_missing <- na.omit(df)
    
    linear_model <- lm(
      percent ~ daly_m,
      data = df_no_missing
    )
    
    model_sum <- summary(linear_model)
    
    intercept_p <- coef(model_sum)[1, 4]
    daly_p <- coef(model_sum)[2, 4]
    
    # Print the disease index when the DALY-rate slope is not significant.
    if (!is.na(daly_p) && daly_p > 0.05) {
      print(i)
    }
    
    # If the intercept is not significant, refit without an intercept.
    if (!is.na(intercept_p) && intercept_p > 0.05) {
      
      linear_model <- lm(
        percent ~ 0 + daly_m,
        data = df_no_missing
      )
    }
    
    df$predicted_b <- predict(
      linear_model,
      newdata = df
    )
    
    df_n <- df %>%
      mutate(
        percent = ifelse(
          is.na(percent),
          predicted_b,
          percent
        ),
        burden = ifelse(
          is.na(burden),
          percent * totgdp,
          burden
        )
      )
    
    result_df <- rbind(
      result_df,
      df_n
    )
  }
  
  country <- read.csv(
    "/dssg/home/acct-wenze.zhong/jingxuanw/economic/data/data/country name/location_id_old.csv"
  )
  
  country_n <- country %>%
    select(c(3, 4)) %>%
    rename(
      country = location_id,
      WBcode = Country.Code
    )
  
  result_df_n <- result_df %>%
    merge(
      country_n,
      by = "WBcode"
    )
  
  return(result_df_n)
}


result_val <- imputation(data_val)
result_lower <- imputation(data_lower)
result_upper <- imputation(data_upper)


# ------------------------------------------------------------
# 8. Income-group-specific imputation for selected diseases
# ------------------------------------------------------------

inc_grp <- read_xlsx(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/data/data/income group/CLASS.xlsx",
  sheet = 1
)

inc_grp_n <- inc_grp %>%
  select(c(2:4)) %>%
  head(n = 218) %>%
  rename(
    WBcode = Code,
    income = `Income group`
  ) %>%
  merge(
    country_n,
    by = "WBcode",
    all.y = TRUE
  ) %>%
  mutate(
    income = ifelse(WBcode == "COK", "Others", income),
    income = ifelse(WBcode == "NIU", "Others", income),
    income = ifelse(WBcode == "TKL", "Others", income),
    income = ifelse(WBcode == "VEN", "Others", income),
    Region = ifelse(WBcode == "COK", "Others", Region),
    Region = ifelse(WBcode == "NIU", "Others", Region),
    Region = ifelse(WBcode == "TKL", "Others", Region),
    Region = ifelse(WBcode == "VEN", "Others", Region)
  ) %>%
  select(
    WBcode,
    income
  )


imputation_2 <- function(data, i, data_2) {
  
  imp <- data %>%
    select(2:4) %>%
    fill_2050() %>%
    merge(
      GDP_n,
      by = "WBcode",
      all.x = TRUE
    ) %>%
    mutate(
      percent = burden / totgdp
    ) %>%
    merge(
      inc_grp_n,
      by = "WBcode",
      all.x = TRUE
    )
  
  imp_n <- imp %>%
    merge(
      daly_2050,
      by = c("cause_name", "WBcode")
    )
  
  result_df <- data.frame()
  
  df <- imp_n %>%
    subset(
      cause_name == disease_name[i]
    )
  
  income_groups <- unique(df$income)
  
  for (j in seq_along(income_groups)) {
    
    df_group <- df %>%
      filter(
        income == income_groups[j]
      )
    
    df_no_missing <- na.omit(df_group)
    
    if (nrow(df_no_missing) > 2) {
      
      linear_model <- lm(
        percent ~ daly_m,
        data = df_no_missing
      )
      
      model_sum <- summary(linear_model)
      
      intercept_p <- coef(model_sum)[1, 4]
      daly_m_p <- coef(model_sum)[2, 4]
      
      # If the DALY-rate slope is not significant, impute missing burden
      # shares using the median observed share within the income group.
      if (!is.na(daly_m_p) && daly_m_p > 0.05) {
        
        median_share <- quantile(
          df_no_missing$percent,
          probs = 0.5,
          na.rm = TRUE
        )
        
        df_group$predicted_b <- NA
        
        df_group <- df_group %>%
          mutate(
            percent = ifelse(
              is.na(percent),
              median_share,
              percent
            ),
            burden = ifelse(
              is.na(burden),
              percent * totgdp,
              burden
            )
          )
        
        result_df <- rbind(
          result_df,
          df_group
        )
        
        next
      }
      
      # If the intercept is not significant, refit without an intercept.
      if (!is.na(intercept_p) && intercept_p > 0.05) {
        
        linear_model <- lm(
          percent ~ 0 + daly_m,
          data = df_no_missing
        )
      }
      
      df_group$predicted_b <- predict(
        linear_model,
        newdata = df_group
      )
      
    } else {
      
      # If there are insufficient observations within an income group,
      # use the corresponding result from the overall imputation.
      df_group <- df_group %>%
        merge(
          data_2,
          by = c(
            "cause_name",
            "WBcode",
            "totgdp",
            "daly_m"
          )
        ) %>%
        mutate(
          burden.x = burden.y
        ) %>%
        rename(
          burden = burden.x,
          percent = percent.x
        ) %>%
        select(
          "cause_name",
          "WBcode",
          "burden",
          "totgdp",
          "percent",
          "income",
          "daly_m"
        )
      
      df_group$predicted_b <- NA
      
      result_df <- rbind(
        result_df,
        df_group
      )
      
      next
    }
    
    df_group <- df_group %>%
      mutate(
        percent = ifelse(
          is.na(percent),
          predicted_b,
          percent
        ),
        burden = ifelse(
          is.na(burden),
          percent * totgdp,
          burden
        )
      )
    
    result_df <- rbind(
      result_df,
      df_group
    )
  }
  
  country <- read.csv(
    "/dssg/home/acct-wenze.zhong/jingxuanw/economic/data/data/country name/location_id_old.csv"
  )
  
  country_n <- country %>%
    select(c(3, 4)) %>%
    rename(
      country = location_id,
      WBcode = Country.Code
    )
  
  result_df_n <- result_df %>%
    merge(
      country_n,
      by = "WBcode"
    )
  
  return(result_df_n)
}


# Disease indices retained from the original analysis.
result_42_val <- imputation_2(
  data_val,
  42,
  result_val
) %>%
  select(-income)

result_42_lower <- imputation_2(
  data_lower,
  42,
  result_lower
) %>%
  select(-income)

result_42_upper <- imputation_2(
  data_upper,
  42,
  result_upper
) %>%
  select(-income)

result_52_val <- imputation_2(
  data_val,
  52,
  result_val
) %>%
  select(-income)

result_52_lower <- imputation_2(
  data_lower,
  52,
  result_lower
) %>%
  select(-income)


# ------------------------------------------------------------
# 9. Replace selected primary-imputation results
# ------------------------------------------------------------

result_m_val <- rbind(
  result_42_val,
  result_52_val
)

result_m_lower <- rbind(
  result_42_lower,
  result_52_lower
)

result_val_n <- result_val %>%
  merge(
    result_m_val,
    by = c(
      "WBcode",
      "cause_name",
      "country"
    ),
    all.x = TRUE
  ) %>%
  mutate(
    burden.x = ifelse(
      !is.na(burden.y),
      burden.y,
      burden.x
    )
  ) %>%
  select(c(1:8)) %>%
  rename(
    burden = burden.x,
    totgdp = totgdp.x,
    percent = percent.x,
    daly_m = daly_m.x,
    predicted_b = predicted_b.x
  )

result_lower_n <- result_lower %>%
  merge(
    result_m_lower,
    by = c(
      "WBcode",
      "cause_name",
      "country"
    ),
    all.x = TRUE
  ) %>%
  mutate(
    burden.x = ifelse(
      !is.na(burden.y),
      burden.y,
      burden.x
    )
  ) %>%
  select(c(1:8)) %>%
  rename(
    burden = burden.x,
    totgdp = totgdp.x,
    percent = percent.x,
    daly_m = daly_m.x,
    predicted_b = predicted_b.x
  )

result_upper_n <- result_upper %>%
  merge(
    result_42_upper,
    by = c(
      "WBcode",
      "cause_name",
      "country"
    ),
    all.x = TRUE
  ) %>%
  mutate(
    burden.x = ifelse(
      !is.na(burden.y),
      burden.y,
      burden.x
    )
  ) %>%
  select(c(1:8)) %>%
  rename(
    burden = burden.x,
    totgdp = totgdp.x,
    percent = percent.x,
    daly_m = daly_m.x,
    predicted_b = predicted_b.x
  )


# ------------------------------------------------------------
# 10. Combine point estimate and lower/upper bounds
# ------------------------------------------------------------

result_df_n <- result_val_n %>%
  select(c(1:5)) %>%
  merge(
    result_lower_n,
    by = c(
      "cause_name",
      "WBcode",
      "country"
    )
  ) %>%
  select(c(1:6)) %>%
  merge(
    result_upper_n,
    by = c(
      "cause_name",
      "WBcode",
      "country"
    )
  ) %>%
  select(c(1:7)) %>%
  rename(
    val = burden.x,
    lower = burden.y,
    upper = burden
  )


# ------------------------------------------------------------
# 11. Aggregate imputed burden by disease
# ------------------------------------------------------------

imp_val <- result_val_n %>%
  select(
    cause_name,
    burden
  ) %>%
  group_by(cause_name) %>%
  mutate(
    burden = sum(
      burden,
      na.rm = TRUE
    )
  ) %>%
  distinct(
    cause_name,
    .keep_all = TRUE
  )

imp_lower <- result_lower_n %>%
  select(
    cause_name,
    burden
  ) %>%
  group_by(cause_name) %>%
  mutate(
    burden = sum(
      burden,
      na.rm = TRUE
    )
  ) %>%
  distinct(
    cause_name,
    .keep_all = TRUE
  )

imp_upper <- result_upper_n %>%
  select(
    cause_name,
    burden
  ) %>%
  group_by(cause_name) %>%
  mutate(
    burden = sum(
      burden,
      na.rm = TRUE
    )
  ) %>%
  distinct(
    cause_name,
    .keep_all = TRUE
  )

imp_result <- imp_val %>%
  merge(
    imp_lower,
    by = "cause_name"
  ) %>%
  merge(
    imp_upper,
    by = "cause_name"
  ) %>%
  rename(
    val = burden.x,
    lower = burden.y,
    upper = burden
  )


# ------------------------------------------------------------
# 12. Save outputs
# ------------------------------------------------------------

write.csv(
  result_df_n,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/impburden_cause_country_ui.csv"
)

write.csv(
  imp_result,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/impburden_cause_tot_ui.csv"
)
