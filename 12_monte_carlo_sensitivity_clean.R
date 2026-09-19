# ============================================================
# Step 12. Monte Carlo-based sensitivity analysis
# ============================================================
#
# Purpose:
#   Assess the robustness of projected health-mediated macroeconomic
#   burdens to uncertainty in key macroeconomic-model parameters.
#
# Sensitivity design:
#   - Number of simulations: 1,000
#   - Random seed: 123
#   - Parameters varied from 50% to 150% of baseline values:
#       * capital share (alpha)
#       * depreciation rate (delta)
#       * saving rate
#       * Mincer-function parameters (m1, m2, m3)
#
# Sampling structure retained from the original analysis:
#   - Capital share is sampled separately for each country from a uniform
#     0.5x-1.5x baseline range, then sorted within country and assigned
#     simulation ranks 1-1000.
#   - Saving rate is sampled separately for each country from the same
#     relative range and likewise sorted within country.
#   - Depreciation is sampled from 0.5x-1.5x its baseline value and sorted.
#   - The same simulation rank is used for capital share, saving rate, and
#     depreciation, creating a rank-aligned low-to-high parameter sequence.
#   - The three Mincer coefficients are multiplied by one common scaling
#     factor sampled uniformly from 0.5 to 1.5; this factor is not sorted.
#   - The seed is reset to 123 before each parameter-sampling block,
#     matching the original analysis.
#
# Important:
#   This is a Monte Carlo-based parameter sensitivity analysis, not a
#   bootstrap analysis.
#
# Main-analysis risks retained:
#   1. Air pollution
#   2. Unsafe water, sanitation, and handwashing (UWSH)
#   3. Non-optimal temperature
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
#   4. Labor-force participation rates by 5-year age-sex group
#      Source: International Labour Organization
#      Identifier:
#      https://www.ilo.org/global/statistics-and-databases/lang--en/index.htm
#      Download date: 2026-02-06
#
#   5. Population projections
#      Source: United Nations, Department of Economic and Social Affairs
#      Identifier:
#      https://population.un.org/wpp/Download/Standard/Population/
#      Download date: 2026-02-06
#
#   6. Age-specific educational attainment
#      Source: Barro-Lee Educational Attainment Dataset
#      Identifier: https://barrolee.github.io/BarroLeeDataSet/
#      Download date: 2026-02-06
#
#   7. Mortality, morbidity, DALYs, and population-attributable fractions
#      Source: Global Burden of Disease Study 2021 (GBD 2021)
#      Identifier: https://vizhub.healthdata.org/gbd-results
#      Download date: 2026-02-06
#
#   8. U.S. disease-specific treatment expenditure
#      Source: Dieleman JL, Cao J, Chapin A, Chen C, Li Z, Liu A, et al.
#              US Health Care Spending by Payer and Health Condition,
#              1996-2016. JAMA. 2020;323(9):863.
#      DOI: 10.1001/jama.2020.0734
#      Download date: 2026-02-06
#
#   9. Income-group and regional classification
#      Input file: CLASS.xlsx
#      Source/version: [TO FILL: exact source and classification version]
#      Download date: 2026-02-06
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       dplyr:       1.1.4
#       data.table:  1.14.10
#       future:      1.33.0
#       future.apply: 1.11.1
#       progressr:   0.14.0
#       progress:    1.2.3
#       readxl:      1.4.3
#       tidyr:       1.3.0
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages and global settings
# ------------------------------------------------------------

library(dplyr)
library(data.table)
library(future)
library(future.apply)
library(progressr)
library(progress)
library(readxl)
library(tidyr)

options(
  future.globals.maxSize = 100 * 1024^3
)

out_dir <- "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all"
cf_dir <- "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met"
sensitivity_table_dir <- "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome"

n_samples <- 1000
random_seed <- 123


# ------------------------------------------------------------
# 2. Monte Carlo parameter sampling
# ------------------------------------------------------------

# ============================================
# ============================================

### capital share
alpha <- read.csv(file.path(out_dir, "capital share.csv"))
alpha_n <- alpha[, c(2, 4)]
set.seed(random_seed)

sampling_alpha <- data.frame()
for (i in 1:nrow(alpha_n)) {
  alpha_val <- alpha_n$alpha[i]
  random_samples <- runif(n_samples, min = alpha_val * 0.5, max = alpha_val * 1.5)
  temp_result <- data.frame(WBcode = alpha_n$WBcode[i], sampled_alpha = random_samples)
  sampling_alpha <- rbind(sampling_alpha, temp_result)
}

sampling_alpha_n <- sampling_alpha %>% 
  group_by(WBcode) %>% 
  arrange(WBcode, sampled_alpha) %>% 
  mutate(samp = rep(1:1000, length.out = n())) %>% 
  group_by(samp) %>% 
  arrange(samp, WBcode)

write.csv(sampling_alpha_n, file.path(out_dir, "capital share_sensitivity.csv"))

### depreciation rate
delta <- 0.05
set.seed(random_seed)
random_samples <- runif(n_samples, min = delta * 0.5, max = delta * 1.5)
sampling_delta <- data.frame(delta = random_samples) %>% arrange(delta)
write.csv(sampling_delta, file.path(out_dir, "depreciation_sensitivity.csv"))

### saving rate
st <- read.csv(file.path(out_dir, "saving rate.csv"))[, c(2:3)]
set.seed(random_seed)
sampling_saving <- data.frame()
for (i in 1:nrow(st)) {
  sav_val <- st$sav[i]
  if (sav_val < 0) {
    min_val <- sav_val * 1.5
    max_val <- sav_val * 0.5
  } else {
    min_val <- sav_val * 0.5
    max_val <- sav_val * 1.5
  }
  if (min_val > max_val) min_val <- 0
  random_samples <- runif(n_samples, min = min_val, max = max_val)
  temp_result <- data.frame(WBcode = st$WBcode[i], sampled_sav = random_samples)
  sampling_saving <- rbind(sampling_saving, temp_result)
}

sampling_saving_n <- sampling_saving %>% 
  group_by(WBcode) %>% 
  arrange(WBcode, sampled_sav) %>% 
  mutate(samp = rep(1:1000, length.out = n())) %>% 
  group_by(samp) %>% 
  arrange(samp, WBcode)

write.csv(sampling_saving_n, file.path(out_dir, "saving rate_sensitivity.csv"))

### Mincer parameters
m1 <- 0.091
m2 <- 0.1301
m3 <- -0.0023
set.seed(random_seed)
scaling_factor <- runif(n_samples, min = 0.5, max = 1.5)
sampling_mincer <- data.frame(
  m1 = m1 * scaling_factor,
  m2 = m2 * scaling_factor,
  m3 = m3 * scaling_factor
)

rm(alpha, alpha_n, st, sampling_alpha, sampling_saving, delta, random_samples, 
   scaling_factor, m1, m2, m3, temp_result)
invisible(gc())

# ============================================
# ============================================
edu  <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/edu level_m.csv")
lt_n <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/labor participation.csv")[, c(2:6)]
nt_n <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/number_1950.csv")[, c(2:6)]

lt_c  <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/labor_cf_1950_ui.csv")
lt_cn <- lt_c[, c(2:9)]
nt_c  <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/number_cf_1950_ui.csv")
nt_cn <- nt_c[, c(2:9)]

rm(lt_c, nt_c)
invisible(gc())

# ============================================
# ============================================
# ============================================
# ============================================


setDT(edu)
setDT(lt_n)
setDT(nt_n)
setDT(lt_cn)
setDT(nt_cn)

setkey(lt_n, WBcode, sex, age, year)
setkey(nt_n, WBcode, sex, age, year)
setkey(lt_cn, WBcode, sex, age, year)
setkey(nt_cn, cause_name, WBcode, sex, age, year)

tryCatch({
  plan(multicore, workers = 16)
  message("Using multicore (fork) mode with shared memory.")
}, error = function(e) {
  plan(multisession, workers = 8)
  message("Multicore is unavailable; falling back to 8 multisession workers.")
})

handlers(global = TRUE)
handlers("progress")

with_progress({
  p <- progressor(steps = 1000)
  
  invisible(future_lapply(1:1000, function(i) {
    library(dplyr)
    library(data.table)
    
    m1 <- sampling_mincer[i, 1]
    m2 <- sampling_mincer[i, 2]
    m3 <- sampling_mincer[i, 3]
    
    ht <- edu %>%
      mutate(ageto = ifelse(ageto == 999, 70, ageto)) %>%
      mutate(ht = exp(m1 * Interpolated_Data + 
                        m2 * (((ageto + agefrom) / 2) - Interpolated_Data - 5) + 
                        m3 * (((ageto + agefrom) / 2) - Interpolated_Data - 5)^2), 
             samp = i) %>%
      filter(year_n >= 2019 & year_n <= 2050) %>%
      mutate(sex = ifelse(sex == "FALSE", "female", sex), 
             sex = ifelse(sex == "M", "male", sex),
             agefrom = recode(agefrom,
                              "15" = "15-19", "20" = "20-24", "25" = "25-29", "30" = "30-34",
                              "35" = "35-39", "40" = "40-44", "45" = "45-49", "50" = "50-54",
                              "55" = "55-59", "60" = "60-64", "65" = "65+")) %>%
      rename(age = agefrom, year = year_n) %>%
      select(WBcode, sex, age, year, ht, samp) %>%
      as.data.table()
    
    setkey(ht, WBcode, sex, age, year)
    
    dm1 <- merge(ht, lt_n, by = c('WBcode','sex','age','year'), all.y = TRUE)
    dm1 <- merge(dm1, nt_n, by = c('WBcode','sex','age','year'), all.x = TRUE)
    
    Ht_actual <- dm1 %>%
      arrange(WBcode, year, sex, age) %>%
      group_by(WBcode, year) %>%
      mutate(Ht = ht * val * number,
             Ht_a = sum(Ht),
             Ht_a = ifelse(Ht_a == 0, NA, Ht_a)) %>%
      select(WBcode, year, Ht_a) %>%
      distinct(WBcode, year, .keep_all = TRUE) %>%
      mutate(samp = i) %>%
      as.data.table()
    
    fwrite(Ht_actual, file.path(out_dir, sprintf("Ht_sensitivity_samp_%04d.csv", i)))
    
    dm2 <- merge(lt_cn, ht, by = c('WBcode','sex','age','year'), all.x = TRUE)
    dm2 <- merge(dm2, nt_cn, by = c('cause_name','WBcode','sex','age','year'), all.x = TRUE)
    
    Ht_cf <- dm2 %>%
      arrange(cause_name, WBcode, year, sex, age) %>%
      group_by(cause_name, WBcode, year) %>%
      mutate(Ht_val = ht * L_val * N_val,
             Ht_val_a = sum(Ht_val),
             Ht_val_a = ifelse(Ht_val_a == 0, NA, Ht_val_a)) %>%
      select(cause_name, WBcode, year, Ht_val_a) %>%
      distinct(cause_name, WBcode, year, .keep_all = TRUE) %>%
      mutate(samp = i) %>%
      as.data.table()
    
    fwrite(Ht_cf, file.path(out_dir, sprintf("Ht_cf_sensitivity_samp_%04d.csv", i)))
    
    rm(ht, dm1, dm2, Ht_actual, Ht_cf)
    invisible(gc())
    
    p()
    return(i)
  }, future.packages = c("dplyr", "data.table"), future.seed = TRUE))
})

plan(sequential)

# ============================================
# ============================================
GDP <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/gdp_161950_n.csv")
GDP_n <- GDP %>% select(c(2:4)) %>% subset(year > 2016)

Kt  <- read.csv(file.path(out_dir, "physical capital stock_2019.csv"))[, c(2:4)]
st  <- read.csv(file.path(out_dir, "saving rate_sensitivity.csv"))[, c(2:4)]
alpha_n <- read.csv(file.path(out_dir, "capital share_sensitivity.csv"))[, c(2:4)]
delta_samp <- read.csv(file.path(out_dir, "depreciation_sensitivity.csv"))

Kt_n <- Kt
for (year in 2020:2050) {
  new_data <- Kt
  new_data$year <- year
  new_data$cn <- 0
  Kt_n <- rbind(Kt_n, new_data)
}
rm(Kt, GDP)
invisible(gc())

plan(multisession, workers = 16)

with_progress({
  p <- progressor(steps = 1000)
  
  invisible(future_lapply(1:1000, function(i) {
    library(dplyr)
    library(data.table)
    
    Ht_file <- file.path(out_dir, sprintf("Ht_sensitivity_samp_%04d.csv", i))
    Ht_n <- fread(Ht_file)
    
    st_n <- st %>% subset(samp == i) %>% select(WBcode, sampled_sav)
    delta <- delta_samp[i, 2]
    alpha_samp <- alpha_n %>% subset(samp == i) %>% select(WBcode, sampled_alpha)
    
    output <- Kt_n %>%
      merge(GDP_n, by = c("WBcode", "year"), all.y = TRUE) %>%
      merge(st_n, by = "WBcode", all.x = TRUE) %>%
      arrange(WBcode, year) %>%
      group_by(WBcode) %>%
      mutate(grp = cur_group_id())
    
    proj_K <- lapply(split(output, output$grp), function(tmp) {
      for (j in 2:nrow(tmp)) {
        tmp[j, 3] <- tmp[j - 1, 5] * tmp[j - 1, 4] + tmp[j - 1, 3] * (1 - delta)
      }
      tmp
    }) %>% bind_rows()
    
    At <- proj_K %>%
      merge(Ht_n, by = c("WBcode", "year"), all.x = TRUE) %>%
      merge(alpha_samp, by = "WBcode") %>%
      arrange(WBcode, year) %>%
      mutate(tech = val.gdp / (cn ^ sampled_alpha) / (Ht_a ^ (1 - sampled_alpha)),
             samp = i) %>%
      select(WBcode, year, tech, samp)
    
    fwrite(as.data.frame(At), 
           file.path(out_dir, sprintf("At_sensitivity_samp_%04d.csv", i)))
    
    p()
    return(i)
  }, future.packages = c("dplyr", "data.table"), future.seed = TRUE))
})

rm(GDP_n, Kt_n, st, alpha_n, delta_samp)
invisible(gc())
plan(sequential)

# ============================================
# ============================================
# ============================================
#
# 1. 16 workers -> 4 workers
# ============================================


# ------------------------------------------------
# ------------------------------------------------

TCt <- read.csv(
  file.path(cf_dir, "treatment cost_1950_n2.csv")
)[, c(2:5)]

TCt$expense <- TCt$expense * 1000000000


GDP <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/gdp_161950_n.csv"
)

GDP_n <- GDP %>%
  select(c(2:4)) %>%
  subset(year > 2016)


Kt <- read.csv(
  file.path(out_dir, "physical capital stock_2019.csv")
)[, c(2:4)]


st <- read.csv(
  file.path(out_dir, "saving rate_sensitivity.csv")
)[, c(2:4)]


alpha_n <- read.csv(
  file.path(out_dir, "capital share_sensitivity.csv")
)[, c(2:4)]


delta_samp <- read.csv(
  file.path(out_dir, "depreciation_sensitivity.csv")
)


# ------------------------------------------------
# ------------------------------------------------

Kt_n <- Kt

for (year in 2020:2050) {
  
  new_data <- Kt
  
  new_data$year <- year
  new_data$cn <- 0
  
  Kt_n <- rbind(Kt_n, new_data)
}

rm(new_data)


# ------------------------------------------------
# ------------------------------------------------

setDT(TCt)
setDT(GDP_n)
setDT(Kt_n)
setDT(st)
setDT(alpha_n)
setDT(delta_samp)


GDP_n <- GDP_n[
  year >= 2020 & year <= 2050
]


setkey(Kt_n, WBcode, year)
setkey(GDP_n, WBcode, year)
setkey(TCt, cause_name, WBcode, year)


invisible(gc())


# ============================================================
#
# ============================================================

all_samples <- 1:1000

burden_paths <- file.path(
  cf_dir,
  sprintf("burden_samp_%04d.csv", all_samples)
)

completed <- file.exists(burden_paths)

todo_samples <- all_samples[!completed]


message(
  "Completed: ", sum(completed),
  " / 1000; remaining: ", length(todo_samples)
)


if (length(todo_samples) > 0) {
  
  
  # ==========================================================
  # ==========================================================
  
  workers_part5 <- 4L
  
  batch_size <- 20L
  
  
  sample_batches <- split(
    todo_samples,
    ceiling(seq_along(todo_samples) / batch_size)
  )
  
  
  # ==========================================================
  # ==========================================================
  
  for (batch_id in seq_along(sample_batches)) {
    
    
    current_samples <- sample_batches[[batch_id]]
    
    
    message(
      "\n=========================================="
    )
    
    message(
      "Part 5 batch ",
      batch_id,
      " / ",
      length(sample_batches),
      " | samples: ",
      min(current_samples),
      " - ",
      max(current_samples)
    )
    
    message(
      "=========================================="
    )
    
    
    # --------------------------------------------------------
    # --------------------------------------------------------
    
    n_workers <- min(
      workers_part5,
      length(current_samples)
    )
    
    
    plan(
      multisession,
      workers = n_workers
    )
    
    
    # --------------------------------------------------------
    # --------------------------------------------------------
    
    future_lapply(
      
      current_samples,
      
      function(i) {
        
        library(data.table)
        
        
        data.table::setDTthreads(1)
        
        
        # ====================================================
        # ====================================================
        
        Ht_cf_file <- file.path(
          out_dir,
          sprintf(
            "Ht_cf_sensitivity_samp_%04d.csv",
            i
          )
        )
        
        
        At_file <- file.path(
          out_dir,
          sprintf(
            "At_sensitivity_samp_%04d.csv",
            i
          )
        )
        
        
        # ====================================================
        # ====================================================
        
        Ht_val_n <- fread(
          Ht_cf_file,
          select = c(
            "cause_name",
            "WBcode",
            "year",
            "Ht_val_a"
          )
        )
        
        
        all_At_n <- fread(
          At_file,
          select = c(
            "WBcode",
            "year",
            "tech"
          )
        )
        
        
        # ====================================================
        # ====================================================
        
        st_n <- st[
          samp == i,
          .(
            WBcode,
            sampled_sav
          )
        ]
        
        
        alpha_samp <- alpha_n[
          samp == i,
          .(
            WBcode,
            sampled_alpha
          )
        ]
        
        
        delta <- delta_samp[[2]][i]
        
        
        # ====================================================
        # D. Merge
        # ====================================================
        
        output_c <- merge(
          Ht_val_n,
          Kt_n,
          by = c(
            "WBcode",
            "year"
          ),
          all = FALSE,
          sort = FALSE
        )
        
        
        rm(Ht_val_n)
        invisible(gc())
        
        
        output_c <- merge(
          output_c,
          TCt,
          by = c(
            "cause_name",
            "WBcode",
            "year"
          ),
          all = FALSE,
          sort = FALSE
        )
        
        
        output_c <- merge(
          output_c,
          all_At_n,
          by = c(
            "WBcode",
            "year"
          ),
          all = FALSE,
          sort = FALSE
        )
        
        
        rm(all_At_n)
        invisible(gc())
        
        
        output_c <- merge(
          output_c,
          st_n,
          by = "WBcode",
          all = FALSE,
          sort = FALSE
        )
        
        
        output_c <- merge(
          output_c,
          alpha_samp,
          by = "WBcode",
          all = FALSE,
          sort = FALSE
        )
        
        
        rm(
          st_n,
          alpha_samp
        )
        
        invisible(gc())
        
        
        output_c <- output_c[
          year >= 2019 &
            year <= 2050
        ]
        
        
        setorder(
          output_c,
          cause_name,
          WBcode,
          year
        )
        
        
        # ====================================================
        #
        #
        # proj_output_c <- ...
        # tmp <- subset(...)
        # proj_output_c <- rbind(...)
        # ====================================================
        
        output_c[
          
          ,
          
          Yt := {
            
            n <- .N
            
            
            K_path <- as.numeric(cn)
            
            
            Y_path <- numeric(n)
            
            
            for (k in seq_len(n)) {
              
              
              # --------------------------------------------
              #
              # Yt =
              # tech *
              # capital^alpha *
              # human_capital^(1-alpha)
              # --------------------------------------------
              
              Y_path[k] <-
                
                tech[k] *
                
                (
                  K_path[k] ^
                    sampled_alpha[k]
                ) *
                
                (
                  Ht_val_a[k] ^
                    (
                      1 -
                        sampled_alpha[k]
                    )
                )
              
              
              # --------------------------------------------
              #
              #
              # K(t+1) =
              # (1-delta)*Kt +
              # saving*Yt +
              # saving*treatment expense
              # --------------------------------------------
              
              if (k < n) {
                
                K_path[k + 1] <-
                  
                  (
                    1 -
                      delta
                  ) *
                  
                  K_path[k] +
                  
                  sampled_sav[k] *
                  Y_path[k] +
                  
                  sampled_sav[k] *
                  expense[k]
                
              }
              
            }
            
            
            Y_path
            
          },
          
          by = .(
            cause_name,
            WBcode
          )
          
        ]
        
        
        # ====================================================
        #
        # ====================================================
        
        burden_data <- output_c[
          
          year >= 2020 &
            year <= 2050,
          
          .(
            cause_name,
            WBcode,
            year,
            Yt
          )
          
        ]
        
        
        rm(output_c)
        
        invisible(gc())
        
        
        # Yt == 0 -> NA
        burden_data[
          Yt == 0,
          Yt := NA_real_
        ]
        
        
        # ====================================================
        # ====================================================
        
        burden_data <- merge(
          burden_data,
          GDP_n,
          by = c(
            "WBcode",
            "year"
          ),
          all = FALSE,
          sort = FALSE
        )
        
        
        # ====================================================
        # ====================================================
        
# ====================================================
# ====================================================

        burden <- burden_data[
          ,
          .(
            burden = {
              
              diff_y <- Yt - val.gdp
              
              if (anyNA(diff_y)) {
                NA_real_
              } else {
                sum(diff_y)
              }
              
            }
          ),
          by = .(
            cause_name,
            WBcode
          )
        ]
        
        burden[, samp := i]
        
        
        setcolorder(
          burden,
          c(
            "samp",
            "cause_name",
            "WBcode",
            "burden"
          )
        )
        
        
        # ====================================================
        #
        #
        # ====================================================
        
        final_file <- file.path(
          cf_dir,
          sprintf(
            "burden_samp_%04d.csv",
            i
          )
        )
        
        
        temp_file <- paste0(
          final_file,
          ".tmp_",
          Sys.getpid()
        )
        
        
        fwrite(
          burden,
          temp_file
        )
        
        
        if (!file.rename(
          temp_file,
          final_file
        )) {
          
          stop(
            "Unable to rename temporary file to: ",
            final_file
          )
          
        }
        
        
        # ====================================================
        # ====================================================
        
        rm(
          burden_data,
          burden
        )
        
        invisible(gc())
        
        
        return(i)
        
      },
      
      
      future.packages = "data.table",
      
      future.seed = TRUE
      
    )
    
    
    # ========================================================
    #
    # ========================================================
    
    plan(sequential)
    
    
    invisible(gc())
    
    
    message(
      "Batch ",
      batch_id,
      " completed. Total completed: ",
      sum(
        file.exists(
          file.path(
            cf_dir,
            sprintf(
              "burden_samp_%04d.csv",
              1:1000
            )
          )
        )
      ),
      " / 1000"
    )
    
  }
  
}


# ============================================================
# ============================================================

plan(sequential)


rm(
  GDP,
  GDP_n,
  Kt,
  Kt_n,
  st,
  alpha_n,
  delta_samp,
  TCt
)

invisible(gc())
# ============================================
# ============================================
burden_files <- list.files(cf_dir, pattern = "burden_samp_\\d{4}\\.csv", full.names = TRUE)
output_tot <- rbindlist(lapply(burden_files, fread))
write.csv(output_tot, file.path(cf_dir, "burden_cause_country_samp.csv"))

rm(burden_files)
invisible(gc())

# ============================================
# ============================================
GDP <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/gdp_161950_n.csv")
GDP_n <- GDP %>% select(c(2:4)) %>% subset(year > 2019) %>% 
  group_by(WBcode) %>% 
  mutate(totgdp = sum(val.gdp)) %>% 
  select(c(1, 4)) %>% 
  distinct(WBcode, .keep_all = TRUE)

country <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv")
country_n <- country %>% select(c(2,3)) %>% rename(country = location_name, WBcode = Country.Code)

disease_name <- unique(output_tot$cause_name)
country_code <- unique(GDP_n$WBcode)

daly <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/DALYs/air&met/daly_rate2.csv')
country_n_full <- country %>% select(c(2,3,4)) %>% rename(WBcode = Country.Code)
daly_n <- daly %>% 
  merge(country_n_full, by = 'location_id') %>% 
  select(c(10,17,18,13,14)) %>% 
  arrange(cause_name, WBcode, year)

proj <- function(data){
  data_1019_n <- data %>%
    arrange(cause_name, WBcode, year) %>%
    group_by(cause_name, WBcode) %>%
    mutate(rate = (val - lag(val)) / lag(val)) %>%
    mutate(rate_m = mean(rate, na.rm = TRUE)) %>%
    mutate(rate_m = ifelse(rate_m > 0.02, 0.02, rate_m))
  
  data_19 <- data_1019_n %>% subset(year == 2019) %>% select(!c(rate))
  result_list <- list()
  for (i in 1:31) {
    mutated_data <- data_19 %>% 
      mutate(val = val * ((1+rate_m)^i), year = 2019 + i) %>% 
      select(-rate_m)
    result_list[[i]] <- mutated_data
  }
  final_data <- do.call(rbind, result_list)
  final_data <- data_19 %>% rbind(final_data) %>% select(-rate_m) %>% subset(year > 2019)
  return(final_data)
}

daly_2050 <- daly_n %>% 
  proj(.) %>% 
  arrange(cause_name, WBcode, year) %>% 
  group_by(cause_name, WBcode) %>% 
  mutate(daly_m = mean(val) / 100000) %>% 
  select(c(1,2,3,4,6)) %>% 
  distinct(cause_name, WBcode, .keep_all = TRUE)

fill_2050 <- function(a){
  all_combinations <- expand.grid(cause_name = disease_name, WBcode = country_code)
  result_df <- all_combinations %>%
    merge(a, by = c('cause_name','WBcode'), all.x = TRUE) %>%
    arrange(cause_name, WBcode)
  return(result_df)
}

result_list <- vector("list", 1000)

pb <- progress_bar$new(
  format = "Processing Imputation [:bar] :percent | :current/:total | eta: :eta",
  total = 1000, width = 60)

for (k in 1:1000) {
  pb$tick()
  data <- output_tot %>% ungroup() %>% subset(samp == k) %>% select(cause_name, WBcode, burden)
  imp <- data %>% fill_2050(.) %>% merge(GDP_n, by = 'WBcode', all.x = TRUE) %>% mutate(percent = burden / totgdp)
  imp_n <- imp %>% merge(daly_2050, by = c('cause_name','WBcode'))
  
  result_df <- data.frame()
  for (i in 1:length(disease_name)) {
    df <- imp_n %>% subset(cause_name == disease_name[i])
    df_no_missing <- na.omit(df[, c("percent", "daly_m")])
    
    if (nrow(df_no_missing) == 0) {
      message(paste0("Skipping disease: ", disease_name[i], " -- no complete observations are available for model fitting."))
      df_n <- df %>% mutate(predicted_b = NA, percent = percent, burden = burden)
    } else {
      linear_model <- lm(percent ~ daly_m, data = df_no_missing)
      df$predicted_b <- predict(linear_model, newdata = df)
      df_n <- df %>% 
        mutate(percent = ifelse(is.na(percent), predicted_b, percent)) %>%
        mutate(burden = ifelse(is.na(burden), percent * totgdp, burden))
    }
    result_df <- rbind(result_df, df_n)
  }
  
  result_df_n <- result_df %>% 
    merge(country_n, by = 'WBcode') %>% 
    select(c(1:7, 9)) %>% 
    mutate(samp = k)
  
  result_list[[k]] <- as.data.frame(result_df_n)
}

all_result_df_n <- rbindlist(result_list)
all_result_df_n <- all_result_df_n %>% rename(country = location_name.y)


imp_country <- all_result_df_n %>% 
  select(country, WBcode, cause_name, samp, burden, totgdp) %>% 
  group_by(country, cause_name) %>% 
  arrange(country, cause_name, samp) %>% 
  summarise(
    lower_95UI = quantile(burden, probs = 0.025),
    upper_95UI = quantile(burden, probs = 0.975)
  )

imp_country <- imp_country %>% 
  left_join(country %>% select(location_name, location_id),
            by = c("country" = "location_name"))

imp_all <- all_result_df_n %>% 
  select(c(6,2,3,9)) %>% 
  group_by(samp, cause_name) %>%
  mutate(burden = sum(burden, na.rm = TRUE)) %>%
  distinct(samp, cause_name, .keep_all = TRUE) %>%
  group_by(cause_name) %>% 
  arrange(cause_name, samp) %>% 
  summarise(
    lower_95UI = quantile(burden, probs = 0.025),
    upper_95UI = quantile(burden, probs = 0.975)
  )

write.csv(imp_country, file.path(out_dir, "impburden_cause_country_sens.csv"))
write.csv(imp_all, file.path(out_dir, "impburden_cause_tot_sens.csv"))

rm(result_list, result_df, result_df_n, imp, imp_n, data, output_tot, daly, daly_n, daly_2050, GDP, GDP_n)
invisible(gc())

# ============================================
# ============================================
# ============================================

###############################
###############################
# 9. Apply PAFs to sensitivity intervals
# PAF

paf_air <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/PAF/air&met/air/IHME-GBD_2021_DATA-b54e5652-1.csv")
paf_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/PAF/air&met/water/IHME-GBD_2021_DATA-e0be27d9-1.csv")
paf_tem <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/PAF/air&met/temperature/IHME-GBD_2021_DATA-39726ee5-1.csv")

paf_air_n <- paf_air %>% mutate(val=ifelse(lower<0,0,val)) %>%
  select(.,c(12,10,3,16))  %>%
  arrange(.,rei_name,cause_name,location_id) %>% filter(cause_name!='Asthma'&cause_name!='Blindness and vision loss'&cause_name!='Otitis media'&cause_name!='Upper respiratory infections')%>%
  filter(rei_name=='Air pollution')

# test_country <- unique(paf_air_n$location_id)#204

paf_wat_n <- paf_wat %>% mutate(val=ifelse(lower<0,0,val)) %>%
  select(.,c(12,10,3,16))  %>%
  arrange(.,rei_name,cause_name,location_id) %>% mutate(cause_name = ifelse(cause_name == 'Diarrheal diseases', 'Enteric infections', cause_name)) %>% 
  filter(cause_name!='Lower respiratory infections') %>% filter(rei_name=='Unsafe water, sanitation, and handwashing')

paf_tem_n <- paf_tem  %>% mutate(val=ifelse(lower<0,0,val)) %>%
  select(.,c(12,10,3,16))  %>%
  arrange(.,rei_name,cause_name,location_id) %>%filter(cause_name!='Exposure to mechanical forces'&cause_name!='Other unintentional injuries') %>%
  filter(rei_name=='Non-optimal temperature') 

# test_country <- unique(paf_tem_n$location_id)#204

imp_country <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/impburden_cause_country_sens.csv")
#--------------------#
# Air pollution
#--------------------#
air_burden <- imp_country %>% select(-X) %>% merge(.,paf_air_n,by=c('cause_name','location_id'),all.x=TRUE) %>%
    mutate(lower=lower_95UI*val,upper=upper_95UI*val) %>%
    arrange(.,country,cause_name) %>% group_by(.,country) %>%
    mutate(lower_n=sum(lower)/10^6,upper_n=sum(upper)/10^6) %>%
    select(.,c('country','lower_n','upper_n')) %>% rename(lower=lower_n,upper=upper_n) %>%
    distinct(.,country, .keep_all = TRUE)

#   mutate(lower=lower_95UI*val,upper=upper_95UI*val) %>%
#   arrange(.,country,cause_name)  %>%group_by(.,country) %>%
#   mutate(lower_n=sum(lower)/10^6,upper_n=sum(upper)/10^6) %>%select(.,c(3,10,11))%>% 
#   select(.,c('country','lower_n','upper_n')) %>% rename(lower=lower_n,upper=upper_n) %>%
#   distinct(country, .keep_all = TRUE)
# test_country <- unique(air_burden$country)#204
# test_country <- unique(air_burden$cause_name)#204

write.csv(air_burden, "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_sens_1000.csv")

#--------------------#
# Water pollution
#--------------------#
wat_burden <- imp_country %>% select(-X) %>% merge(.,paf_wat_n,by=c('cause_name','location_id'),all.x=TRUE) %>%
  mutate(lower=lower_95UI*val,upper=upper_95UI*val) %>%
  arrange(.,country,cause_name) %>% group_by(.,country) %>%
  mutate(lower_n=sum(lower)/10^6,upper_n=sum(upper)/10^6) %>%
  select(.,c('country','lower_n','upper_n')) %>% rename(lower=lower_n,upper=upper_n) %>%
  distinct(.,country, .keep_all = TRUE)

write.csv(wat_burden, "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_sens_1000.csv")

#--------------------#
# Temperature exposure
#--------------------#
tem_burden <- imp_country %>% select(-X) %>% merge(.,paf_tem_n,by=c('cause_name','location_id'),all.x=TRUE) %>%
  mutate(lower=lower_95UI*val,upper=upper_95UI*val) %>%
  arrange(.,country,cause_name) %>% group_by(.,country) %>%
  mutate(lower_n=sum(lower)/10^6,upper_n=sum(upper)/10^6) %>%
  select(.,c('country','lower_n','upper_n')) %>% rename(lower=lower_n,upper=upper_n) %>%
  distinct(.,country, .keep_all = TRUE)

write.csv(tem_burden, "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_sens_1000.csv")

#   mutate(lower=lower_95UI*val,upper=upper_95UI*val) %>%
#   arrange(.,country,cause_name)  %>%group_by(.,country) %>%
#   mutate(lower_n=sum(lower)/10^6,upper_n=sum(upper)/10^6) %>%select(.,c(3,10,11))%>% 
#   select(.,c('country','lower_n','upper_n')) %>% rename(lower=lower_n,upper=upper_n) %>%
#   distinct(country, .keep_all = TRUE)
# 
# write.csv(tem_burden, "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_low_sens.csv")

#####################diseases sens#####################
##air
##14 causes
cause_air <- imp_country %>% select(-X) %>% merge(.,paf_air_n,by=c('cause_name','location_id'),all.x=TRUE) %>%
  mutate(lower=lower_95UI*val/10^6,upper=upper_95UI*val/10^6) %>% 
  arrange(.,cause_name,location_id) %>% select(cause_name,location_id,lower,upper)
write.csv(cause_air,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/cause_air_sens.csv")

cause_wat <- imp_country %>% select(-X) %>% merge(.,paf_wat_n,by=c('cause_name','location_id'),all.x=TRUE) %>%
  mutate(lower=lower_95UI*val/10^6,upper=upper_95UI*val/10^6) %>%
  arrange(.,cause_name,location_id) %>% select(cause_name,location_id,lower,upper)
write.csv(cause_wat,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/cause_wat_sens.csv")

cause_tem <- imp_country %>% select(-X) %>% merge(.,paf_tem_n,by=c('cause_name','location_id'),all.x=TRUE) %>%
  mutate(lower=lower_95UI*val/10^6,upper=upper_95UI*val/10^6) %>%
  arrange(.,cause_name,location_id) %>% select(cause_name,location_id,lower,upper)
write.csv(cause_tem,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/cause_tem_sens.csv")
##################
#################
# 10. Sensitivity-analysis summary tables
# Tables
num1 <- read_xlsx("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/population/population_both.xlsx", sheet = 'Estimates',col_names = F)
colname <- read_xlsx("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/population/population_both.xlsx", sheet = 'Estimates',col_names = F,n_max = 1)
colnames(num1) <- colname
num1 <- num1[-1,]
num2 <- read_xlsx("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/population/population_both.xlsx", sheet = 'Medium variant',col_names = F)
colnames(num2) <- colname
num2 <- num2[-1,]
num <- num1 %>%
  rbind(num2)

num_n <- num %>%
  subset(.,Year>=2020 & Year<=2050) %>%
  rename(country=`Region, subregion, country or area *`,WBcode=`ISO3 Alpha-code`,year=Year) %>%
  select(.,c('country','WBcode',11:32)) %>%
  arrange(.,country,year) %>%
  mutate(across(3:24, as.numeric),) %>%
  pivot_longer(cols = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39',
                        '40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84'
                        ,'85-89','90-94','95-99','100+'), names_to = "age", values_to = "number") %>%
  group_by(country,year) %>% mutate(num=sum(number)) %>%
  distinct(.,country,year,.keep_all = TRUE) %>% group_by(country) %>%
  mutate(num_m =mean(num)*1000) %>% select('country','WBcode','num_m') %>%
  distinct(.,country,.keep_all = TRUE)

####Total by country
cost_air <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_rei.csv')###in million$
cost_wat <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_rei.csv')###in million$
cost_tem <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_rei.csv')###in million$

cost2_air <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_sens_1000.csv")
cost2_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_sens_1000.csv")
cost2_tem <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_sens_1000.csv")

country <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv")

cost_air <- cost_air %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)
cost_air <- cost_air %>% subset(rei_name=='Air pollution') 

cost_wat <- cost_wat %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)
cost_wat <- cost_wat %>% subset(rei_name=='Unsafe water, sanitation, and handwashing') 

cost_tem <- cost_tem %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)
cost_tem <- cost_tem %>% subset(rei_name=='Non-optimal temperature') 

country_n <- country %>% select(.,c(2,3)) %>% rename(WBcode=Country.Code,country=location_name)
gdp <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/gdp_161950_n.csv')
gdp_country <- gdp %>% select(.,c(2:4)) %>% subset(.,year>2019) %>% merge(country_n,by='WBcode',all.y = TRUE) %>%
  group_by(WBcode) %>% mutate(val.gdp=sum(val.gdp)/10^6) %>% select(-year) %>% distinct(.,WBcode,country,.keep_all = TRUE)

inc_grp <- read_xlsx("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/income group/CLASS.xlsx",sheet = 1)
inc_grp_n <- inc_grp %>% 
  select(.,c(2:4)) %>%
  head(., n = 218) %>%
  rename(WBcode=Code,income=`Income group`) %>%
  merge(country_n,by='WBcode',all.y = TRUE) %>%
  mutate(income=ifelse(WBcode=='COK','Others',income),
         income=ifelse(WBcode=='NIU','Others',income),
         income=ifelse(WBcode=='TKL','Others',income),
         income=ifelse(WBcode=='VEN','Others',income),
         Region=ifelse(WBcode=='COK','Others',Region),
         Region=ifelse(WBcode=='NIU','Others',Region),
         Region=ifelse(WBcode=='TKL','Others',Region),
         Region=ifelse(WBcode=='VEN','Others',Region))

air_country <- cost_air %>% select(-c(lower,upper)) %>% merge(.,cost2_air,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(.,Region,country.x) %>%
  mutate(num_m=num_m/10^6,
         percent_val=round(burden/val.gdp*100,3),burden=round(burden),
         percent_lower=round(lower/val.gdp*100,3),lower=round(lower),
         percent_upper=round(upper/val.gdp*100,3),upper=round(upper),
         capital_val=round(burden/num_m),
         capital_lower=round(lower/num_m),
         capital_upper=round(upper/num_m),
         cost=paste(burden,' ',"(", lower, ", ", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, ", ", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, ", ", capital_upper, ")", sep="")) %>%
  select(c('Region','country.x','cost','percent','capital'))

write.csv(air_country,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/air_country_sens_1000.csv")

wat_country <- cost_wat %>% select(-c(lower,upper)) %>% merge(.,cost2_wat,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(.,Region,country.x) %>%
  mutate(num_m=num_m/10^6,
         percent_val=round(burden/val.gdp*100,3),burden=round(burden),
         percent_lower=round(lower/val.gdp*100,3),lower=round(lower),
         percent_upper=round(upper/val.gdp*100,3),upper=round(upper),
         capital_val=round(burden/num_m),
         capital_lower=round(lower/num_m),
         capital_upper=round(upper/num_m),
         cost=paste(burden,' ',"(", lower, ", ", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, ", ", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, ", ", capital_upper, ")", sep="")) %>%
  select(c('Region','country.x','cost','percent','capital'))

write.csv(wat_country,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/wat_country_sens_1000.csv")

tem_country <- cost_tem %>% select(-c(lower,upper)) %>% merge(.,cost2_tem,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(.,Region,country.x) %>%
  mutate(num_m=num_m/10^6,
         percent_val=round(burden/val.gdp*100,3),burden=round(burden),
         percent_lower=round(lower/val.gdp*100,3),lower=round(lower),
         percent_upper=round(upper/val.gdp*100,3),upper=round(upper),
         capital_val=round(burden/num_m),
         capital_lower=round(lower/num_m),
         capital_upper=round(upper/num_m),
         cost=paste(burden,' ',"(", lower, ", ", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, ", ", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, ", ", capital_upper, ")", sep="")) %>%
  select(c('Region','country.x','cost','percent','capital'))

write.csv(tem_country,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/tem_country_sens_1000.csv")


####################################################################3
##Table 2 V2: by income group & region
air_inc <- cost_air %>% select(-c(lower,upper)) %>% merge(.,cost2_air,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = c('country')) %>%
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%arrange(.,income,Region) %>% 
  group_by(.,income) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,income,.keep_all = TRUE) %>%
  select(.,c('income','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('income','cost','percent','capital')) 

air_region <- cost_air %>% select(-c(lower,upper)) %>% merge(.,cost2_air,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = c('country')) %>%
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(.,Region) %>% 
  group_by(.,Region) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,Region,.keep_all = TRUE) %>%
  select(.,c('Region','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('Region','cost','percent','capital')) %>%
  rbind(air_inc)

write.csv(air_region,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/air_inc&region_sens_1000.csv")

##water
wat_inc <- cost_wat %>% select(-c(lower,upper)) %>% merge(.,cost2_wat,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = c('country')) %>%
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%arrange(.,income,Region) %>% 
  group_by(.,income) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,income,.keep_all = TRUE) %>%
  select(.,c('income','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('income','cost','percent','capital')) 

wat_region <- cost_wat %>% select(-c(lower,upper)) %>% merge(.,cost2_wat,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = c('country')) %>%
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(.,Region) %>% 
  group_by(.,Region) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,Region,.keep_all = TRUE) %>%
  select(.,c('Region','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('Region','cost','percent','capital')) %>%
  rbind(wat_inc)

write.csv(wat_region,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/wat_inc&region_sens_1000.csv")

##tem
tem_inc <- cost_tem %>% select(-c(lower,upper)) %>% merge(.,cost2_tem,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = c('country')) %>%
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%arrange(.,income,Region) %>% 
  group_by(.,income) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,income,.keep_all = TRUE) %>%
  select(.,c('income','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('income','cost','percent','capital')) 

tem_region <- cost_tem %>% select(-c(lower,upper)) %>% merge(.,cost2_tem,by = 'country') %>%
  select(-X) %>% merge(.,inc_grp_n,by = c('country')) %>%
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(.,Region) %>% 
  group_by(.,Region) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,Region,.keep_all = TRUE) %>%
  select(.,c('Region','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('Region','cost','percent','capital')) %>%
  rbind(tem_inc)

write.csv(tem_region,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/tem_inc&region_sens_1000.csv")


##############by diseases
air_cause <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_allcause.csv")
wat_cause <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_allcause.csv")
tem_cause <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_allcause.csv")

cause_sens_air <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/cause_air_sens.csv")
cause_sens_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/cause_wat_sens.csv")
cause_sens_tem <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/cause_tem_sens.csv")

cause_sens_air <-cause_sens_air %>% rename(country=location_id)
cause_sens_wat <-cause_sens_wat %>% rename(country=location_id)
cause_sens_tem <-cause_sens_tem %>% rename(country=location_id)

##air
air_disease <- air_cause %>% select(-c(X,lower,upper)) %>% merge(.,cause_sens_air,by = c('cause_name','country')) %>%
  select(-X)  %>%rename(location_id = country)
air_disease <- air_disease %>%
  left_join(country %>% select(location_id, location_name),
            by = "location_id") %>%
  rename(country = location_name)

air_disease <- air_disease %>% merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(cause_name,country.x) %>% 
  group_by(.,cause_name) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                    ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,cause_name,.keep_all = TRUE) %>%
  select(.,c('cause_name','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,4),
         percent_lower=round(lower/gdp*100,4),
         percent_upper=round(upper/gdp*100,4),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cause_name','cost','percent','capital'))

write.csv(air_disease,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/air_diseases_sens_1000.csv")

##wat
wat_disease <- wat_cause %>% select(-c(X,lower,upper)) %>% merge(.,cause_sens_wat,by = c('cause_name','country')) %>%
  select(-X)  %>%rename(location_id = country)
wat_disease <- wat_disease %>%
  left_join(country %>% select(location_id, location_name),
            by = "location_id") %>%
  rename(country = location_name)

wat_disease <- wat_disease %>% merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(cause_name,country.x) %>% 
  group_by(.,cause_name) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                    ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,cause_name,.keep_all = TRUE) %>%
  select(.,c('cause_name','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,4),
         percent_lower=round(lower/gdp*100,4),
         percent_upper=round(upper/gdp*100,4),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cause_name','cost','percent','capital'))

write.csv(wat_disease,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/wat_diseases_sens_1000.csv")

#tem
tem_disease <- tem_cause %>% select(-c(X,lower,upper)) %>% merge(.,cause_sens_tem,by = c('cause_name','country')) %>%
  select(-X)  %>%rename(location_id = country)
tem_disease <- tem_disease %>%
  left_join(country %>% select(location_id, location_name),
            by = "location_id") %>%
  rename(country = location_name)

tem_disease <- tem_disease %>% merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>% arrange(cause_name,country.x) %>% 
  group_by(.,cause_name) %>% mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
                                    ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,cause_name,.keep_all = TRUE) %>%
  select(.,c('cause_name','burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,4),
         percent_lower=round(lower/gdp*100,4),
         percent_upper=round(upper/gdp*100,4),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cause_name','cost','percent','capital'))

write.csv(tem_disease,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/tem_diseases_sens_1000.csv")

###############
air_tot <- cost_air %>% select(-c(lower,upper)) %>% merge(.,cost2_air,by = 'country') %>%
  select(-X) %>%  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))
write.csv(air_tot,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/air_tot_sens_1000.csv")

wat_tot <- cost_wat %>% select(-c(lower,upper)) %>% merge(.,cost2_wat,by = 'country') %>%
  select(-X) %>%  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))
write.csv(wat_tot,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/wat_tot_sens_1000.csv")

tem_tot <- cost_tem %>% select(-c(lower,upper)) %>% merge(.,cost2_tem,by = 'country') %>%
  select(-X) %>%  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))
write.csv(tem_tot,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/tem_tot_sens_1000.csv")

# ============================================================
#
#
# air_apm_sens_1000.csv
# air_hap_sens_1000.csv
# air_aop_sens_1000.csv
# water_uws_sens_1000.csv
# water_us_sens_1000.csv
# water_nahf_sens_1000.csv
# tem_high_sens_1000.csv
# tem_low_sens_1000.csv
# ============================================================


# ------------------------------------------------------------
# ------------------------------------------------------------

imp_country_sub <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/impburden_cause_country_sens.csv"
)


# ------------------------------------------------------------
#
#
#
# ------------------------------------------------------------

calc_subrisk_sens <- function(
    imp_country_new,
    paf_data,
    rei_target,
    exclude_causes = NULL,
    rename_diarrheal = FALSE
) {
  
  paf_sub <- paf_data %>%
    mutate(
      val = ifelse(lower < 0, 0, val)
    ) %>%
    select(
      rei_name,
      cause_name,
      location_id,
      val
    )
  
  if (rename_diarrheal) {
    
    paf_sub <- paf_sub %>%
      mutate(
        cause_name = ifelse(
          cause_name == "Diarrheal diseases",
          "Enteric infections",
          cause_name
        )
      )
  }
  
  
  if (!is.null(exclude_causes)) {
    
    paf_sub <- paf_sub %>%
      filter(
        !cause_name %in% exclude_causes
      )
  }
  
  
  paf_sub <- paf_sub %>%
    filter(
      rei_name == rei_target
    )
  
  
  # ------------------------------------------
  # ------------------------------------------
  
  if (nrow(paf_sub) == 0) {
    
    stop(
      paste0(
        "Risk factor not found in the PAF data: ",
        rei_target,
        "\nCheck unique(paf_data$rei_name) for the available names."
      )
    )
  }
  
  
  # ------------------------------------------
  # ------------------------------------------
  
  result <- imp_country_new %>%
    select(
      country,
      cause_name,
      location_id,
      lower_95UI,
      upper_95UI
    ) %>%
    inner_join(
      paf_sub %>%
        select(
          cause_name,
          location_id,
          val
        ),
      by = c(
        "cause_name",
        "location_id"
      )
    ) %>%
    mutate(
      lower = lower_95UI * val,
      upper = upper_95UI * val
    ) %>%
    group_by(country) %>%
    summarise(
      lower = sum(lower) / 10^6,
      upper = sum(upper) / 10^6,
      .groups = "drop"
    )
  
  
  return(result)
}


# ============================================================
# 3. AIR POLLUTION
# ============================================================

air_exclude <- c(
  "Asthma",
  "Blindness and vision loss",
  "Otitis media",
  "Upper respiratory infections"
)


# ------------------------------------------------------------
# APM
# Ambient particulate matter pollution
# ------------------------------------------------------------

air_apm_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_air,
  rei_target = "Ambient particulate matter pollution",
  exclude_causes = air_exclude
)


write.csv(
  air_apm_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_apm_sens_1000.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# HAP
# Household air pollution from solid fuels
# ------------------------------------------------------------

air_hap_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_air,
  rei_target = "Household air pollution from solid fuels",
  exclude_causes = air_exclude
)


write.csv(
  air_hap_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_hap_sens_1000.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# AOP
# Ambient ozone pollution
# ------------------------------------------------------------

air_aop_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_air,
  rei_target = "Ambient ozone pollution",
  exclude_causes = air_exclude
)


write.csv(
  air_aop_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_aop_sens_1000.csv",
  row.names = FALSE
)


# ============================================================
# 4. WATER
# ============================================================

water_exclude <- c(
  "Lower respiratory infections"
)


# ------------------------------------------------------------
# UWS
# Unsafe water source
# ------------------------------------------------------------

water_uws_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_wat,
  rei_target = "Unsafe water source",
  exclude_causes = water_exclude,
  rename_diarrheal = TRUE
)


write.csv(
  water_uws_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_uws_sens_1000.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# US
# Unsafe sanitation
# ------------------------------------------------------------

water_us_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_wat,
  rei_target = "Unsafe sanitation",
  exclude_causes = water_exclude,
  rename_diarrheal = TRUE
)


write.csv(
  water_us_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_us_sens_1000.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# NAHF
# No access to handwashing facility
# ------------------------------------------------------------

water_nahf_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_wat,
  rei_target = "No access to handwashing facility",
  exclude_causes = water_exclude,
  rename_diarrheal = TRUE
)


write.csv(
  water_nahf_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_nahf_sens_1000.csv",
  row.names = FALSE
)


# ============================================================
# 5. TEMPERATURE
# ============================================================

temperature_exclude <- c(
  "Exposure to mechanical forces",
  "Other unintentional injuries"
)


# ------------------------------------------------------------
# High temperature
# ------------------------------------------------------------

tem_high_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_tem,
  rei_target = "High temperature",
  exclude_causes = temperature_exclude
)


write.csv(
  tem_high_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_high_sens_1000.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# Low temperature
# ------------------------------------------------------------

tem_low_sens_1000 <- calc_subrisk_sens(
  imp_country_new = imp_country_sub,
  paf_data = paf_tem,
  rei_target = "Low temperature",
  exclude_causes = temperature_exclude
)


write.csv(
  tem_low_sens_1000,
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_low_sens_1000.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# ------------------------------------------------------------

cat("\n========= Sub-risk sensitivity 1000 check =========\n")

cat("Air APM:", nrow(air_apm_sens_1000), "countries\n")
cat("Air HAP:", nrow(air_hap_sens_1000), "countries\n")
cat("Air AOP:", nrow(air_aop_sens_1000), "countries\n")

cat("Water UWS:", nrow(water_uws_sens_1000), "countries\n")
cat("Water US:", nrow(water_us_sens_1000), "countries\n")
cat("Water NAHF:", nrow(water_nahf_sens_1000), "countries\n")

cat("Temperature high:", nrow(tem_high_sens_1000), "countries\n")
cat("Temperature low:", nrow(tem_low_sens_1000), "countries\n")
####Total by country
air_apm <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_apm.csv')
air_hap <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_hap.csv')###in million$
air_aop <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_aop.csv')###in million$

sens_apm_air <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_apm_sens_1000.csv")
sens_hap_air <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_hap_sens_1000.csv")
sens_aop_air <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/air_aop_sens_1000.csv")

#apm
air_apm1 <- air_apm %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)%>%
  select(-c(lower,upper))%>% arrange(.,country,cause_name)  %>%group_by(.,country) %>%
  mutate(burden_n=sum(burden)) %>%
  distinct(country, .keep_all = TRUE) %>% select(-c(burden))%>% rename(burden=burden_n)

air_tot_apm <- air_apm1  %>% merge(.,sens_apm_air,by = 'country') %>%
   merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9, 
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))
#hap
air_hap1 <- air_hap %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)%>%
  select(-c(lower,upper))%>% arrange(.,country,cause_name)  %>%group_by(.,country) %>%
  mutate(burden_n=sum(burden)) %>%
  distinct(country, .keep_all = TRUE) %>% select(-c(burden))%>% rename(burden=burden_n)

air_tot_hap <- air_hap1  %>% merge(.,sens_hap_air,by = 'country') %>%
   merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9, 
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

#aop
air_aop <- air_aop %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)

air_tot_aop <- air_aop %>% select(-c(lower,upper)) %>% merge(.,sens_aop_air,by = 'country') %>%
   merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

air_tot_combined <- rbind(air_tot_apm,air_tot_aop,air_tot_hap)
write.csv(air_tot_combined,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/air_subrisk_sens_1000.csv")


#-------------------------------------------------------------------------------------
wat_uws <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_uws.csv')
wat_us <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_us.csv')
wat_nahf <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/wat_nahf.csv')

sens_uws_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_uws_sens_1000.csv")
sens_us_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_us_sens_1000.csv")
sens_nahf_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_nahf_sens_1000.csv")


# cost2_wat <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/water_sens.csv")
#us
wat_us <- wat_us %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)

wat_tot_us <- wat_us %>% select(-c(lower,upper)) %>% merge(.,sens_us_wat,by = 'country') %>%
   merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

#uws
wat_uws <- wat_uws %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)

wat_tot_uws <- wat_uws %>% select(-c(lower,upper)) %>% merge(.,sens_uws_wat,by = 'country') %>%
  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

#nahf
wat_nahf <- wat_nahf %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)

wat_tot_nahf <- wat_nahf %>% select(-c(lower,upper)) %>% merge(.,sens_nahf_wat,by = 'country') %>%
  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9,
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

wat_tot_combined <- rbind(wat_tot_uws, wat_tot_us, wat_tot_nahf)
write.csv(wat_tot_combined,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/water_subrisk_sens_1000.csv")


#----------------------------------------------------------------------------------------------------
tem_high <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_ht.csv')
tem_low <- read.csv('/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_lt.csv')

sens_high_tem <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_high_sens_1000.csv")
sens_low_tem <- read.csv("/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/air&met/tem_low_sens_1000.csv")

#high
tem_high1 <- tem_high %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)%>%
  select(-c(lower,upper))%>% arrange(.,country,cause_name)  %>%group_by(.,country) %>%
  mutate(burden_n=sum(burden)) %>%
  distinct(country, .keep_all = TRUE) %>% select(-c(burden))%>% rename(burden=burden_n)

tem_tot_high <- tem_high1  %>% merge(.,sens_high_tem,by = 'country') %>%
  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9, 
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

#low
tem_low1 <- tem_low %>%
  left_join(country %>% select(location_id, location_name),
            by = c("country" = "location_id"))%>% select(.,c(2,7,4,5,6))%>% rename(country=location_name)%>%
  select(-c(lower,upper))%>% arrange(.,country,cause_name)  %>%group_by(.,country) %>%
  mutate(burden_n=sum(burden)) %>%
  distinct(country, .keep_all = TRUE) %>% select(-c(burden))%>% rename(burden=burden_n)

tem_tot_low <- tem_low1  %>% merge(.,sens_low_tem,by = 'country') %>%
  merge(.,inc_grp_n,by = 'country') %>% 
  merge(gdp_country,by=c('country','WBcode')) %>% merge(num_n,by='WBcode') %>%
  mutate(.,burden=round(sum(burden),2),lower=round(sum(lower),2)
         ,upper=round(sum(upper),2),gdp=sum(val.gdp),num=sum(num_m)) %>%
  distinct(.,burden,.keep_all = TRUE) %>%
  select(.,c('burden','lower','upper','gdp','num')) %>%
  mutate(.,burden=round(burden/10^3),lower=round(lower/10^3),upper=round(upper/10^3),gdp=gdp/10^3,num=num/10^9, 
         percent_val=round(burden/gdp*100,3),
         percent_lower=round(lower/gdp*100,3),
         percent_upper=round(upper/gdp*100,3),
         capital_val=round(burden/num),
         capital_lower=round(lower/num),
         capital_upper=round(upper/num),
         cost=paste(burden,' ',"(", lower, "-", upper, ")", sep=""),
         percent=paste(percent_val,' ',"(", percent_lower, "-", percent_upper, ")", sep=""),
         capital=paste(capital_val,' ',"(", capital_lower, "-", capital_upper, ")", sep="")) %>%
  select(c('cost','percent','capital'))

tem_tot_combined <- rbind(tem_tot_low , tem_tot_high)
write.csv(tem_tot_combined,"/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/sen_table_outcome/tem_subrisk_sens_1000.csv")