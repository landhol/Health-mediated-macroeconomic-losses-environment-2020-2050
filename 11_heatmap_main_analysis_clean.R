# ============================================================
# Step 11. Heatmaps for the main analysis
# ============================================================
#
# Purpose:
#   Prepare and plot disease-specific heatmaps for the main-analysis
#   environmental risks with multiple disease contributors:
#     1. Air pollution
#     2. Non-optimal temperature
#
#   Heatmaps compare disease-specific macroeconomic burden and burden
#   as a share of cumulative GDP across:
#     - global;
#     - World Bank income groups; and
#     - geographic regions.
#
#   UWSH is not plotted in this script because the corresponding
#   disease-specific analysis contains only one principal disease
#   contributor and was not displayed as a heatmap in the original
#   figure workflow.
#
# Data sources:
#
#   1. Risk-attributable macroeconomic burden
#      Derived in Step 8 using GBD 2021 population-attributable fractions.
#
#   2. Gross domestic product
#      Source: Institute for Health Metrics and Evaluation (IHME)
#      Identifier: DOI: 10.6069/HHKW-4F29
#      Download date: 2026-02-06
#
#   3. Income-group and regional classification
#      Input file: CLASS.xlsx
#      Source/version: [TO FILL: exact source and classification version]
#      Download date: 2026-02-06
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       readxl:  1.4.3
#       dplyr:   1.1.4
#       tidyr:   1.3.0
#       ggplot2: 3.4.4
#
# Notes:
#   - Monetary burden inputs are in millions.
#   - GDP is summed over 2020-2050 and converted to millions.
#   - The variable "percent" represents burden as a percentage of
#     cumulative GDP and is therefore calculated as burden / GDP * 100.
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

input_dir <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/",
  "outcome/rural outcome/air&met/"
)

figure_dir <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/",
  "outcome/figure/air&met/heat map/"
)


# ------------------------------------------------------------
# 3. Country mapping, cumulative GDP, and classifications
# ------------------------------------------------------------

country <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv"
)

country_n <- country %>%
  select(c(3, 4)) %>%
  rename(
    WBcode = Country.Code,
    country = location_id
  )

gdp <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/outcome/rural outcome/all/gdp_161950_n.csv"
)

# Cumulative GDP over 2020-2050, expressed in millions.
gdp_country <- gdp %>%
  select(c(2:4)) %>%
  subset(year > 2019) %>%
  merge(
    country_n,
    by = "WBcode",
    all.y = TRUE
  ) %>%
  group_by(WBcode) %>%
  summarise(
    val.gdp = sum(val.gdp) / 1e6,
    country = first(country),
    .groups = "drop"
  )

income_group <- read_xlsx(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/income group/CLASS.xlsx",
  sheet = 1
)

inc_grp_n <- income_group %>%
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
# 4. Read disease-specific burden inputs
# ------------------------------------------------------------

air_allcause <- read.csv(
  file.path(
    input_dir,
    "air_allcause.csv"
  )
)

air_apm <- read.csv(
  file.path(
    input_dir,
    "air_apm.csv"
  )
)

air_hap <- read.csv(
  file.path(
    input_dir,
    "air_hap.csv"
  )
)

temperature_allcause <- read.csv(
  file.path(
    input_dir,
    "tem_allcause.csv"
  )
)

temperature_high <- read.csv(
  file.path(
    input_dir,
    "tem_ht.csv"
  )
)

temperature_low <- read.csv(
  file.path(
    input_dir,
    "tem_lt.csv"
  )
)


# ------------------------------------------------------------
# 5. Helper functions
# ------------------------------------------------------------

drop_index_column <- function(data) {

  if ("X" %in% names(data)) {
    data <- data %>%
      select(-X)
  }

  return(data)
}


# Build the no-country heatmap dataset:
# global + income groups + regions, by disease.
prepare_heatmap_data <- function(data) {

  data <- drop_index_column(data)

  merged_data <- data %>%
    merge(
      inc_grp_n,
      by = "country"
    ) %>%
    merge(
      gdp_country,
      by = c("country", "WBcode")
    )

  # Regional summaries.
  data_region <- merged_data %>%
    group_by(
      cause_name,
      Region
    ) %>%
    summarise(
      burden = round(
        sum(burden),
        2
      ),
      gdp = sum(val.gdp),
      .groups = "drop"
    ) %>%
    mutate(
      percent = burden / gdp * 100
    ) %>%
    transmute(
      cause_name,
      location = Region,
      percent,
      burden
    )

  # Income-group summaries.
  data_income <- merged_data %>%
    group_by(
      cause_name,
      income
    ) %>%
    summarise(
      burden = round(
        sum(burden),
        2
      ),
      gdp = sum(val.gdp),
      .groups = "drop"
    ) %>%
    mutate(
      percent = burden / gdp * 100,
      location = factor(
        income,
        levels = c(
          "High income",
          "Upper middle income",
          "Lower middle income",
          "Low income"
        )
      )
    ) %>%
    arrange(location) %>%
    transmute(
      cause_name,
      location = as.character(location),
      percent,
      burden
    )

  # Global summaries.
  data_global <- merged_data %>%
    group_by(cause_name) %>%
    summarise(
      burden = round(
        sum(burden),
        2
      ),
      gdp = sum(val.gdp),
      .groups = "drop"
    ) %>%
    mutate(
      percent = burden / gdp * 100,
      location = "Global"
    ) %>%
    select(
      cause_name,
      location,
      percent,
      burden
    )

  bind_rows(
    data_global,
    data_income,
    data_region
  ) %>%
    filter(
      location != "Others"
    )
}


format_percent_label <- function(x) {

  case_when(
    is.na(x) ~ "",
    x < 0.001 ~ "<0.001%",
    x < 0.01 ~ paste0(
      formatC(
        x,
        format = "f",
        digits = 3
      ),
      "%"
    ),
    TRUE ~ paste0(
      formatC(
        x,
        format = "f",
        digits = 2
      ),
      "%"
    )
  )
}


abbreviate_cause <- function(cause_name) {

  case_when(
    cause_name == "Age-related and other hearing loss" ~ "ARHL",
    cause_name == "Asthma" ~ "Asthma",
    cause_name == "Cardiomyopathy and myocarditis" ~ "CM",
    cause_name == "Chronic kidney disease" ~ "CKD",
    cause_name == "Chronic obstructive pulmonary disease" ~ "COPD",
    cause_name == "Diabetes mellitus type 1" ~ "T1DM",
    cause_name == "Diabetes mellitus type 2" ~ "T2DM",
    cause_name == "Enteric infections" ~ "EI",
    cause_name == "Exposure to mechanical forces" ~ "EMF",
    cause_name == "Falls" ~ "Falls",
    cause_name == "Hemolytic disease and other neonatal jaundice" ~ "HDNJ",
    cause_name == "Hypertensive heart disease" ~ "HHD",
    cause_name == "Idiopathic developmental intellectual disability" ~ "IDID",
    cause_name == "Interpersonal violence" ~ "IPV",
    cause_name == "Ischemic heart disease" ~ "IHD",
    cause_name == "Larynx cancer" ~ "LCa",
    cause_name == "Leukemia" ~ "Leukemia",
    cause_name == "Low back pain" ~ "LBP",
    cause_name == "Lower respiratory infections" ~ "LRI",
    cause_name == "Mesothelioma" ~ "Mesothelioma",
    cause_name == "Nasopharynx cancer" ~ "NPCa",
    cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma" ~ "NEBAT",
    cause_name == "Neonatal preterm birth" ~ "NPE",
    cause_name == "Neonatal sepsis and other neonatal infections" ~ "NSNI",
    cause_name == "Other infectious diseases" ~ "OID",
    cause_name == "Other unintentional injuries" ~ "OUI",
    cause_name == "Ovarian cancer" ~ "OCa",
    cause_name == "Pneumoconiosis" ~ "Pneumoconiosis",
    cause_name == "Self-harm" ~ "Self-harm",
    cause_name == "Stroke" ~ "Stroke",
    cause_name == "Tracheal, bronchus, and lung cancer" ~ "TBL",
    cause_name == "Transport injuries" ~ "TI",
    TRUE ~ cause_name
  )
}


# ------------------------------------------------------------
# 6. Prepare heatmap datasets
# ------------------------------------------------------------

air_all_heat <- prepare_heatmap_data(
  air_allcause
)

air_apm_heat <- prepare_heatmap_data(
  air_apm
)

air_hap_heat <- prepare_heatmap_data(
  air_hap
)

temperature_all_heat <- prepare_heatmap_data(
  temperature_allcause
)

temperature_high_heat <- prepare_heatmap_data(
  temperature_high
)

temperature_low_heat <- prepare_heatmap_data(
  temperature_low
)


write.csv(
  air_all_heat,
  file.path(
    input_dir,
    "air_all_heat.csv"
  ),
  row.names = FALSE
)

write.csv(
  air_apm_heat,
  file.path(
    input_dir,
    "air_apm_heat.csv"
  ),
  row.names = FALSE
)

write.csv(
  air_hap_heat,
  file.path(
    input_dir,
    "air_hap_heat.csv"
  ),
  row.names = FALSE
)

write.csv(
  temperature_all_heat,
  file.path(
    input_dir,
    "tem_all_heat.csv"
  ),
  row.names = FALSE
)

write.csv(
  temperature_high_heat,
  file.path(
    input_dir,
    "tem_ht_heat.csv"
  ),
  row.names = FALSE
)

write.csv(
  temperature_low_heat,
  file.path(
    input_dir,
    "tem_lt_heat.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 7. Heatmap plotting function
# ------------------------------------------------------------

heat_plot <- function(
  data,
  risk_label
) {

  global_reference <- data %>%
    filter(
      location == "Global"
    ) %>%
    select(
      cause_name,
      global_burden = burden,
      global_percent = percent
    )

  df_plot <- data %>%
    left_join(
      global_reference,
      by = "cause_name"
    ) %>%
    mutate(
      burden_category = case_when(
        burden < 0.2 * global_burden ~ "Very low",
        burden < 0.4 * global_burden ~ "Low",
        burden <= 0.6 * global_burden ~ "Moderate",
        burden <= 0.8 * global_burden ~ "High",
        TRUE ~ "Very high"
      ),
      percent_category = case_when(
        percent < 0.5 * global_percent ~ "Lower",
        percent < 0.8 * global_percent ~ "Slightly lower",
        percent <= 1.2 * global_percent ~ "Similar to global",
        percent <= 1.5 * global_percent ~ "Slightly higher",
        TRUE ~ "Higher"
      ),
      cause_abbr = abbreviate_cause(
        cause_name
      )
    )

  desired_locations <- c(
    "Global",
    "High income",
    "Upper middle income",
    "Lower middle income",
    "Low income",
    "East Asia & Pacific",
    "Europe & Central Asia",
    "Latin America & Caribbean",
    "Middle East & North Africa",
    "North America",
    "South Asia",
    "Sub-Saharan Africa"
  )

  # Order disease columns by global absolute burden, highest to lowest.
  cause_order <- df_plot %>%
    filter(
      location == "Global"
    ) %>%
    arrange(
      desc(burden)
    ) %>%
    pull(cause_abbr)

  df_long <- df_plot %>%
    pivot_longer(
      cols = c(
        burden,
        percent
      ),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      category = ifelse(
        metric == "burden",
        as.character(burden_category),
        as.character(percent_category)
      ),
      category = factor(
        category,
        levels = c(
          "Very low",
          "Low",
          "Moderate",
          "High",
          "Very high",
          "Lower",
          "Slightly lower",
          "Similar to global",
          "Slightly higher",
          "Higher"
        )
      ),
      label = ifelse(
        metric == "percent",
        format_percent_label(value),
        format(
          round(value, 0),
          big.mark = ",",
          scientific = FALSE
        )
      ),
      metric = factor(
        metric,
        levels = c(
          "burden",
          "percent"
        )
      ),
      cause_abbr = factor(
        cause_abbr,
        levels = cause_order
      ),
      location = factor(
        location,
        levels = rev(
          desired_locations
        )
      )
    )

  heatmap_colors <- c(
    "Lower" = "#699bbb",
    "Slightly lower" = "#A3B1C0",
    "Similar to global" = "#E8E8E8",
    "Slightly higher" = "#C4A0A1",
    "Higher" = "#B77A76",
    "Very low" = "#699bbb",
    "Low" = "#A3B1C0",
    "Moderate" = "#E8E8E8",
    "High" = "#C4A0A1",
    "Very high" = "#B77A76"
  )

  ggplot(
    df_long,
    aes(
      x = metric,
      y = location,
      fill = category
    )
  ) +
    geom_tile(
      color = "white",
      linewidth = 0.4
    ) +
    geom_text(
      aes(
        label = label
      ),
      size = 3,
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = heatmap_colors,
      name = "Compared with global"
    ) +
    facet_grid(
      ~ cause_abbr,
      scales = "free_x",
      space = "free_x"
    ) +
    coord_cartesian(
      expand = FALSE
    ) +
    theme_minimal(
      base_size = 16
    ) +
    theme(
      strip.text = element_text(
        size = 16,
        face = "bold"
      ),
      axis.text.x = element_text(
        size = 14,
        face = "bold",
        color = "black"
      ),
      axis.text.y = element_text(
        size = 14,
        face = "bold",
        color = "black"
      ),
      plot.title = element_text(
        size = 20,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 14,
        hjust = 0.5,
        color = "gray30"
      ),
      legend.position = "bottom",
      legend.text = element_text(
        size = 14,
        face = "bold"
      ),
      legend.title = element_text(
        size = 14,
        face = "bold"
      ),
      panel.grid = element_blank(),
      panel.spacing.x = grid::unit(
        0.2,
        "lines"
      ),
      plot.margin = margin(
        10,
        10,
        10,
        10
      )
    ) +
    labs(
      title = paste(
        "Macroeconomic burden of",
        risk_label
      ),
      subtitle = "Absolute burden and percentage of GDP by disease",
      x = NULL,
      y = NULL
    )
}


# ------------------------------------------------------------
# 8. Generate and save heatmaps
# ------------------------------------------------------------

heatmap_specs <- list(
  list(
    data = air_all_heat,
    label = "air pollution",
    file = "air_all.pdf"
  ),
  list(
    data = air_apm_heat,
    label = "ambient particulate matter pollution",
    file = "air_apm.pdf"
  ),
  list(
    data = air_hap_heat,
    label = "household air pollution from solid fuels",
    file = "air_hap.pdf"
  ),
  list(
    data = temperature_all_heat,
    label = "non-optimal temperature",
    file = "tem_all.pdf"
  ),
  list(
    data = temperature_high_heat,
    label = "high temperature",
    file = "tem_ht.pdf"
  ),
  list(
    data = temperature_low_heat,
    label = "low temperature",
    file = "tem_lt.pdf"
  )
)

for (spec in heatmap_specs) {

  plot_object <- heat_plot(
    spec$data,
    spec$label
  )

  ggsave(
    filename = file.path(
      figure_dir,
      spec$file
    ),
    plot = plot_object,
    width = 18,
    height = 10
  )
}
