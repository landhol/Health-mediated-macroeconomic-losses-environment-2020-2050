# ============================================================
# Step 10. Country-level maps for the main analysis
# ============================================================
#
# Purpose:
#   Generate country-level maps for the three environmental risks
#   evaluated in the manuscript:
#     1. Air pollution
#     2. Unsafe water, sanitation, and handwashing (UWSH)
#     3. Non-optimal temperature
#
#   The script produces:
#     - absolute macroeconomic-burden maps;
#     - burden-as-a-share-of-GDP maps; and
#     - maps of the leading disease contributor where this analysis
#       was included in the original workflow.
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
#   3. World map geometry
#      Obtained with ggplot2::map_data("world").
#
# Software:
#   R version: 4.2.1
#   Platform: x86_64-pc-linux-gnu (64-bit)
#   Operating system: Ubuntu 22.04.2 LTS
#   Package versions:
#       dplyr:    1.1.4
#       ggplot2:  3.4.4
#       patchwork: 1.1.3
#       sf:        1.0-20
#
# Notes:
#   - Monetary burden inputs from Step 8 are in millions and are converted
#     to billions for absolute-burden maps.
#   - GDP is summed over 2020-2050 before calculating burden as a share
#     of cumulative GDP.
#   - Country-name harmonization is performed only for map matching.
#   - For continuous maps, Taiwan is combined with China following the
#     original mapping workflow; the combined value is displayed for both
#     polygons.
#
# ============================================================


# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)


# ------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------

input_dir <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/",
  "outcome/rural outcome/air&met/"
)

figure_dir <- paste0(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/",
  "outcome/figure/air&met/fig_country/"
)


# ------------------------------------------------------------
# 3. Country-name harmonization
# ------------------------------------------------------------

harmonize_map_names <- function(data) {

  data$location <- as.character(data$location)

  replacements <- c(
    "United States of America" = "USA",
    "Russian Federation" = "Russia",
    "United Kingdom" = "UK",
    "Congo" = "Republic of Congo",
    "Iran (Islamic Republic of)" = "Iran",
    "Democratic People's Republic of Korea" = "North Korea",
    "Taiwan (Province of China)" = "Taiwan",
    "Republic of Korea" = "South Korea",
    "United Republic of Tanzania" = "Tanzania",
    "C?te d'Ivoire" = "Ivory Coast",
    "Côte d'Ivoire" = "Ivory Coast",
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Czechia" = "Czech Republic",
    "Republic of Moldova" = "Moldova",
    "Viet Nam" = "Vietnam",
    "Lao People's Democratic Republic" = "Laos",
    "Syrian Arab Republic" = "Syria",
    "Micronesia (Federated States of)" = "Micronesia",
    "Cabo Verde" = "Cape Verde",
    "United States Virgin Islands" = "Virgin Islands",
    "Eswatini" = "Swaziland",
    "Brunei Darussalam" = "Brunei"
  )

  for (old_name in names(replacements)) {
    data$location[data$location == old_name] <- replacements[[old_name]]
  }

  # Duplicate multi-island country values to match polygons in map_data().
  split_locations <- list(
    "Trinidad and Tobago" = c("Trinidad", "Tobago"),
    "Antigua and Barbuda" = c("Antigua", "Barbuda"),
    "Saint Kitts and Nevis" = c("Saint Kitts", "Nevis"),
    "Saint Vincent and the Grenadines" = c("Saint Vincent", "Grenadines")
  )

  for (combined_name in names(split_locations)) {

    original_row <- data[
      data$location == combined_name,
      ,
      drop = FALSE
    ]

    if (nrow(original_row) > 0) {

      data <- data[
        data$location != combined_name,
        ,
        drop = FALSE
      ]

      for (new_name in split_locations[[combined_name]]) {

        new_row <- original_row
        new_row$location <- new_name

        data <- rbind(
          data,
          new_row
        )
      }
    }
  }

  return(data)
}


combine_taiwan_with_china <- function(data) {

  if (
    "Taiwan" %in% data$location &&
      "China" %in% data$location
  ) {

    china_value <- data$val[data$location == "China"][1]
    taiwan_value <- data$val[data$location == "Taiwan"][1]

    combined_value <- china_value + taiwan_value

    data$val[data$location == "China"] <- combined_value
    data$val[data$location == "Taiwan"] <- combined_value
  }

  return(data)
}


# ------------------------------------------------------------
# 4. Quantile-map preparation
# ------------------------------------------------------------

make_interval_labels <- function(breaks, digits = 0) {

  formatted <- format(
    round(breaks, digits),
    trim = TRUE,
    scientific = FALSE,
    nsmall = digits
  )

  labels <- character(length(breaks) - 1)

  labels[1] <- paste0(
    "< ",
    formatted[2]
  )

  if (length(labels) > 2) {

    for (i in 2:(length(labels) - 1)) {

      labels[i] <- paste0(
        formatted[i],
        " to < ",
        formatted[i + 1]
      )
    }
  }

  labels[length(labels)] <- paste0(
    "\u2265 ",
    formatted[length(breaks) - 1]
  )

  return(labels)
}


prepare_quantile_map <- function(
  data,
  probs,
  digits = 0,
  combine_taiwan = TRUE
) {

  map_data_input <- data
  colnames(map_data_input) <- c(
    "val",
    "location"
  )

  map_data_input <- harmonize_map_names(
    map_data_input
  )

  if (combine_taiwan) {
    map_data_input <- combine_taiwan_with_china(
      map_data_input
    )
  }

  world_data <- ggplot2::map_data("world")

  breaks <- unname(
    quantile(
      map_data_input$val[
        !is.na(map_data_input$val)
      ],
      probs = probs
    )
  )

  if (anyDuplicated(breaks)) {
    stop(
      "Quantile breaks are not unique. Review the probability sequence ",
      "for this risk before plotting."
    )
  }

  interval_labels <- make_interval_labels(
    breaks,
    digits = digits
  )

  total <- full_join(
    world_data,
    map_data_input,
    by = c("region" = "location")
  ) %>%
    filter(
      region != "Antarctica"
    ) %>%
    mutate(
      val2 = cut(
        val,
        breaks = breaks,
        labels = interval_labels,
        include.lowest = TRUE,
        right = FALSE
      )
    )

  return(total)
}


# Quantile schemes retained from the original analysis.
probs_10 <- c(
  0, 0.1, 0.2, 0.3, 0.4,
  0.5, 0.6, 0.7, 0.8, 0.9, 1
)

probs_8 <- c(
  0, 0.125, 0.25, 0.375,
  0.5, 0.625, 0.75, 0.825, 1
)

# Used for the highly right-skewed high-temperature absolute burden.
probs_high_temp_absolute <- c(
  0, 0.5, 0.7, 0.8, 0.9, 1
)

# Used for high-temperature burden as a share of GDP.
probs_high_temp_percent <- c(
  0, 0.5, 0.6, 0.7, 0.8, 0.9, 1
)


# ------------------------------------------------------------
# 5. Map plotting functions
# ------------------------------------------------------------

plot_world_map <- function(
  data,
  legend_title
) {

  ggplot(
    data = data,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = val2
    )
  ) +
    geom_polygon(
      colour = "black",
      linewidth = 0.2
    ) +
    scale_fill_brewer(
      palette = "RdYlBu",
      direction = -1
    ) +
    theme(
      aspect.ratio = 1 / 3
    ) +
    xlim(
      -200,
      200
    ) +
    ylim(
      -150,
      100
    ) +
    theme_void() +
    labs(
      x = "",
      y = ""
    ) +
    guides(
      fill = guide_legend(
        title = legend_title,
        ncol = 2,
        byrow = TRUE,
        keywidth = 0.32,
        keyheight = 0.08,
        default.unit = "inch"
      )
    ) +
    theme(
      legend.position = c(0.1, 0.4),
      legend.justification = c(0, 0),
      legend.spacing.y = grid::unit(
        0.1,
        "cm"
      ),
      legend.title = element_text(
        size = 14,
        face = "bold"
      ),
      legend.text = element_text(
        size = 14
      )
    )
}


plot_inset <- function(
  data,
  xlim_values,
  ylim_values,
  aspect_ratio
) {

  ggplot(
    data = data,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = val2
    )
  ) +
    geom_polygon(
      colour = "black",
      linewidth = 0.2
    ) +
    scale_fill_brewer(
      palette = "RdYlBu",
      direction = -1
    ) +
    coord_sf(
      xlim = xlim_values,
      ylim = ylim_values
    ) +
    theme_void() +
    theme(
      aspect.ratio = aspect_ratio,
      legend.position = "none",
      panel.border = element_rect(
        fill = NA,
        color = "black",
        linewidth = 0.5,
        linetype = "solid"
      )
    )
}


make_inset_panel_1 <- function(data) {

  caribbean <- plot_inset(
    data,
    c(-90, -60),
    c(8, 26),
    3 / 4
  )

  persian_gulf <- plot_inset(
    data,
    c(47, 55.5),
    c(21, 31),
    1
  )

  balkan <- plot_inset(
    data,
    c(14, 31),
    c(35.5, 49),
    1 / 1.25
  )

  southeast_asia <- plot_inset(
    data,
    c(94, 117.5),
    c(-9.5, 8.5),
    1 / 1.4
  )

  caribbean |
    persian_gulf |
    balkan |
    southeast_asia
}


make_inset_panel_2 <- function(data) {

  west_africa <- plot_inset(
    data,
    c(-17, -10),
    c(7, 15),
    1
  )

  eastern_mediterranean <- plot_inset(
    data,
    c(31, 38),
    c(29, 35.5),
    1
  )

  west_africa |
    eastern_mediterranean
}


make_inset_panel_3 <- function(data) {

  plot_inset(
    data,
    c(3, 28),
    c(49.5, 58.5),
    1 / 1.5
  )
}


save_map_set <- function(
  data,
  legend_title,
  file_stub
) {

  world_plot <- plot_world_map(
    data,
    legend_title
  )

  inset_1 <- make_inset_panel_1(data)
  inset_2 <- make_inset_panel_2(data)
  inset_3 <- make_inset_panel_3(data)

  ggsave(
    filename = file.path(
      figure_dir,
      paste0(file_stub, ".pdf")
    ),
    plot = world_plot,
    width = 18,
    height = 10
  )

  ggsave(
    filename = file.path(
      figure_dir,
      paste0(file_stub, "_sub1.pdf")
    ),
    plot = inset_1,
    width = 18,
    height = 10
  )

  ggsave(
    filename = file.path(
      figure_dir,
      paste0(file_stub, "_sub2.pdf")
    ),
    plot = inset_2,
    width = 18,
    height = 10
  )

  ggsave(
    filename = file.path(
      figure_dir,
      paste0(file_stub, "_sub3.pdf")
    ),
    plot = inset_3,
    width = 18,
    height = 10
  )
}


# ------------------------------------------------------------
# 6. Read main-analysis burden data
# ------------------------------------------------------------

air_rei <- read.csv(
  file.path(
    input_dir,
    "air_rei.csv"
  )
)

water_rei <- read.csv(
  file.path(
    input_dir,
    "wat_rei.csv"
  )
)

temperature_rei <- read.csv(
  file.path(
    input_dir,
    "tem_rei.csv"
  )
)

country_lookup <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic2/data/data/country name/location_id_old.csv"
)

country_map <- country_lookup %>%
  select(c(2, 4)) %>%
  rename(
    country = location_id,
    location_name = location_name
  )


# Convert Step-8 burden from millions to billions.
prepare_burden_input <- function(
  data,
  risk_name
) {

  data %>%
    filter(
      rei_name == risk_name
    ) %>%
    select(
      country,
      burden
    ) %>%
    merge(
      country_map,
      by = "country"
    ) %>%
    transmute(
      burden = burden / 1000,
      country = location_name
    )
}


air_total_burden <- prepare_burden_input(
  air_rei,
  "Air pollution"
)

air_apm_burden <- prepare_burden_input(
  air_rei,
  "Ambient particulate matter pollution"
)

air_hap_burden <- prepare_burden_input(
  air_rei,
  "Household air pollution from solid fuels"
)

air_aop_burden <- prepare_burden_input(
  air_rei,
  "Ambient ozone pollution"
)

water_total_burden <- prepare_burden_input(
  water_rei,
  "Unsafe water, sanitation, and handwashing"
)

water_source_burden <- prepare_burden_input(
  water_rei,
  "Unsafe water source"
)

water_sanitation_burden <- prepare_burden_input(
  water_rei,
  "Unsafe sanitation"
)

water_handwashing_burden <- prepare_burden_input(
  water_rei,
  "No access to handwashing facility"
)

temperature_total_burden <- prepare_burden_input(
  temperature_rei,
  "Non-optimal temperature"
)

temperature_high_burden <- prepare_burden_input(
  temperature_rei,
  "High temperature"
)

temperature_low_burden <- prepare_burden_input(
  temperature_rei,
  "Low temperature"
)


# ------------------------------------------------------------
# 7. Absolute macroeconomic-burden maps
# ------------------------------------------------------------

absolute_map_specs <- list(
  list(
    data = air_total_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to air pollution, 2020-2050",
    file = "air_tot"
  ),
  list(
    data = air_apm_burden,
    probs = probs_8,
    title = "Macroeconomic burden attributable to ambient particulate matter pollution, 2020-2050",
    file = "air_apm"
  ),
  list(
    data = air_hap_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to household air pollution from solid fuels, 2020-2050",
    file = "air_hap"
  ),
  list(
    data = air_aop_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to ambient ozone pollution, 2020-2050",
    file = "air_aop"
  ),
  list(
    data = water_total_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to unsafe water, sanitation, and handwashing, 2020-2050",
    file = "wat_tot"
  ),
  list(
    data = water_source_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to unsafe water source, 2020-2050",
    file = "wat_uws"
  ),
  list(
    data = water_sanitation_burden,
    probs = probs_8,
    title = "Macroeconomic burden attributable to unsafe sanitation, 2020-2050",
    file = "wat_us"
  ),
  list(
    data = water_handwashing_burden,
    probs = probs_8,
    title = "Macroeconomic burden attributable to no access to handwashing facility, 2020-2050",
    file = "wat_nahf"
  ),
  list(
    data = temperature_total_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to non-optimal temperature, 2020-2050",
    file = "tem_tot"
  ),
  list(
    data = temperature_high_burden,
    probs = probs_high_temp_absolute,
    title = "Macroeconomic burden attributable to high temperature, 2020-2050",
    file = "tem_ht"
  ),
  list(
    data = temperature_low_burden,
    probs = probs_10,
    title = "Macroeconomic burden attributable to low temperature, 2020-2050",
    file = "tem_lt"
  )
)

for (spec in absolute_map_specs) {

  map_data_prepared <- prepare_quantile_map(
    spec$data,
    probs = spec$probs,
    digits = 0
  )

  save_map_set(
    map_data_prepared,
    spec$title,
    spec$file
  )
}


# ------------------------------------------------------------
# 8. Burden as a share of cumulative GDP
# ------------------------------------------------------------

gdp <- read.csv(
  "/dssg/home/acct-wenze.zhong/jingxuanw/economic/outcome/rural outcome/all/gdp_161950_n.csv"
)

gdp_country_map <- country_lookup %>%
  select(c(2, 3)) %>%
  rename(
    WBcode = Country.Code,
    country = location_name
  )

# Cumulative GDP over 2020-2050, expressed in billions.
gdp_country <- gdp %>%
  select(c(2:4)) %>%
  subset(
    year > 2019
  ) %>%
  merge(
    gdp_country_map,
    by = "WBcode",
    all.y = TRUE
  ) %>%
  group_by(WBcode) %>%
  summarise(
    gdp = sum(val.gdp) / 1e9,
    country = first(country),
    .groups = "drop"
  )


calculate_gdp_share <- function(data) {

  data %>%
    merge(
      gdp_country,
      by = "country"
    ) %>%
    transmute(
      percent_gdp = burden / gdp * 100,
      country
    )
}


air_total_percent <- calculate_gdp_share(
  air_total_burden
)

air_apm_percent <- calculate_gdp_share(
  air_apm_burden
)

air_hap_percent <- calculate_gdp_share(
  air_hap_burden
)

air_aop_percent <- calculate_gdp_share(
  air_aop_burden
)

water_total_percent <- calculate_gdp_share(
  water_total_burden
)

water_source_percent <- calculate_gdp_share(
  water_source_burden
)

water_sanitation_percent <- calculate_gdp_share(
  water_sanitation_burden
)

water_handwashing_percent <- calculate_gdp_share(
  water_handwashing_burden
)

temperature_total_percent <- calculate_gdp_share(
  temperature_total_burden
)

temperature_high_percent <- calculate_gdp_share(
  temperature_high_burden
)

temperature_low_percent <- calculate_gdp_share(
  temperature_low_burden
)


percent_map_specs <- list(
  list(
    data = air_total_percent,
    probs = probs_10,
    title = "Air pollution, 2020-2050 (% of GDP)",
    file = "air_tot_percent"
  ),
  list(
    data = air_apm_percent,
    probs = probs_10,
    title = "Ambient particulate matter pollution, 2020-2050 (% of GDP)",
    file = "air_apm_percent"
  ),
  list(
    data = air_hap_percent,
    probs = probs_10,
    title = "Household air pollution from solid fuels, 2020-2050 (% of GDP)",
    file = "air_hap_percent"
  ),
  list(
    data = air_aop_percent,
    probs = probs_10,
    title = "Ambient ozone pollution, 2020-2050 (% of GDP)",
    file = "air_aop_percent"
  ),
  list(
    data = water_total_percent,
    probs = probs_10,
    title = "Unsafe water, sanitation, and handwashing, 2020-2050 (% of GDP)",
    file = "wat_tot_percent"
  ),
  list(
    data = water_source_percent,
    probs = probs_10,
    title = "Unsafe water source, 2020-2050 (% of GDP)",
    file = "wat_uws_percent"
  ),
  list(
    data = water_sanitation_percent,
    probs = probs_10,
    title = "Unsafe sanitation, 2020-2050 (% of GDP)",
    file = "wat_us_percent"
  ),
  list(
    data = water_handwashing_percent,
    probs = probs_10,
    title = "No access to handwashing facility, 2020-2050 (% of GDP)",
    file = "wat_nahf_percent"
  ),
  list(
    data = temperature_total_percent,
    probs = probs_10,
    title = "Non-optimal temperature, 2020-2050 (% of GDP)",
    file = "tem_tot_percent"
  ),
  list(
    data = temperature_high_percent,
    probs = probs_high_temp_percent,
    title = "High temperature, 2020-2050 (% of GDP)",
    file = "tem_ht_percent"
  ),
  list(
    data = temperature_low_percent,
    probs = probs_10,
    title = "Low temperature, 2020-2050 (% of GDP)",
    file = "tem_lt_percent"
  )
)

for (spec in percent_map_specs) {

  map_data_prepared <- prepare_quantile_map(
    spec$data,
    probs = spec$probs,
    digits = 2
  )

  save_map_set(
    map_data_prepared,
    spec$title,
    spec$file
  )
}


# ------------------------------------------------------------
# 9. Leading disease contributor maps
# ------------------------------------------------------------

# Fixed colors retained for the disease categories that occur in the
# air-pollution and temperature maps.
disease_colors <- c(
  "Ischemic heart disease" = "#FF4500",
  "Stroke" = "#8B0000",
  "Hypertensive heart disease" = "#FF6B8B",
  "Diabetes mellitus type 2" = "#FFCCCB",
  "Chronic kidney disease" = "#BFB1D0",
  "Transport injuries" = "#FDBB5A",
  "Falls" = "#FF8C00",
  "Interpersonal violence" = "#C04000",
  "Lower respiratory infections" = "#96D2B0",
  "Chronic obstructive pulmonary disease" = "#20B2AA",
  "Idiopathic developmental intellectual disability" = "#87CEFA"
)


get_leading_cause <- function(data) {

  map_lookup <- country_lookup %>%
    select(c(2, 4)) %>%
    rename(
      country = location_id,
      location_name = location_name
    )

  result <- data %>%
    merge(
      map_lookup,
      by = "country"
    ) %>%
    arrange(
      location_name,
      cause_name
    ) %>%
    group_by(location_name) %>%
    slice_max(
      order_by = burden,
      n = 1,
      with_ties = FALSE
    ) %>%
    ungroup() %>%
    transmute(
      location = location_name,
      cause_name
    )

  # Retain the original presentation rule for Taiwan.
  if (
    "Taiwan (Province of China)" %in% result$location &&
      "China" %in% result$location
  ) {

    china_cause <- result$cause_name[
      result$location == "China"
    ][1]

    result$cause_name[
      result$location == "Taiwan (Province of China)"
    ] <- china_cause
  }

  return(result)
}


prepare_categorical_map <- function(data) {

  map_data_input <- data %>%
    rename(
      val = cause_name
    )

  map_data_input <- harmonize_map_names(
    map_data_input
  )

  world_data <- ggplot2::map_data("world")

  full_join(
    world_data,
    map_data_input,
    by = c("region" = "location")
  ) %>%
    filter(
      region != "Antarctica"
    )
}


plot_leading_disease_map <- function(
  data,
  legend_title
) {

  used_causes <- unique(
    na.omit(
      data$val
    )
  )

  color_values <- disease_colors[
    names(disease_colors) %in% used_causes
  ]

  ggplot(
    data = data,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = val
    )
  ) +
    geom_polygon(
      colour = "black",
      linewidth = 0.2
    ) +
    scale_fill_manual(
      values = color_values,
      na.value = "grey90"
    ) +
    theme(
      aspect.ratio = 1 / 3
    ) +
    xlim(
      -200,
      200
    ) +
    ylim(
      -150,
      100
    ) +
    theme_void() +
    guides(
      fill = guide_legend(
        title = legend_title,
        ncol = 1,
        byrow = TRUE,
        keywidth = 0.32,
        keyheight = 0.08,
        default.unit = "inch"
      )
    ) +
    theme(
      legend.position = c(0.1, 0.4),
      legend.justification = c(0, 0),
      legend.spacing.y = grid::unit(
        0.1,
        "cm"
      ),
      legend.title = element_text(
        size = 14,
        face = "bold"
      ),
      legend.text = element_text(
        size = 14
      )
    )
}


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

air_aop <- read.csv(
  file.path(
    input_dir,
    "air_aop.csv"
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


leading_disease_specs <- list(
  list(
    data = air_allcause,
    title = "Disease associated with air pollution",
    file = "air_allcause_type.pdf"
  ),
  list(
    data = air_apm,
    title = "Disease associated with ambient particulate matter pollution",
    file = "air_apm_type.pdf"
  ),
  list(
    data = air_hap,
    title = "Disease associated with household air pollution from solid fuels",
    file = "air_hap_type.pdf"
  ),
  list(
    data = air_aop,
    title = "Disease associated with ambient ozone pollution",
    file = "air_aop_type.pdf"
  ),
  list(
    data = temperature_allcause,
    title = "Disease associated with non-optimal temperature",
    file = "tem_allcause_type.pdf"
  ),
  list(
    data = temperature_high,
    title = "Disease associated with high temperature",
    file = "tem_ht_type.pdf"
  ),
  list(
    data = temperature_low,
    title = "Disease associated with low temperature",
    file = "tem_lt_type.pdf"
  )
)

for (spec in leading_disease_specs) {

  leading_cause <- get_leading_cause(
    spec$data
  )

  map_data_prepared <- prepare_categorical_map(
    leading_cause
  )

  leading_cause_plot <- plot_leading_disease_map(
    map_data_prepared,
    spec$title
  )

  ggsave(
    filename = file.path(
      figure_dir,
      spec$file
    ),
    plot = leading_cause_plot,
    width = 18,
    height = 10
  )
}
