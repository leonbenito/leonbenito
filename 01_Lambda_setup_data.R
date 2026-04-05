# 01_Lambda_setup_data.R
# source this file within Lambda.qmd to setup data

# INPUTS -------
latest <- "February 2026 MPR"
plot.date.min <- as.Date('2023-01-01')

# Get Data: sheets as list of df's----
thisfile <- glue('Projections Databank - {latest}.xlsx')
filepath <- here('data', thisfile)

# Get sheets and filter out some
exclude_words <- c(
  "Cover",
  "Notes",
  "Contents",
  "FAME",
  "Projection",
  "assumption"
)
sheets <- excel_sheets(filepath) %>% # magrittr
  .[
    !str_detect(
      .,
      regex(paste(exclude_words, collapse = "|"), ignore_case = TRUE)
    )
  ]
cat(glue("Reading {length(sheets)} sheets \n"))

# skip top empty cells and Tidy
dat_raw <- sheets |>
  set_names() |>
  map(
    ~ {
      skip_rows <- if (.x == "38. Bank Rate") 11 else 3
      read_excel(filepath, sheet = .x, skip = skip_rows)
    }
  )

# Clean Quarterly Forecast Data----
tidy_fcast_data_Q <- function(df) {
  df_long <- df |>
    pivot_longer(
      cols = -1,
      names_to = "quarter",
      values_to = "value"
    ) |>
    rename(mpr = 1) |>
    mutate(
      mpr_clean = str_remove(mpr, "\\s*\\(.*\\)") |>
        str_remove("\\s*MPR.*$") |>
        str_trim(),
      # Extract year and quarter number
      mpr_date = as.Date(parse_date_time(mpr_clean,
                                         orders = c("my", "mY", "yq", "Yq"),
                                         quiet = TRUE)),
      date = case_when(
        grepl("Q1", quarter) ~ yq(quarter),
        grepl("Q2", quarter) ~ yq(quarter),
        grepl("Q3", quarter) ~ yq(quarter),
        grepl("Q4", quarter) ~ yq(quarter),
        TRUE ~ NA_Date_
      ),
      
      value = as.numeric(value)
    ) |>
    dplyr::select(mpr, date, value) |>
    filter(!is.na(date), !is.na(value))
  
  return(df_long)
}

tidy_fcast_data_A <- function(df) {
  # check validity
  if (!is.data.frame(df) || ncol(df) < 2 || nrow(df) == 0) {
    return(NULL)
  }
  
  df_long <- df |>
    # avoid naming issues
    pivot_longer(
      cols = -1,
      names_to = "forecast_year",
      values_to = "value",
      names_transform = list(forecast_year = as.character) # Keep as chr
    ) |>
    rename(publication_date = 1) |>
    mutate(
      publication_date = as.Date(publication_date),
      forecast_year = as.numeric(forecast_year),
      value = as.numeric(value)
    ) |>
    filter(!is.na(publication_date), !is.na(forecast_year), !is.na(value)) |>
    arrange(publication_date, forecast_year)
  
  return(df_long)
}

# Get Quarterly or Annual Forecast Sheet Names----
matching_names_Q <- names(dat_raw)[grepl("^(1|2|3|4|38)\\.", names(dat_raw))]
matching_names_A <- names(dat_raw)[grepl(
  "^([5-9]|[12][0-9]|3[0-3])\\.",
  names(dat_raw)
)]

# match names
datQ <- dat_raw[matching_names_Q]
datA <- dat_raw[matching_names_A]

# Quarterly and A nnual list of df's
datQ <- map(datQ, tidy_fcast_data_Q)
datA <- map(datA, tidy_fcast_data_A)

# PLOTS----
# Plot 4 main macro variables from MPR/IRs
plots <- datQ |>
  imap(
    ~ {
      plot_data <- .x |>
        mutate(
          date = as_date(date),
          mpr_numeric = as.numeric(mpr)
        )
          
      # Clean title
      clean_title <- str_remove(.y, "^\\d+\\.\\s*")
      
      # Determine y-axis label based on variable name
      y_label <- case_when(
        str_detect(clean_title, regex("CPI|inflation|GDP growth", ignore_case = TRUE)) ~ "%yoy",
        str_detect(clean_title, regex("Unemployment", ignore_case = TRUE)) ~ "%",
        str_detect(clean_title, regex("GDP level", ignore_case = TRUE)) ~ "index",
        TRUE ~ "Value"  # Default fallback
      )
      
      ggplot(plot_data, aes(x = date, y = value, color = mpr_numeric, group = mpr)) +
        geom_line(linewidth = 1, alpha = 0.7) +
        scale_color_gradient2(
          low = "#0073C2FF",     # JCO blue
          mid = "#868686FF",     # JCO gray
          high = "#CD534CFF",    # JCO red
          midpoint = median(plot_data$mpr_numeric),
          name = NULL
        ) +
        scale_x_date(
          expand = expansion(mult = 0.02),
          date_breaks = "5 years",
          date_labels = "%Y"
        ) +
        labs(
          title = clean_title,
          subtitle = "Successive BoE Policy Report forecasts",
          x = "Date",
          y = y_label
        ) +
        theme(base_family = "roboto") +
        theme(
          text = element_text(family = "roboto"),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 16, family = "roboto"),
          plot.subtitle = element_text(size = 12, family = "roboto"),
          axis.text = element_text(size = 12, family = "roboto"),
          axis.title = element_text(size = 13, face = "bold", family = "roboto"),
          panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8) 
        )
    }
  )
plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]

# Zoom Past n MPRs ------
zoom_datQ <- function(df, n = 12) {
  # n most recent publication dates
  recent_dates <- df |>
    distinct(mpr) |>
    arrange(desc(mpr)) |>
    slice(1:n) |>
    pull(mpr)
  # Filter
  df |>
    filter(mpr %in% recent_dates) |>
    arrange(mpr, date)
}

# Apply to dataframes for past n MPRs----
datQ_zoom <- map(datQ, zoom_datQ, n = 12)

# Make Plots----
plotsQ_zoom <- datQ_zoom |>
  imap(
    ~ {
      df_filtered <- .x |>
        filter(date >= plot.date.min) |>
        mutate(
          date = as_date(date),
          mpr = as.factor(mpr)
        )
      
      # Clean title - remove leading number, dot, whitespace
      clean_title <- str_remove(.y, "^\\d+\\.\\s*")
      
      # Determine y-axis label based on variable name
      y_label <- case_when(
        str_detect(clean_title, regex("CPI|inflation|GDP growth", ignore_case = TRUE)) ~ "%yoy",
        str_detect(clean_title, regex("Unemployment", ignore_case = TRUE)) ~ "%",
        str_detect(clean_title, regex("GDP level", ignore_case = TRUE)) ~ "index",
        TRUE ~ "Value"
      )
      
      # Create plot
      ggplot(df_filtered, aes(x = date, y = value, color = mpr, group = mpr)) +
        geom_line(linewidth = 1.2, alpha = 0.8) +
        geom_point(size = 2.5, alpha = 0.8) +
        scale_color_jco(name = NULL) +
        scale_x_date(
          expand = expansion(mult = 0.02),
          date_breaks = "2 years",
          date_labels = "%Y"
        ) +
        labs(
          title = clean_title,
          subtitle = "Most recent 12 BoE Policy Report forecasts",
          x = "Date",
          y = y_label
        ) +
        theme(base_family = "roboto") +
        theme(
          text = element_text(family = "roboto"),
          legend.position = "none",
          plot.title = element_text(face = "bold", size = 16, family = "roboto"),
          plot.subtitle = element_text(size = 12, family = "roboto"),
          axis.text = element_text(size = 12, family = "roboto"),
          axis.title = element_text(size = 13, face = "bold", family = "roboto"),
          panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8)
        )
    }
  )
#plotsQ_zoom[[1]]

# ---------- Collect Lambda relevant data --------------
# Output gap from datA, annual df
names <- names(datA)
names_set <- names[str_detect(names, "Output gap|inflation")]

# Data: Lambda
lambda_df <- datA %>%

  # 1. Select only dfs specified in 'names_set
  .[names_set] %>%
  # Combine selected dfs in long df
  bind_rows(.id = "variable") %>%

  pivot_wider(
    id_cols = c(forecast_year, publication_date),
    names_from = variable,
    values_from = value
  ) %>%
  rename_with(
    # function to apply that removes first 4 chr
    .fn = ~ str_sub(., 5),

    .cols = contains("output") | contains("inflation")
  ) %>%
  arrange(publication_date, forecast_year) 

# simple Output gap plot
p_og <- ggplot(subset(lambda_df, forecast_year>=2017), aes(x=forecast_year, y = `Output gap`, color=publication_date, group = publication_date)) + 
  geom_line() + 
  geom_hline(yintercept = 0.0, lty=4) + 
  labs(
    title = "UK Output Gap",
    subtitle = "Sucessive BoE forecasts",
    x = "Date",
    y = "% potential output"
  ) +
  theme(base_family = "roboto") +
  theme(
    text = element_text(family = "roboto"),
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, family = "roboto"),
    plot.subtitle = element_text(size = 12, family = "roboto"),
    axis.text = element_text(size = 12, family = "roboto"),
    axis.title = element_text(size = 13, face = "bold", family = "roboto"),
    panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8)
  )

# Output gap and Inflation, by Sub-period
#----------------------------------------
datA_og <- datA[[str_which(names(datA), "Output gap")]] |> 
  filter(publication_date ==max(publication_date)) |> 
  rename(og = value)
  
datQ_cpi <- datQ[[1]] |> 
  filter(mpr==max(mpr)) |> 
  filter(month(date)==7) |>  # Q3 for merging with annual average
  rename(cpi = value) |> 
  mutate(forecast_year = year(date))
  
lambda <- datA_og |> 
  left_join(datQ_cpi)

# Annual Output gaps
lambda_df <- lambda |>
  mutate(
    sub_period = case_when(
      # condition 1: dates before 2008
      forecast_year < 2008 ~ "Pre-GFC (Pre-2008)",
      # condition 2: 2008 - 20
      forecast_year >= 2008 & forecast_year < 2020 ~ "Post-GFC and ELB (2008-19)",
      TRUE ~ "Covid-19 and post-Covid (2020+)"
    ),
    # convert to a factor
    sub_period = factor(
      sub_period,
      levels = c(
        "Pre-GFC (Pre-2008)",
        "Post-GFC and ELB (2008-19)",
        "Covid-19 and post-Covid (2020+)"
      )
    )
  )
sum(is.na(lambda_df$sub_period))


# [1]: Output gap and CPI
p1 <- ggplot(
  lambda_df,
  aes(x = og, y = cpi)
) +
  geom_point(size = 2.5, alpha = 0.7) +  # Removed duplicate
  geom_smooth(
    method = MASS::rlm,
    se = TRUE,
    color = "blue",
    linetype = "dashed",
    linewidth = 1,
    alpha = 0.3
  ) +
  geom_text_repel(
    aes(label = forecast_year),
    size = 3.5,
    box.padding = 0.3,
    max.overlaps = 15,
    family = "roboto"
  ) +
  geom_hline(yintercept = 2.0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  facet_wrap(~sub_period) +
  labs(
    title = "UK: CPI Inflation and Output Gaps",
    x = "Output Gap (%)",
    y = "CPI Inflation (%yoy)"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    text = element_text(family = "roboto"),
    plot.title = element_text(face = "bold", size = 16, family = "roboto", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12, family = "roboto"),
    axis.title = element_text(size = 13, face = "bold", family = "roboto"),
    axis.text = element_text(size = 11, family = "roboto"),
    panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8),
    panel.spacing = unit(1, "lines")
  )
p1

# -------- Add Code for Lamda.qmd or 01_lambda.R -------------

# Collect Lambda-relevant data
namesA <- names(datA)
namesQ <- names(datQ)
names_setA <- namesA[str_detect(namesA, "Output gap")]
names_setQ <- namesQ[str_detect(namesQ, "inflation")]

#latest MPR 
latest_mpr <- max(datA[[names_setA[1]]]$publication_date)
latest_year <- year(latest_mpr)

output_gap_name <- names(datA)[str_detect(names(datA), regex("Output gap", ignore_case = TRUE))]
cpi_name <- names(datQ)[str_detect(names(datQ), regex("CPI inflation", ignore_case = TRUE))]

# Lambda analysis: INFL and Output gaps, Historic
lambda_dfA <- datA %>%
  
  .[names_setA] %>%
  
  bind_rows(.id = "variable") %>%
  
  pivot_wider(
    id_cols = c(forecast_year, publication_date),
    names_from = variable, 
    values_from = value
  ) %>%
  rename_with(
    .fn = ~str_sub(., 5),
    
    .cols = contains("output")
  ) %>%
  rename(year = forecast_year) %>%
  
  dplyr::filter(publication_date==latest_mpr & year <= year(publication_date)) 


# Quarterly CPI data
lambda_dfQ <- datQ %>%
  .[names_setQ] %>%
  bind_rows(.id = "variable") %>%
  
  mutate(
    year = year(date),
    sub_period = case_when(
      year(date) < 2008 ~ "Pre-GFC (Pre-2008)",
      year(date) >=2008 & year(date) < 2020 ~ "Post-GFC and ELB (2008-2019)",
      TRUE ~ "Covid-19 and Post-Covid (2020+)"
    ),
    sub_period = factor(sub_period, levels = c(
      "Pre-GFC (Pre-2008)",
      "Post-GFC and ELB (2008-2019)",
      "Covid-19 and Post-Covid (2020+)"
    ))
  ) %>%
  rename_with(
    .fn = ~ str_sub(., 5),
    .cols = contains("inflation")
  ) %>%
  rename(cpi_inflation = value,
         publication_date = mpr) %>%
  
  dplyr::filter(publication_date==latest_mpr & year <= year(publication_date)) %>%
  arrange(publication_date, year)

lambda_df <- lambda_dfQ |> 
  left_join(lambda_dfA) |> dplyr::select(-variable)

#----------------------------
# Annual OG and CPI plots
#----------------------------
p2 <- ggplot(
  lambda_df,
  aes(x = `Output gap`, y = cpi_inflation)  # Fixed: added =, proper backticks
) + 
  geom_point(size = 2.5, alpha = 0.7) +  # Removed duplicate
  geom_smooth(
    method = MASS::rlm,
    se = TRUE,
    color = "#0073C2FF",
    linetype = "dashed",
    linewidth = 1,
    alpha = 0.3
  ) + 
  geom_text_repel(
    aes(label = year),
    size = 3.5,
    box.padding = 0.3,
    max.overlaps = 15,
    family = "roboto"
  ) + 
  geom_hline(yintercept = 2.0, linetype = "dashed", color = "#E64B35FF", linewidth = 1) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) + 
  facet_wrap(~sub_period) + 
  labs(
    title = "UK: CPI Inflation and Output Gap",
    x = "Output Gap (%)",
    y = "CPI Inflation (%yoy)"
  ) + 
  theme_minimal(base_family = "roboto") +
  theme(
    text = element_text(family = "roboto"),
    plot.title = element_text(face = "bold", size = 16, family = "roboto", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12, family = "roboto"),
    axis.title = element_text(size = 13, face = "bold", family = "roboto"),
    axis.text = element_text(size = 11, family = "roboto"),
    panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8)
  )

p2


#---------------------------------------
# Lambda Analysis for 2y-ahead Forecasts
#---------------------------------------
output_gap_2y <- datA[[output_gap_name[1]]] %>%
  mutate(
    pub_year = year(publication_date),
    is_2y_ahead = (forecast_year == pub_year + 2)
  ) %>%
  rename(output_gap = value) %>%
  filter(is_2y_ahead) %>%
  dplyr::select(publication_date, output_gap)

cpi_inflation_2y <- datQ[[cpi_name[1]]] %>%
  rename(publication_date =mpr) %>%
  mutate(
    months_ahead = interval(publication_date, date) %/% months(1)
  ) %>%
  filter(months_ahead >=23 & months_ahead <= 25) %>%
  group_by(publication_date) %>%
  slice_min(abs(months_ahead - 24), n=1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(cpi_inflation = value) %>%
  dplyr::select(publication_date, cpi_inflation)

lambda_df_2y <- inner_join(output_gap_2y, cpi_inflation_2y, by = "publication_date")

model <- lm(
  cpi_inflation ~ output_gap,
  data = lambda_df_2y
)

lambda_2y_with_preds <- lambda_df_2y |> 
  mutate(predicted_cpi = predict(model))

latest_year <- max(year(lambda_2y_with_preds$publication_date), na.rm = TRUE)
final_year_data <- lambda_2y_with_preds |> 
  filter(year(publication_date)==latest_year)
final_observation <- lambda_df_2y |> 
  arrange(publication_date) |> 
  slice_tail(n=1)

# LAMBDA PLOT
lambda <- ggplot(lambda_df_2y, aes(x=output_gap, y = cpi_inflation)) + 
  geom_point() + 
  geom_point(
    data = final_observation,
    aes(x=output_gap, y = cpi_inflation),
    shape = 22,
    size = 8,
    fill = "red",
    color = "black",
    stroke = 1.2
  ) + 
  geom_text_repel(
    aes(label = format(publication_date, "%b-%Y")),
    size = 5,
    box.padding = 0.3,
    max.overlaps = 15
  ) + 
  geom_abline(
    intercept = 2.0,
    slope = -0.1,
    color = "purple",
    linetype = "dotdash",
    linewidth = 1
  ) + 
  geom_abline(
    intercept = 2.0,
    slope = -1,
    color = "darkgreen",
    linetype = "dotdash",
    linewidth = 1
  ) + 
  annotate(
    "text",
    x = -1.5,
    y = 2.0 + (-0.1 * -1.5),
    label = "lambda = 0.1",
    color = "purple",
    fontface = "bold",
    size = 5,
    vjust = -0.5
  ) + 
  annotate(
    "text",
    x = -0.5,
    y = 2.0 + (-1 * -0.5),
    label = "lambda = 1.0",
    color = "darkgreen",
    fontface = "bold",
    size = 5,
    hjust = 1.2
  ) + 
  geom_hline(yintercept = 2.0, lty=4, color = "red") + 
  geom_vline(xintercept = 0.0, lty=4, color = "red") + 
  labs(
    title = "CPI Inflation and Output Gap",
    subtitle = "BoE 2-year ahead forecasts",
    x = "Output Gap (%)", y = "CPI Inflation, %yoy",
    caption = "Sources: BoE, Own calculations"
  ) +
  theme(base_family = "roboto") +
  theme(
    text = element_text(family = "roboto"),
    plot.title = element_text(face = "bold", size = 16, family = "roboto", hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "roboto", hjust = 0.5),
    plot.caption = element_text(size = 9, family = "roboto", color = "gray50", hjust = 1),
    axis.title = element_text(size = 13, face = "bold", family = "roboto"),
    axis.text = element_text(size = 11, family = "roboto"),
    panel.border = element_rect(color = "gray40", fill = NA, linewidth = 0.8) 
  )
lambda






  




