# libraries and such -----------
ally::libri(data.table, glue, ggplot2, ggsci)
source("esir_ally.R")

# specs ------------
path_to_eSIR_output  <- "/Volumes/tiny/projects/covid/science_revision/data/no_intervention/"
path_to_eSAIR_output <- ""
eSAIR_casename       <- "2021-03-19_no_int_r2" # something like this?? forgot your nomenclature
end_date <- as.Date("2021-04-15")

## these shouldn't change
last_obs   <- as.Date("2021-03-18")
start_obs  <- last_obs - 99
start_proj <- last_obs + 1
last_proj  <- last_obs + 150

# data ------------

## observed
obs <- fread("data/covid19india_national_counts_20211031.csv")[, date := as.Date(date)]
obs <- obs[between(date, start_obs - 14, last_proj + 14)][, smooth_cases := predict(loess(daily_cases ~ as.numeric(date), span = 0.3))][between(date, start_obs, last_proj)][, scenario := "Observed"]

## esir
esir <- fread(glue("{path_to_eSIR_output}2021-03-19_no_intervention_r2_data2.txt"), showProgress = FALSE)[, scenario := "eSIR"][, date := as.Date(date)][!is.na(incidence)][, smooth_cases := predict(loess(incidence ~ as.numeric(date), span = 0.3))][]

## seirfansy
seir <- as.data.table(readRDS("seirfansy_data/No_Intervention.rds")$Cases_Deaths)[, .(date = dates, daily_cases = P_daily, daily_deaths = D_daily)][, date := as.Date(date)][, scenario := "SEIR"]

## esair
if (file.exists(glue("{path_to_eSAIR_output}{eSAIR_casename}_data2.txt"))) {
  
  colors <- c("Observed" = "black", "eSIR" = pal_lancet()(3)[2], "SEIR" = pal_lancet()(3)[3], "eSAIR" = pal_lancet()(3)[1])

  esair <- fread(glue("{path_to_eSAIR_output}{eSAIR_casename}_data2.txt"))[, scenario := "eSAIR"][, date := as.Date(date)][!is.na(incidence)][, smooth_cases := predict(loess(incidence ~ as.numeric(date), span = 0.3))][]
  
  combo <- rbindlist(list(
    obs,
    esir,
    seir[, smooth_cases := daily_cases],
    esair
  ), fill = TRUE)[date <= end_date][]
  
} else {
  
  colors <- c("Observed" = "black", "eSIR" = pal_lancet()(3)[2], "SEIR" = pal_lancet()(3)[3])
  
  combo <- rbindlist(list(
    obs,
    esir,
    seir[, smooth_cases := daily_cases]
  ), fill = TRUE)[date <= end_date][]
}

(fit_plot <- combo |>
    ggplot(aes(x = date, y = smooth_cases, color = scenario)) +
    geom_vline(xintercept = as.Date("2021-03-18"), linetype = 2, color = "gray40", size = 1) +
    annotate(geom = "text", label = "Prediction start", x = as.Date("2021-03-17"), y = 200000, hjust = 1, size = 3, fontface = "bold", family = "Helvetica", color = "gray40") +
    geom_line(size = 1) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Model fit plot",
      subtitle = glue("{format_date(combo[, min(date)])} to {format_date(combo[, max(date)])}"),
      x     = "Date",
      y     = "Daily cases"
    ))



ggsave(filename = "fig/sensitivity/model_fit_plot.pdf",
       width = 6, height = 4, device = cairo_pdf)

ggsave(filename = "fig/sensitivity/model_fit_plot.png",
       width = 6, height = 4, dpi = 320, device = png)
