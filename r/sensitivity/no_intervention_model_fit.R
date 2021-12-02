ally::libri(data.table, glue, ggplot2, ggsci)
source("esir_ally.R")

obs <- fread("data/covid19india_national_counts_20211031.csv")[, date := as.Date(date)]

seir <- as.data.table(readRDS("seirfansy_data/No_Intervention.rds")$Cases_Deaths)[, .(date = dates, daily_cases = P_daily, daily_deaths = D_daily)][, date := as.Date(date)][, scenario := "SEIRfansy"]

last_obs   <- as.Date("2021-03-18")
start_obs  <- last_obs - 99
start_proj <- last_obs + 1
last_proj  <- last_obs + 150

d <- obs[between(date, start_obs - 14, last_proj + 14)][, smooth_cases := predict(loess(daily_cases ~ as.numeric(date), span = 0.3))][between(date, start_obs, last_proj)][, scenario := "Observed"]

end_date <- as.Date("2021-04-15")

no_int <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/no_intervention/2021-03-19_no_intervention_r2_data2.txt"), showProgress = FALSE)[, scenario := "eSIR"][, date := as.Date(date)]

no_int <- no_int[!is.na(incidence)][, smooth_cases := predict(loess(incidence ~ as.numeric(date), span = 0.3))][]

taco <- rbindlist(list(
  d[date <= no_int[, max(date)]], no_int, seir[, smooth_cases := daily_cases][date <= no_int[, max(date)]]
), fill = TRUE)

colors <- c("Observed" = "black", "eSIR" = pal_lancet()(3)[2], "SEIRfansy" = pal_lancet()(3)[3])

(fit_plot <- taco |>
    ggplot(aes(x = date, y = smooth_cases, color = scenario)) +
    geom_vline(xintercept = as.Date("2021-03-18"), linetype = 2, color = "gray40", size = 1) +
    annotate(geom = "text", label = "Prediction start", x = as.Date("2021-03-17"), y = 200000, hjust = 1, size = 3, fontface = "bold", family = "Helvetica", color = "gray40") +
    geom_line(size = 1) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Model fit plot",
      subtitle = glue("{format_date(no_int[, min(date)])} to {format_date(no_int[, max(date)])}"),
      x     = "Date",
      y     = "Daily cases"
    ))



ggsave(filename = "fig/sensitivity/no_int_mar19_fit.pdf",
       width = 6, height = 4, device = cairo_pdf)

ggsave(filename = "fig/sensitivity/no_int_mar19_fit.png",
       width = 6, height = 4, dpi = 320, device = png)
