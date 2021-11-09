library(data.table)
library(glue)
library(ggplot2)

f <- list.files(here("src"))
for (i in seq_along(f)) {source(here("src", f[i]))}

format_date <- function(x) {
  glue("{trimws(format(x, '%B'))} {trimws(format(x, '%e'))}, {trimws(format(x, '%Y'))}")
}

fldr <- "test"

dat <- fread("data_for_lockdown_extended.csv")[place == "India"]

last_obs   <- as.Date("2021-03-18")
start_obs  <- last_obs - 99
start_proj <- last_obs + 1
last_proj  <- last_obs + 150

d <- dat[between(date, start_obs - 14, last_proj + 14)][, smooth_cases := predict(loess(daily_cases ~ as.numeric(date), span = 0.3))][between(date, start_obs, last_proj)][, scenario := "Observed"]

end_date <- as.Date("2021-04-15")

no_int <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/no_intervention/2021-03-19_no_intervention_r2_data2.txt"), showProgress = FALSE)[, scenario := "Predicted"]

no_int <- no_int[!is.na(incidence)][, smooth_cases := predict(loess(incidence ~ as.numeric(date), span = 0.3))][]

taco <- rbindlist(list(
  d[date <= no_int[, max(date)]], no_int
), fill = TRUE)

colors <- c("Observed" = "black", "Predicted" = pal_lancet()(3)[2])

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
