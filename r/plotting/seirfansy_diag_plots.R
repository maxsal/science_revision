# libraries -------------
ally::libri(data.table, ggplot2, janitor, glue)

source("esir_ally.R")

# helper function -----------
test <- function(ds, x, y) {
  glue("{format(ds[phases[x]], '%b %e, %y')} - {format(ds[phases[y]] - 1, '%b %e, %y')}")
}

# specs -----------
init_date   <- as.Date("2021-03-19")
end_date    <- as.Date("2021-04-15")
phases      <- c(1, 25, 40, 55, 70, 85)
train_dates <- seq.Date(from = init_date - 100, to = init_date - 1, by = "day")

# data -----------
res <- readRDS("seirfansy_data/No_Intervention.rds")
obs <- fread("data/covid19india_national_counts_20211031.csv")

phase_date_range <- c(as.vector(test(ds = train_dates, 1:5, 2:6)), glue("{format(train_dates[phases[6]], '%b %e, %y')} - {format(init_date - 1, '%b %e, %y')}"))

### NO INTERVENTION MODEL FIT ----------

# r0 plot ----------
(r0_plot <- as.data.table(res$R0)[, `:=` (order = 1:.N, phase = factor(phase_date_range, levels = phase_date_range))] |>
  ggplot(aes(x = phase, y = mean)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.25) +
  labs(
    title = "Estimated R\u2080 across phases in SEIRfansy training period",
    x = "Phase",
    y = "R\u2080"
  ) +
  theme(
    text = element_text(family = "Helvetica Neue"),
    axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5)))

ggsave(filename = "fig/seirfansy/no_int_r0_est.pdf",
       plot     = r0_plot,
       width = 7, height = 5, device = cairo_pdf)

# fit plot ----------
fit_data <- rbindlist(list(
  obs[, .(date, daily_cases)][, `:=` (date = as.Date(date), scenario = "Observed")][between(date, train_dates[1], end_date)],
  as.data.table(res$Cases_Deaths)[, .(date = dates, daily_cases = P_daily)][, `:=` (date = as.Date(date), scenario = "Predicted")][date <= end_date]
), use.names = TRUE)


(fit_plot <- fit_data |>
    ggplot(aes(x = date, y = daily_cases, color = scenario)) +
    geom_line(size = 1) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "No intervention SEIRfansy model fit",
      subtitle = glue("{format_date(fit_data[, min(date)])} to {format_date(fit_data[, max(date)])}"),
      x     = "Date",
      y     = "Daily reported cases"
    ) +
    theme(
      text        = element_text(family = "Helvetica Neue")
    ))

ggsave(filename = "fig/seirfansy/no_int_fit_plot.pdf",
       plot     = fit_plot,
       width = 7, height = 5, device = cairo_pdf)

### R0 across scenarios -----------
r0_scen <- readRDS("seirfansy_data/Table_R0.rds")

scenarios <- c("Moderate PHI", "Strengthened PHI", "Moderate lockdown - March 19", "Moderate lockdown - March 30", "Moderate lockdown - April 15")

init_dates <- as.Date(c("2021-02-19", "2021-03-13", "2021-03-19", "2021-03-30", "2021-04-15"))
phase_mid  <- round(na.omit(frollmean(c(phases, 100), 2)))

plot_dates <- data.table(
  "t2_dates" = seq.Date(from = init_dates[1] - 100, to = init_dates[1] - 1, by = "day")[phase_mid],
  "t3_dates" = seq.Date(from = init_dates[2] - 100, to = init_dates[2] - 1, by = "day")[phase_mid],
  "t4_dates" = seq.Date(from = init_dates[3] - 100, to = init_dates[3] - 1, by = "day")[phase_mid],
  "t4_2_dates" = seq.Date(from = init_dates[4] - 100, to = init_dates[4] - 1, by = "day")[phase_mid],
  "t4_3_dates" = seq.Date(from = init_dates[5] - 100, to = init_dates[5] - 1, by = "day")[phase_mid]
)

for (i in seq_along(r0_scen)) {
  r0_scen[[i]] <- as.data.table(r0_scen[[i]])[, scenario := scenarios[i]][]
}

r0_scen_data <- rbindlist(r0_scen)[, date := melt(plot_dates)[, value]][]

names(colores4) <- c("Observed", r0_scen_data[, unique(scenario)])

(r0_scen_plot <- r0_scen_data[, lt := "solid"][scenario == "Moderate lockdown - March 30", lt := "dashed"][scenario == "Moderate lockdown - April 15", lt := "dotted"] |>
  ggplot(aes(x = date, y = mean, color = scenario)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.25) +
    scale_color_manual(values = colores4) +
  geom_line(aes(linetype = lt)) +
    scale_linetype_identity() +
  labs(
    title = "SEIRfansy predicted R(t) across scenarios",
    x     = "Date",
    y     = "R(t)",
    caption = "Points are plotted on midpoint of range used to estimate R(t)"
  ))

ggsave(filename = "fig/seirfansy/r0_scen_plot.pdf",
       plot     = r0_scen_plot,
       width = 7, height = 5, device = cairo_pdf)

ggsave(filename = "fig/seirfansy/r0_scen_plot.png",
       plot     = r0_scen_plot, units = "in", dpi = 320,
       width = 7, height = 5, device = png)

### Beta across scenarios ------------
beta_scen <- readRDS("seirfansy_data/Table_beta.rds")

# scenarios <- c("Moderate PHI", "Strengthened PHI", "Moderate lockdown - March 19", "Moderate lockdown - March 30", "Moderate lockdown - April 15")

# init_dates <- as.Date(c("2021-02-19", "2021-03-13", "2021-03-19", "2021-03-30", "2021-04-15"))
# phase_mid  <- round(na.omit(frollmean(c(phases, 100), 2)))

# plot_dates <- data.table(
#   "t2_dates" = seq.Date(from = init_dates[1] - 100, to = init_dates[1] - 1, by = "day")[phase_mid],
#   "t3_dates" = seq.Date(from = init_dates[2] - 100, to = init_dates[2] - 1, by = "day")[phase_mid],
#   "t4_dates" = seq.Date(from = init_dates[3] - 100, to = init_dates[3] - 1, by = "day")[phase_mid],
#   "t4_2_dates" = seq.Date(from = init_dates[4] - 100, to = init_dates[4] - 1, by = "day")[phase_mid],
#   "t4_3_dates" = seq.Date(from = init_dates[5] - 100, to = init_dates[5] - 1, by = "day")[phase_mid]
# )

for (i in seq_along(beta_scen)) {
  beta_scen[[i]] <- as.data.table(beta_scen[[i]])[, scenario := scenarios[i]][]
}

beta_scen_data <- rbindlist(beta_scen)[, date := melt(plot_dates)[, value]][]

colors  <- colores4[-1]
names(colors) <- beta_scen_data[, unique(scenario)]

(beta_scen_plot <- beta_scen_data[, lt := "solid"][scenario == "Moderate lockdown - March 30", lt := "dashed"][scenario == "Moderate lockdown - April 15", lt := "dotted"] |>
    ggplot(aes(x = date, y = mean, color = scenario)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.25) +
    scale_color_manual(values = colors) +
    geom_line(aes(linetype = lt)) +
    scale_linetype_identity() +
    labs(
      title = "SEIRfansy predicted \u03B2 across scenarios",
      x     = "Date",
      y     = "\u03B2",
      caption = "**Notes:** Points are plotted on midpoint of range used to estimate \u03B2.<br>Moderate lockdown beginning March 19 is solid purple, March 30 is dashed purple, and April 14 is dotted."
    ) +
    guides(color=guide_legend(nrow=2, byrow=TRUE)))

ggsave(filename = "fig/seirfansy/beta_scen_plot.pdf",
       plot     = beta_scen_plot,
       width = 7, height = 5, device = cairo_pdf)

ggsave(filename = "fig/seirfansy/beta_scen_plot.png",
       plot     = beta_scen_plot, units = "in", dpi = 320,
       width = 7, height = 5, device = png)
