library(data.table)
library(ggplot2)
library(glue)

new <- fread("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-19_no_intervention_r2_s0.241_k1e-04_mod_data2.txt")[, .(date, incidence)][, scenario := "Seroprevalence 24.1%"]

old <- fread("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-19_no_intervention_r2_data2.txt")[, .(date, incidence)][, scenario := "Reported deaths and\nrecovered"]

obs <- fread("data_for_lockdown_extended.csv")[place == "India"][, .(date, incidence = daily_cases)][, scenario := "Observed"][between(date, new[, min(date)], new[, max(date)])][]

plot_dat <- rbindlist(list(
  obs,
  old,
  new
))

plot_dat[!is.na(incidence)][order(date), `:=` (
  smooth = predict(loess(incidence ~ as.numeric(date), span = 0.25))
), by = "scenario"][]

p1 <- plot_dat[!is.na(incidence)][order(date), `:=` (
  smooth = predict(loess(incidence ~ as.numeric(date), span = 0.25))
), by = "scenario"][] |>
  ggplot(aes(x = date, y = smooth, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 500000))

ggsave(filename = "~/Downloads/param_mod_1.pdf", plot = p1, width = 7, height = 5, device = cairo_pdf)

colors <- c(
  "Observed" = "black",
  "Reported deaths and recovered" = pal_lancet()(3)[2],
  "Seroprevalence 24.1%" = pal_lancet()(3)[3]
)

p2 <- plot_dat[!is.na(incidence)][order(date), `:=` (
  smooth = predict(loess(incidence ~ as.numeric(date), span = 0.25))
), by = "scenario"][] |>
  ggplot(aes(x = date, y = smooth, color = scenario)) +
  geom_line(size = 1) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250000)) +
  xlim(as.Date("2021-01-01"), as.Date("2021-04-15")) +
  labs(
    title = "Comparison of initial seroprevalence on prediction",
    subtitle = glue("January 1, 2021 to April 15, 2021"),
    x     = "Date",
    y     = "Daily cases",
    caption = glue("**Notes:** This is a no intervention eSIR model beginning March 19, 2021 trained on trailing 100 days of data.")
  )

ggsave(filename = "fig/sensitivity/theta_comp_0415.pdf", plot = p2, width = 7, height = 5, device = cairo_pdf)
ggsave(filename = "fig/sensitivity/theta_comp_0415.png", plot = p2, width = 7, height = 5, units = "in", dpi = 320, device = png)
