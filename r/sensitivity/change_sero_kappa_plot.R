library(data.table)
library(ggplot2)
library(glue)

new <- fread("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-19_no_intervention_r2_s0.25_k0.01_mod_data2.txt")[, .(date, incidence)][, scenario := "No Int - Mar 19 - New"]

old <- fread("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-19_no_intervention_r2_data2.txt")[, .(date, incidence)][, scenario := "No Int - Mar 19 - Old"]

obs <- fread("data_for_lockdown_extended.csv")[place == "India"][, .(date, incidence = daily_cases)][, scenario := "Observed"][between(date, new[, min(date)], new[, max(date)])][]

plot_dat <- rbindlist(list(
  obs,
  old,
  new
))

p1 <- plot_dat |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 500000))

ggsave(filename = "~/Downloads/param_mod_1.pdf", plot = p1, width = 7, height = 5, device = cairo_pdf)

p2 <- plot_dat |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 500000)) +
  xlim(as.Date("2021-01-01"), as.Date("2021-04-15"))

ggsave(filename = "~/Downloads/param_mod_2.pdf", plot = p2, width = 7, height = 5, device = cairo_pdf)
