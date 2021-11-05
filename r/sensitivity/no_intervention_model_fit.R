library(data.table)
library(glue)
library(ggplot2)

fldr <- "test"

dat <- fread("data_for_lockdown_extended.csv")[place == "India"]

last_obs   <- as.Date("2021-03-18")
start_obs  <- last_obs - 99
start_proj <- last_obs + 1
last_proj  <- last_obs + 150

d <- dat[between(date, start_obs, last_proj)]


no_int <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/no_intervention/2021-03-19_no_intervention_r2_data2.txt"), showProgress = FALSE)[, scenario := "No int - March 19 - Prod"]

no_int_0301 <- fread(glue("/Volumes/tiny/projects/covid/science_revision/{fldr}/no_intervention/2021-03-19_no_intervention_r2_data2.txt"), showProgress = FALSE)[, scenario := "No int - March 19 - Test"]

plot_dat <- rbindlist(list(
  d[, .(date, incidence = daily_cases)][, scenario := "Observed"],
  no_int[, .(date, incidence, scenario)],
  no_int_0301[, .(date, incidence, scenario)]
))[, scenario := factor(scenario, levels = c("Observed", "No int - March 19 - Test", "No int - March 19 - Prod"))]

plot_dat |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  # geom_vline(xintercept = start_proj, linetype = 2, color = "gray40") +
  geom_line(size = 1) +
  xlim(as.Date("2021-01-01"), as.Date("2021-04-15")) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 250000))

ggsave(filename = "~/Downloads/no_int_mar19_fit_testprod_20210815.pdf",
       width = 7, height = 5, device = cairo_pdf)
