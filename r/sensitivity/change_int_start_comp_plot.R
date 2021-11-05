library(data.table)
library(ggplot2)
library(glue)

path <- "/Volumes/tiny/projects/covid/science_revision/test/early_intervention/"

scen <- "0.95"
r_0  <- 5

obs <- covid19india::get_nat_counts()[, .(date, incidence = daily_cases)][, scenario := "Observed"][between(date, as.Date("2021-03-01"), as.Date("2021-10-01"))][]

dates <- as.Date(c("2021-03-20", "2021-03-25", "2021-03-30", "2021-04-05", "2021-04-10", "2021-04-15"))

for (i in seq_along(dates)) {
  
  if (i == 1) {
    d <- fread(glue("{path}{dates[i]}_{scen}_r{r_0}_data.txt"))[, scenario := glue("{scen} - {dates[i]}")]
  } else {
    d <- rbindlist(list(
      d,
      fread(glue("{path}{dates[i]}_{scen}_r{r_0}_data.txt"))[, scenario := glue("{scen} - {dates[i]}")]
    ))
  }
  
}

d <- rbindlist(list(d[, date := as.Date(date)], obs), fill = TRUE, use.names = TRUE)

d |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1250000)) +
  xlim(as.Date("2021-03-01"), as.Date("2021-06-30")) +
  labs(
    title = glue("pi = {scen}; r0 = {r_0}")
  )

ggsave(filename = glue("~/Downloads/change_time_p{scen}_r{r_0}.pdf"),
       width = 7, height = 5, device = cairo_pdf)
