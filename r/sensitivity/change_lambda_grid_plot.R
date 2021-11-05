library(data.table)
library(ggplot2)
library(glue)

l_values <- expand.grid(lY = 10^c(-1, -2, -3), lR = 10^c(-1, -2, -3))

obs <- get_nat_counts(mohfw = FALSE)[between(date, as.Date("2021-03-18"), as.Date("2021-08-15"))][, `:=` (scenario = "Observed", lY = 0.001, lR = 0.001)][, .(date, incidence = daily_cases, scenario, lY, lR)]

for (i in 1:nrow(l_values)) {
  
  if (i == 1) {
    
    x <- fread(glue("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-18_no_intervention_r2_lY{l_values$lY[i]}_lR{l_values$lR[i]}_mod_data.txt"))[, `:=` (scenario = glue("lY{l_values$lY[i]} - lR{l_values$lR[i]}"), lY = l_values$lY[i], lR = l_values$lR[i])]
    
  } else {
    
    if (!file.exists(glue("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-18_no_intervention_r2_lY{l_values$lY[i]}_lR{l_values$lR[i]}_mod_data.txt"))) { next }
    
    x <- rbindlist(list(
      x, 
      fread(glue("/Volumes/tiny/projects/covid/science_revision/test/no_intervention/2021-03-18_no_intervention_r2_lY{l_values$lY[i]}_lR{l_values$lR[i]}_mod_data.txt"))[, `:=` (scenario = glue("lY{l_values$lY[i]} - lR{l_values$lR[i]}"), lY = l_values$lY[i], lR = l_values$lR[i])]
    ))
    
    
  }
}


x <- rbindlist(list(obs, x[, date := as.Date(date)]), fill = TRUE)

x |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Varying lY and lR simultaneously")
ggsave(filename = "~/Downloads/lambda_simultaneous.pdf", width = 7, height = 5, device = cairo_pdf)

x[lY == 0.001] |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Varying lR holding lY @ 0.001")
ggsave(filename = "~/Downloads/lambda_lY_constant.pdf", width = 7, height = 5, device = cairo_pdf)

x[lR == 0.001] |>
  ggplot(aes(x = date, y = incidence, color = scenario)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Varying lY holding lR @ 0.001")
ggsave(filename = "~/Downloads/lambda_lR_constant.pdf", width = 7, height = 5, device = cairo_pdf)
