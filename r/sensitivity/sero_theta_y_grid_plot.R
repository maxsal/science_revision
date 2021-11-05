library(data.table)
library(ggplot2)
library(glue)

path <- "/Volumes/tiny/projects/covid/science_revision/test/no_intervention/"

r_0  <- 5
seroprevs <- c(0.05, 0.10, 0.15, 0.20, 0.216, 0.25)

obs <- covid19india::get_nat_counts()[, .(date, incidence = daily_cases)][, scenario := "Observed"][between(date, as.Date("2021-03-01"), as.Date("2021-10-01"))][]

######
extracto <- function(y, start = as.Date("2021-03-19"), N = 1.34e9, nums = FALSE) {
  
  out <- data.table(
    lower = sapply(as.data.table(y), function(x) stats::quantile(x, names = FALSE, probs = 0.025)),
    median = sapply(as.data.table(y), function(x) stats::quantile(x, names = FALSE, probs = 0.5)),
    upper = sapply(as.data.table(y), function(x) stats::quantile(x, names = FALSE, probs = 0.975))
  )
  
  
  if (nums == TRUE) {
    out <- out * N
  }
  
  out <- out[, date := seq.Date(from = start, by = "day", length.out = .N)][]
  
  return(out)
}

get_ratio <- function(z) {

  load(z)
  
  theta_s <- theta_pp[, , 1]
  theta_i <- theta_pp[, , 2]
  theta_r <- theta_pp[, , 3]
  
  y_pp_tab <- extracto(Y_pp)
  r_pp_tab <- extracto(R_pp)
  
  s_tab <- extracto(theta_s)
  
  theta_i_r <- 1 - s_tab[, median]
  
  y <- data.table(
    date = y_pp_tab[, date],
    median = y_pp_tab[, median] + r_pp_tab[, median]
  )
  
  y_i_r <- y[, median]
  
  ratio <- data.table(
    date = s_tab[, date],
    ratio = theta_i_r / y_i_r
  )
  
  return(ratio)
  
}

######
for (i in seq_along(seroprevs)) {
  
  if (i == 1) {
    d <- get_ratio(glue("{path}2021-03-19_no_intervention_r{r_0}_s{seroprevs[i]}_mod_forecast_MCMC.RData"))[, scenario := glue("Seroprev - {seroprevs[i]}")]
  } else {
    d <- rbindlist(list(
      d,
      get_ratio(glue("{path}2021-03-19_no_intervention_r{r_0}_s{seroprevs[i]}_mod_forecast_MCMC.RData"))[, scenario := glue("Seroprev - {seroprevs[i]}")]
    ))
  }
  
}

d |>
  ggplot(aes(x = date, y = ratio, color = scenario)) +
  geom_line(size = 1) +
  xlim(as.Date("2021-03-01"), as.Date("2021-06-30")) +
  labs(
    title = glue("r0 = {r_0}")
  )

ggsave(filename = glue("~/Downloads/change_seroprev_r{r_0}.pdf"),
       width = 7, height = 5, device = cairo_pdf)

d[order(date), ratio_t7 := frollmean(ratio, n = 7, na.rm = TRUE), by = scenario] |>
  ggplot(aes(x = date, y = ratio_t7, color = scenario)) +
  geom_line(size = 1) +
  xlim(as.Date("2021-03-19"), as.Date("2021-06-30")) +
  labs(
    title = glue("r0 = {r_0}")
  )

ggsave(filename = glue("~/Downloads/change_seroprev_t7_r{r_0}.pdf"),
       width = 7, height = 5, device = cairo_pdf)
