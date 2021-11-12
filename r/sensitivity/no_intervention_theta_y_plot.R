library(data.table)
library(glue)
library(ggplot2)

fldr <- "data"
r_0  <- 2

file <- glue("/Volumes/tiny/projects/covid/science_revision/{fldr}/no_intervention/2021-03-19_no_intervention_r{r_0}_forecast_MCMC.RData")

load(file)

count <- fread("data_for_lockdown_extended.csv")

theta_s <- theta_pp[, , 1]
theta_i <- theta_pp[, , 2]
theta_r <- theta_pp[, , 3]

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

(ratio_plot <- ratio |>
  ggplot(aes(x = date, y = ratio)) +
  geom_line(size = 1) +
  labs(
    title = "No intevention starting March 19, r0 = 2",
    x = "Date",
    y = "I+R Ratio [\u0398] / Y"
  ) +
  theme(text = element_text(family = "Helvetica Neue")))

tag <- ifelse(fldr == "data", "prod", "test")

ggsave(filename = glue("fig/sensitivity/theta_y_no_int_r{r_0}_{tag}_ratio_plot.pdf"),
       plot = ratio_plot,
       width = 7, height = 5, device = cairo_pdf)

ggsave(filename = glue("fig/sensitivity/theta_y_no_int_r{r_0}_{tag}_ratio_plot.png"),
       plot = ratio_plot, units = "in", dpi = 320,
       width = 7, height = 5, device = png)
