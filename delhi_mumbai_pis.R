# libraries ------------
library(data.table)
library(ggplot2)
library(ggsci)

source("src/colores.R")

# dates ----------
delhi_start <- as.Date("2021-04-16")
delhi_end   <- as.Date("2021-06-14")

mumbai_start <- as.Date("2021-04-14")
mumbai_end   <- as.Date("2021-06-07")

# data -----------
x <- fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/covid19india_all_data_20211031.csv",
           showProgress = FALSE)
y <- covid19india::get_district_counts()[district == "Mumbai"]
setnames(y, old = "district", new = "place")

delhi  <- x[place == "Delhi"]
# kerala <- x[place == "Kerala"]
mumbai <- covid19india::get_r0(y)[, .(place, date, r_est = r, r_lower = lower, r_upper = upper)]

get_pi <- function(x, start, end = NULL, ceiling = TRUE) {
  
  start_r <- x[between(date, start - 7, start - 1), mean(r_est, na.rm = TRUE)]
  
  if (is.null(end)) {
    out <- x[date >= start][order(date), `:=` (pis = r_est / start_r, nom = 1:.N)][, smooth_pis := predict(loess(pis ~ nom, span = 0.5))][order(date), smooth_pis := nafill(smooth_pis, type = "locf")][]
  } else {
    out <- x[between(date, as.Date(start), as.Date(end))][order(date), `:=` (pis = r_est / start_r, nom = 1:.N)][, smooth_pis := predict(loess(pis ~ nom, span = 0.5))][order(date), smooth_pis := nafill(smooth_pis, type = "locf")][]
  }
  
  if (ceiling == TRUE) {
    out <- out[smooth_pis > 1, smooth_pis := 1][]
  }
  
  return(out)
  
}

delhi_pi <- get_pi(x = delhi, start = delhi_start, end = delhi_end)
delhi_pi |>
  ggplot(aes(x = date, y = smooth_pis)) +
  geom_line() +
  geom_line(aes(y = pis), linetype = 2)

mumbai_pi <- get_pi(x = mumbai, start = mumbai_start, end = mumbai_end)
mumbai_pi |>
  ggplot(aes(x = date, y = smooth_pis)) +
  geom_line() +
  geom_line(aes(y = pis), linetype = 2)


new_pis <- rbindlist(list(
  delhi_pi[, .(place, date, nom, pis, smooth_pis)][, date := as.Date(date)][, scenario := "Delhi PHI 2021"],
  mumbai_pi[, .(place, date, nom, pis, smooth_pis)][, date := as.Date(date)][, scenario := "Mumbai PHI 2021"]
))

old_pis <- fread("pi_schedule_extended.txt")[scenario %in% c("India lockdown - 200 days",
                                                               "Maharashtra lockdown - 200 days", "Maharashtra early",
                                                               "MH Pre-lock +20%")]
old_pis <- old_pis[scenario == "India lockdown - 200 days", scenario := "Strong lockdown"][
  scenario == "Maharashtra lockdown - 200 days", scenario := "Moderate lockdown"
][
  scenario == "Maharashtra early", scenario := "Strengthened PHI"
][
  scenario == "MH Pre-lock +20%", scenario := "Moderate PHI"
]

all_pis <- rbindlist(list(
  new_pis,
  old_pis[, date := as.Date(date)]
), fill = TRUE, use.names = TRUE)

use_colors <- c(
  "Mumbai PHI 2021" = pal_lancet()(3)[1],
  "Delhi PHI 2021" = pal_lancet()(3)[3],
  "Moderate PHI" = colores4[["Tier 2"]],
  "Strengthened PHI" = colores4[["Tier 3"]],
  "Moderate lockdown" = colores4[["Tier 4"]],
  "Strong lockdown" = "black")

(pi_plot <- all_pis |>
  ggplot(aes(x = nom, y = smooth_pis, color = scenario)) +
  geom_line(size = 1) +
  scale_color_manual(values = use_colors) +
  labs(
    title = "Comparing \u03c0(t) schedules",
    x     = "Days since start of intervention",
    y     = "\u03c0(t)"
  ))

ggsave("fig/response/alternate_pis.pdf", plot = pi_plot, width = 7, height = 5, device = cairo_pdf)
ggsave("fig/response/alternate_pis.png", plot = pi_plot, width = 7, height = 5, units = "in", dpi = 320, device = png)
