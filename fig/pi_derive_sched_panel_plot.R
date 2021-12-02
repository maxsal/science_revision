# libraries --------------
ally::libri(data.table, covid19india, tidyverse, glue, here, ally, patchwork, ggtext)
source("esir_ally.R")

# specs --------------
start_date <- as.Date("2021-01-01")
end_date   <- as.Date("2021-07-31")
cols       <- c("place", "date", "daily_cases", "total_cases")
line_col   <- "#138808"
unity_col  <- "#FF9933"
mark_dates <- c("2021-03-28", "2021-04-14", "2021-06-07")

cols_1 <- c(
  "Moderate PHI\n(non-lockdown)"     = colores4["Tier 2"],
  "Strengthened PHI\n(non-lockdown)" = colores4["Tier 3"],
  "Moderate lockdown"                = colores4["Tier 4"],
  "Strong lockdown"                  = colores4["Observed"]
)

cols_2 <- c(
  "Moderate CFR" = colores["Tier 3"],
  "High CFR"     = colores["Tier 4"],
  "Low CFR"      = colores["Tier 2"]
)

# data --------------
data <- fread("data/covid19india_state_counts_20211031.csv")[place == "Maharashtra"][, date := as.Date(date)][, ..cols]

r_data <- covid19india::get_r0(data)

combo <- data.table::merge.data.table(
  data,
  r_data,
  by = c("place", "date"),
  all.x = TRUE
)[between(date, start_date, end_date)]

start_r <- mean(combo[between(date, as.Date("2021-03-28") - 7, as.Date("2021-03-28") - 1), r], na.rm = TRUE)

combo <- combo[date >= as.Date("2021-03-28"), pis := r / start_r][
  date >= as.Date("2021-03-28"), smooth_pis := predict(loess(pis ~ as.numeric(date), span = 1))][]

schedules <- fread("pi_schedule_extended.txt")
# plots ----------

## panel a: daily cases ------------
case_plot <- combo[daily_cases >= 0] |>
  ggplot(aes(x = date, y = daily_cases))

if (!is.null(mark_dates)) {
  for(i in seq_along(mark_dates)) {
    case_plot <- case_plot + geom_vline(xintercept = as.Date(mark_dates[i]), linetype = 2, color = "gray40")
  }
}

case_plot <- case_plot +
  geom_line(size = 1, color = line_col) +
  geom_point(size = 0.25, shape = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B %Y") +
  labs(
    title = "Daily COVID-19 case count in Maharashtra",
    x = "Date",
    y = "Daily case count"
  )

## panel b: time-varying r plot -----------
r_plot <- combo[!is.na(r)] |>
  ggplot(aes(x = date, y = r))

if (!is.null(mark_dates)) {
  for(i in seq_along(mark_dates)) {
    r_plot <- r_plot + geom_vline(xintercept = as.Date(mark_dates[i]), linetype = 2, color = "gray40")
  }
}

r_plot <- r_plot +
  geom_hline(yintercept = 1, color = unity_col, size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = line_col, alpha = 0.5) +
  geom_line(color = line_col, size = 1) +
  geom_point(size = 0.25, shape = 3) +
  scale_x_date(date_labels = "%B %Y") +
  labs(
    title = "Time-varying R in Maharashtra", x = "Date", y = "R(t)"
  )

## panel c: pi plot ----------
pi_plot <- combo |>
  ggplot(aes(x = date, y = smooth_pis))

if (!is.null(mark_dates)) {
  for(i in seq_along(mark_dates)) {
    pi_plot <- pi_plot + geom_vline(xintercept = as.Date(mark_dates[i]), linetype = 2, color = "gray40")
  }
}

pi_plot <- pi_plot +
  geom_line(size = 1, color = line_col) +
  geom_point(size = 0.25, shape = 3) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B %Y") +
  labs(
    title = "Intervention schedule (\u03c0(t))",
    x = "Date",
    y = "\u03c0(t)"
  )

## panel d: pi schedules ------------
