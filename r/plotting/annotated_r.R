library(data.table)
library(covid19india)
library(tidyverse)
library(glue)
library(here)

start <- as.Date("2021-01-01")
end   <- as.Date("2021-04-30")

india <- get_nat_counts(mohfw = FALSE)

india <- get_r0(india)[, .(place, date, r)][, r_t7 := frollmean(r, n = 7)][between(date, start, end)]

(r_plot <- india %>%
  ggplot(aes(x = date, y = r_t7)) +
  annotate(geom = "rect", xmin = min(india[, date]), xmax = max(india[, date]), ymin = 1, ymax = 1.2, fill = "orange", alpha = 0.5) +
  annotate(geom = "rect", xmin = min(india[, date]), xmax = max(india[, date]), ymin = 1.2, ymax = 1.4, fill = "red", alpha = 0.5) +
  annotate(geom = "rect", xmin = min(india[, date]), xmax = max(india[, date]), ymin = 1.4, ymax = Inf, fill = "purple", alpha = 0.5) +
  
  annotate(geom = "text", x = max(india[, date]) - 1, y = 1.02, color = "gray40", label = "Moderate PHI", fontface = "bold", hjust = 1, vjust = 0) + 
  annotate(geom = "text", x = max(india[, date]) - 1, y = 1.22, color = "gray40", label = "Strengthened PHI", fontface = "bold", hjust = 1, vjust = 0) + 
  annotate(geom = "text", x = max(india[, date]) - 1, y = 1.42, color = "gray40", label = "Moderate lockdown", fontface = "bold", hjust = 1, vjust = 0) + 
  
  geom_vline(xintercept = as.Date("2021-02-19"), linetype = 2) +
  geom_vline(xintercept = as.Date("2021-03-13"), linetype = 2) +
  geom_vline(xintercept = as.Date("2021-03-19"), linetype = 2) +
  
  annotate(geom = "label", x = as.Date("2021-02-19"), y = 1.59, label = "February 19\n(R>1)", size = 3, fontface = "bold", vjust = 0.6) +
  annotate(geom = "label", x = as.Date("2021-03-11"), y = 1.59, label = "March 13\n(R>1.2)", size = 3, fontface = "bold", vjust = 0.6) +
  annotate(geom = "label", x = as.Date("2021-03-21"), y = 1.59, label = "March 19\n(R>1.4)", size = 3, fontface = "bold", vjust = 0.6) +
  
  geom_hline(yintercept = 1) +
  geom_line(size = 1) +
  labs(
    title    = "Trailing 7-day average R in India",
    subtitle = paste0(format(start, '%B %e, %Y'), " to ", format(end, '%B %e, %Y')),
    x        = "Date",
    y        = "Trailing 7-day average R"
  ))

ggsave(
  filename = "fig/annotated_r.pdf",
  plot = r_plot,
  width = 9, height = 5,
  device = cairo_pdf
)

ggsave(
  filename = "fig/annotated_r.png",
  width = 9, height = 5, units = "in", dpi = 320
)
