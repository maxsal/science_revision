library(data.table)
library(janitor)
library(ggplot2)
library(patchwork)

hosp_cap <- 1.9e6 / 1.366e9 * 10000 # beds per 10,000
icu_cap  <- 95000 / 1.366e9 * 10000 # beds per 10,000

line_col  <- "#138808"
unity_col <- "#FF9933"

start_date <- "2021-01-01"
end_date   <- "2021-07-31"

dat <- fread("http://data.covid19india.org/csv/latest/case_time_series.csv",
             showProgress = FALSE)[, -c("Date")]
setnames(dat, "Date_YMD", "date")
setnames(dat, names(dat), make_clean_names(names(dat)))

dat <- dat[data.table::between(date, as.Date(start_date), as.Date(end_date))][
  , active_cases := (total_confirmed - total_recovered) / 10000][
    , `:=` (hosp_cases = active_cases * 0.075, icu_cases = active_cases * (0.075 * 0.11))][]

hosp_plot <- dat |>
  ggplot(aes(x = date, y = hosp_cases)) +
  geom_ribbon(data = dat %>% filter(hosp_cases > hosp_cap), aes (x = date, ymax = hosp_cases), ymin = hosp_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = hosp_cap, color = unity_col, size = 1) +
  geom_line(size = 1, color = line_col) +
  geom_point(size = 0.25, shape = 3) +
  annotate(geom = "text", label = glue::glue("{format(round(hosp_cap, 1), nsmall = 1)} hospital beds per 10,000"),
           x = as.Date(start_date), y = hosp_cap + 2, hjust = 0, color = unity_col, fontface = "bold") + 
  labs(
    title = "Hospital capacity",
    x = "Date",
    y = "Estimated hospitalized cases (per 10,000)"
  ) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  ggplot2::theme(
    text            = ggplot2::element_text(family = "Lato"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )

icu_plot <- dat |>
  ggplot(aes(x = date, y = icu_cases)) +
  geom_ribbon(data = dat %>% filter(icu_cases > icu_cap), aes (x = date, ymax = icu_cases), ymin = icu_cap, fill = "red", alpha = 0.4, color = NA) +
  geom_hline(yintercept = icu_cap, color = unity_col, size = 1) +
  geom_line(size = 1, color = line_col) +
  geom_point(size = 0.25, shape = 3) +
  annotate(geom = "text", label = glue::glue("{format(round(icu_cap, 1), nsmall = 1)} ICU beds per 10,000"),
           x = as.Date(start_date), y = icu_cap + .25, hjust = 0, color = unity_col, fontface = "bold") + 
  labs(
    title = "ICU capacity",
    x = "Date",
    y = "Estimated ICU cases (per 10,000)"
  ) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  ggplot2::theme(
    text            = ggplot2::element_text(family = "Lato"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )

patched <- hosp_plot / icu_plot

cairo_pdf(filename = here("fig", "hospital_capacity_plot.pdf"), width = 8, height = 7)
print(patched)
dev.off()

