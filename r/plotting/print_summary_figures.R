ally::libri(data.table, covid19india, tidyverse, glue, here, ally, patchwork, ggtext)

source("src/colores.R")

cols <- c("place", "date", "daily_cases", "total_cases")
line_col <- "#138808"
unity_col <- "#FF9933"
start_date <- as.Date("2021-03-01")
end_date <- as.Date("2021-07-31")

dat <- fread("data/covid19india_state_counts_20211031.csv")[place == "Maharashtra"][, date := as.Date(date)][, ..cols]
r_dat <- covid19india::get_r0(dat)

(r_plot <- r_dat[between(date, as.Date("2021-03-01"), as.Date("2021-07-31"))][, color := fifelse(between(date, as.Date("2021-03-28"), as.Date("2021-04-14")), "#e0101a", fifelse(between(date, as.Date("2021-04-15"), as.Date("2021-07-31")), "#9900d1", line_col))][] |>
  ggplot(aes(x = date, y = r, color = color)) +
  geom_vline(xintercept = as.Date("2021-03-28"), linetype = 2, color = "gray40", size = 1) +
  geom_vline(xintercept = as.Date("2021-04-14"), linetype = 2, color = "gray40", size = 1) +
  geom_hline(yintercept = 1, color = "gray40", size = 1) + 
  geom_line(size = 1) +
  geom_point(shape = 3, size = 0.25, color = "black") +
  scale_color_identity() +
  scale_y_continuous(breaks = seq(0.6, 1.8, by = 0.2)) +
  labs(
    title = "Time-varying effective R in Maharashtra",
    x = "Date",
    y = expression(R[t]),
  ) +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9),
    axis.title.y    = element_text(size = 9),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  ))

# pis -----------
cols_1 <- c(
  "Moderate PHI" = colores[["MH Pre-lock +20%"]][[1]],
  "Strengthened PHI"    = colores[["MH Pre-lock"]][[2]],
  "Moderate lockdown"     = colores[["Moderate lockdown"]][[2]]
  # "Strong lockdown"       = colores[["Strong lockdown"]][[2]]
)

pis <- read_tsv(here("pi_schedule_extended.txt"),
                col_types = cols()) 
mh20_pis <- pis %>% filter(place == "Maharashtra early") %>%
  mutate(place = "MH Pre-lock +20%",
         smooth_pis = ifelse(smooth_pis * 1.2 > 1, 1, smooth_pis * 1.2)
  )

pis <- bind_rows(pis, mh20_pis) %>%
  filter(place %in% c("India", "Maharashtra", "Maharashtra early", "MH Pre-lock +20%")) %>%
  mutate(
    place = case_when(
      place == "India" ~ "Strong lockdown",
      place == "Maharashtra" ~ "Moderate lockdown",
      place == "Maharashtra early" ~ "Strengthened PHI",
      place == "MH Pre-lock +20%" ~ "Moderate PHI"
    )
  )

(pi_plot <- pis %>%
    filter(place != "Strong lockdown") %>%
    ggplot(aes(x = nom, y = smooth_pis, group = place, color = place)) +
    geom_hline(yintercept = 1, size = 1, linetype = 2, color = "gray40") +
    geom_line(size = 1) +
    labs(
      title   = "Intervention schedules",
      x       = "Days since start of intervention",
      y       = "\u03c0(t)",
      caption = "Note: Dashed line represents no change to pi schedule"
    ) +
    scale_color_manual(values = cols_1) +
    theme_classic() +
    theme(
      text            = element_text(family = "Helvetica Neue"),
      axis.text.x     = element_text(size = 9, vjust = 0.5),
      axis.text.y     = element_text(size = 9),
      axis.title.x    = element_text(size = 9),
      axis.title.y    = element_text(size = 9),
      legend.title    = element_blank(),
      legend.text     = element_text(size = 9),
      legend.position = "top",
      plot.title      = element_text(size = 12, face = "bold"),
      plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
      plot.caption    = element_markdown(size = 8, hjust = 0)
    ))

patched <- r_plot / pi_plot

ggsave(filename = "fig/response/r_pi_stack.pdf",
       width = 7, height = 5, device = cairo_pdf)
