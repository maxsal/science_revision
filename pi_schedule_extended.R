ally::libri(tidyverse, here, glue, ggtext)

dat <- read_csv("data_for_lockdown_extended.csv", col_types = cols()) %>%
  filter(date <= "2021-08-15")

pi_sched  <- read_tsv("pi_schedule.txt", col_types = cols())

# india ------------
location   <- "India"
start_proj <- as.Date("2020-03-27")
length_out <- 199
end_proj   <- start_proj + length_out
dates      <- seq.Date(from = start_proj, to = end_proj, by = "day")

start_r <-  (dat %>% filter(place == location & between(date, start_proj - 7, start_proj - 1)) %>% pull(r_est) %>% mean(na.rm = T)) 

pi_sched_ext <- dat %>%
  filter(place == location & between(date, start_proj, end_proj)) %>%
  select(place, date, cases, daily_cases, r_est) %>%
  arrange(date) %>%
  mutate(
    pis = r_est / start_r,
    nom = 1:nrow(.)
  ) %>%
  mutate(
    smooth_pis = predict(loess(pis ~ nom, span = 0.5))
  )

if (nrow(pi_sched_ext) < length(dates)) {
  pi_sched_ext <- bind_rows(
    pi_sched_ext,
    tibble(date = seq.Date(from = max(pi_sched_ext$date) + 1, length.out = length(dates) - nrow(pi_sched_ext), by = "day"), smooth_pis = NA),
    ) %>%
    mutate(smooth_pis = data.table::nafill(smooth_pis, type = "locf"))
}

pi_sched_ext_ind <- pi_sched_ext %>%
  mutate(
    smooth_pis = case_when(smooth_pis > 1 ~ 1, T ~ smooth_pis),
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    scenario = "India lockdown - 200 days"
  )

  v_date <- "2020-06-01"

comp_plt <- bind_rows(
  pi_sched %>% filter(place == location) %>% select(place, date, smooth_pis) %>% mutate(schedule = "old"),
  pi_sched_ext_ind %>% select(place, date, smooth_pis) %>% mutate(schedule = "new")
) %>%
  ggplot(aes(x = date, y = smooth_pis, group = schedule, color = schedule)) +
  geom_vline(xintercept = as.Date(v_date), color = "gray40", size = 1, linetype = 2) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_labels = "%b %Y") +
  labs(
    title = glue("Comparison of {location} lockdown pi schedules"),
    x     = "Date",
    y     = "Pi",
    caption = "Both schedules start on March 27, 2020.<br>**Old:** LOESS smoother (span = 1) using data from 3/27 through 7/25 (120-days).<br>**New:** LOESS smoother (span = 1), using data from 3/27 through 10/12 (200 days)."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )

comp_plt

ggsave(plot = comp_plt,
       filename = here("fig", "diagnostic", glue("{location}_pi_comp.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# Maharashtra lockdown 200-day-------------
location   <- "Maharashtra"
start_proj <- as.Date("2021-04-14")
length_out <- 199
end_proj   <- start_proj + length_out
dates      <- seq.Date(from = start_proj, to = end_proj, by = "day")

start_r <-  (dat %>% filter(place == location & between(date, start_proj - 7, start_proj - 1)) %>% pull(r_est) %>% mean(na.rm = T)) 

pi_sched_ext <- dat %>%
  filter(place == location & between(date, start_proj, end_proj)) %>%
  select(place, date, cases, daily_cases, r_est) %>%
  arrange(date) %>%
  mutate(
    pis = r_est / start_r,
    nom = 1:nrow(.)
  ) %>%
  mutate(
    smooth_pis = predict(loess(pis ~ nom, span = 1))
  )

# if (nrow(pi_sched_ext) < length(dates)) {
#   pi_sched_ext <- bind_rows(
#     pi_sched_ext,
#     tibble(date = seq.Date(from = max(pi_sched_ext$date) + 1, length.out = length(dates) - nrow(pi_sched_ext), by = "day"), smooth_pis = NA),
#   ) %>%
#     mutate(smooth_pis = data.table::nafill(smooth_pis, type = "locf"))
# }

pi_sched_ext_mh <- pi_sched_ext %>%
  mutate(
    smooth_pis = case_when(smooth_pis > 1 ~ 1, T ~ smooth_pis),
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    scenario = "Maharashtra lockdown - 200 days"
  )

v_date <- "2021-06-07"

comp_plt <- bind_rows(
  pi_sched %>% filter(place == location) %>% select(place, date, smooth_pis) %>% mutate(schedule = "old"),
  pi_sched_ext_mh %>% select(place, date, smooth_pis) %>% mutate(schedule = "new")
) %>%
  ggplot(aes(x = date, y = smooth_pis, group = schedule, color = schedule)) + geom_vline(xintercept = as.Date(start_proj) + (4*7), color = "blue", size = 1, linetype = 2) +
  geom_vline(xintercept = as.Date(start_proj) + (6*7), color = "blue", size = 1, linetype = 2) +
  geom_vline(xintercept = as.Date(start_proj) + (8*7), color = "blue", size = 1, linetype = 2) +
  
  geom_vline(xintercept = as.Date(v_date), color = "gray40", size = 1, linetype = 2) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_labels = "%b %Y") +
  labs(
    title = glue("Comparison of {location} lockdown pi schedules"),
    x     = "Date",
    y     = "Pi",
    caption = "Both schedules start on April 14, 2021.<br>**Old:** LOESS smoother (span = 1) using data from 4/14 through 8/11 (120-days, with smoothing).<br>**New:** LOESS smoother (span = 1), using data from 4/14 through 8/15 (200 days)."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )

ggsave(plot = comp_plt,
       filename = here("fig", "diagnostic", glue("{location}_pi_comp.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# Maharashtra early intervention -----------
location   <- "Maharashtra"
start_proj <- as.Date("2021-03-28")
end_proj   <- as.Date("2021-04-14")
dates      <- seq.Date(from = start_proj, to = end_proj, by = "day")
length_out <- length(dates)
end_proj   <- start_proj + length_out


start_r <-  (dat %>% filter(place == location & between(date, start_proj - 7, start_proj - 1)) %>% pull(r_est) %>% mean(na.rm = T))

pi_sched_ext <- dat %>%
  filter(place == location & between(date, start_proj, end_proj)) %>%
  select(place, date, cases, daily_cases, r_est) %>%
  arrange(date) %>%
  mutate(
    pis = r_est / start_r,
    nom = 1:nrow(.)
  ) %>%
  mutate(
    smooth_pis = predict(loess(pis ~ nom, span = 1))
  )

pi_sched_ext_mh <- pi_sched_ext %>%
  mutate(
    smooth_pis = case_when(smooth_pis > 1 ~ 1, T ~ smooth_pis),
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    scenario = "Maharashtra pre-lockdown"
  )

pi_sched_early_mh <- tibble(
  date = seq.Date(from = start_proj, length.out = 100, by = "day"),
  pis = data.table::nafill(pi_sched_ext$smooth_pis[1:100], type = "locf")) %>%
  mutate(
    smooth_pis     = predict(loess(pis ~ as.numeric(date), span = 1)),
    place          = "Maharashtra early",
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    nom            = 1:100
    )
  
pi_sched_early_mh %>%
  ggplot(aes(x = date, y= smooth_pis)) +
  geom_line(size = 1) +
  theme_minimal()


# combine ------------
pi_ext_comb <- bind_rows(
  pi_sched_ext_ind,
  pi_sched_ext_mh,
  pi_sched_early_mh
)

write_tsv(x = pi_ext_comb, file = "pi_schedule_extended.txt")
