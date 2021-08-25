# libraries ----------
pacman::p_load(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
               ggtext, patchwork)
source(here("src", "extract_cfr.R"))

end_date <- as.Date("2021-06-30")

# load data ----------
obs <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
              col_types = cols()) %>%
  clean_names() %>%
  rename(
    daily_cases  = daily_confirmed,
    daily_deaths = daily_deceased,
    total_cases  = total_confirmed,
    total_deaths = total_deceased
  ) %>%
  select(-date) %>%
  rename(date = date_ymd) %>%
  filter(date >= "2021-02-15")

scenarios <- c("4wk", "6wk", "8wk")

for (i in seq_along(scenarios)) {
  tmp_filename    <- glue("2021-04-15_smooth1_{scenarios[i]}_data.txt")
  if (i == 1) {
    p <- read_tsv(here("data", "lol",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("data", "lol",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

p <- p %>%
  mutate(
    scenario = case_when(
      scenario == "4wk" ~ "4 weeks",
      scenario == "6wk" ~ "6 weeks",
      scenario == "8wk" ~ "8 weeks"
    )
  )

tmp_outname  <- "fig_lol_case_plot.pdf"
# tmp_title    <- "Strong lockdown effect"
# tmp_mh_title <- "Moderate lockdown effect"

# p <- p %>%
#   mutate(
#     scenario = paste(
#       trimws(format(as.Date(scenario), '%B')),
#       trimws(format(as.Date(scenario), '%e'))
#     )
#   )
# 
# p_mh <- p_mh %>%
#   mutate(
#     scenario = paste(
#       trimws(format(as.Date(scenario), '%B')),
#       trimws(format(as.Date(scenario), '%e'))
#     )
#   )

# prepare data ----------
clean_prep <- function(x) {
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = end_date, end_date = end_date, scen = "No intervention")
  wk_4 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-14", end_date = end_date, scen = "4 weeks")
  wk_6 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-14", end_date = end_date, scen = "6 weeks")
  wk_8 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-14", end_date = end_date, scen = "8 weeks")
  
  total <- none %>%
    add_row(wk_4) %>% 
    add_row(wk_6) %>%
    add_row(wk_8)
  
  total.smoothed <- total %>% 
    filter(date <= end_date) %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total.smoothed.plot <- total.smoothed %>% 
    filter(scenario == "No intervention") %>%
    filter(date <= end_date) %>% 
    mutate(scenario = "Observed") %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "4 weeks", 
                     date >= "2021-04-03")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "6 weeks", 
                     date >= "2021-04-03")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "8 weeks", 
                     date >= "2021-04-03")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "4 weeks", "6 weeks", "8 weeks"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)
# total_mh_smoothed_plot <- clean_prep(x = p_mh)

# plot -----------
cases_p <- total_smoothed_plot %>% 
  filter(date >= "2021-02-15" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  xlab("Date") + 
  ylab("Daily cases") + 
  geom_vline(data = total_smoothed_plot %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  # geom_label_repel(data = total_smoothed_plot %>% 
  #                    group_by(scenario) %>% 
  #                    filter(fitted == max(fitted)) %>% 
  #                    dplyr::ungroup() %>% 
  #                    select(scenario, date, fitted) %>% 
  #                    filter(!(scenario %in% c("Observed", "No intervention"))), 
  #                   aes(x = date, 
  #                       y = fitted, 
  #                       label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
  #                       color = scenario,
  #                       family = "Lato"), 
  #                  nudge_y = 100000, 
  #                  nudge_x = -10, 
  #                  size = 4, 
  #                  show.legend  = FALSE, 
  #                  segment.size = 1) + 
  # geom_text_repel(data = total_smoothed_plot %>% 
  #                    group_by(scenario) %>% 
  #                    filter(date == min(date)) %>% 
  #                   distinct() %>%
  #                   slice(1) %>%
  #                    dplyr::ungroup() %>% 
  #                    select(scenario, date, fitted) %>% 
  #                    mutate(text = c("Observed data", "March 15\nlockdown", "March 30\nlockdown"), 
  #                           x = as.Date(c("2021-03-01", "2021-03-08", "2021-03-23")), 
  #                           y = c(50000, rep(500000, 2))), 
  #                  aes(x = x, 
  #                      y = y, 
  #                      label = text,
  #                      color = scenario,
  #                      family = "Lato"), 
  #                  nudge_x = -1,
  #                  size = 4,
  #                 hjust = 0,
  #                  show.legend  = FALSE, 
  #                  segment.size = 1) + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(    title    = "Predicted number of daily COVID-19 cases under\nlockdowns of differing lengths",
           subtitle = glue("February 15, 2021 to {format(end_date, '%B %e, %Y')}"),
           caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}.<br>",
                           "Figures in boxes show peak number of cases for each intervention.<br>",
                           "**\uA9 COV-IND-19 Study Group**"),
       y        = "Daily cases",
       x        = "",
       color    = "Date of intervention") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.title    = element_blank(),
    # legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 10),
    legend.position = "top",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = cases_p,
       height   = 5,
       width    = 7,
       units = "in", device = cairo_pdf)
