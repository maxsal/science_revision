# libraries ----------
ally::libri(tidyverse, lubridate, ggsci, ggrepel, data.table,
            janitor, glue, here, ggtext, patchwork)

source("esir_ally.R")

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
  filter(date >= "2021-01-01")

scenarios <- c("2021-03-13_t3", "2021-03-19_t4", "2021-03-30", "2021-04-15")
tiers     <- c("Tier 3", "Tier 4", "Tier 4 - March 30", "Tier 4 - April 15")

for (i in seq_along(scenarios)) {
  if (i %in% 1:2) {
    tmp_filename    <- glue("{scenarios[i]}_waning_data.txt")
  } else {
    tmp_filename    <- glue("{scenarios[i]}_waning_mh_data.txt")
  }
  
  if (i == 1) {
    p <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/{tmp_filename}"))[
      , scenario := tiers[i]
    ]
    
  } else {
    p <- rbindlist(list(
      p,
      fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/{tmp_filename}"))[
        , scenario := tiers[i]
      ]
    ))[]
  }
  
}

p_e <- fread("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-01-02_20pct_waning_data.txt")[, scenario := "Tier 2"][]

tmp_outname  <- "fig01_wane_case_plot.pdf"
tmp_title    <- "Strong lockdown effect"

p <- rbindlist(list(
  p,
  p_e
))

# prepare data ----------
clean_prep <- function(x) {
  
  none   <- obs %>% mutate(scenario = "No intervention") %>% select(scenario, date, incidence = daily_cases)
  feb_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-02-19", end_date = end_date,
                                   scen = "Tier 2")
  mar_13 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-13", end_date = end_date,
                                   scen = "Tier 3")
  mar_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-19", end_date = end_date,
                                   scen = "Tier 4")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", end_date = end_date,
                                   scen = "Tier 4 - March 30")
  apr_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", end_date = end_date,
                                   scen = "Tier 4 - April 15")
  
  total <- none %>%
    add_row(feb_19) %>%
    add_row(mar_13) %>% 
    add_row(mar_19) %>%
    add_row(mar_30) %>%
    add_row(apr_15)
  
  total.smoothed <- total %>% 
    filter(date <= end_date) %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.25),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total.smoothed.plot <- total.smoothed %>% 
    filter(scenario == "No intervention") %>%
    filter(date <= end_date) %>% 
    mutate(scenario = "Observed") %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 2", 
                     date >= "2021-02-08")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 3", 
                     date >= "2021-03-06")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4", 
                     date >= "2021-03-12")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - March 30", 
                     date >= "2021-03-23")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - April 15", 
                     date >= "2021-04-08")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 2", "Tier 3", "Tier 4", "Tier 4 - March 30", "Tier 4 - April 15"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot <- clean_prep(x = p)

# plot -----------
# tps[scenario == "Tier 2 - February 19", scenario := "Tier 2"]
# tps[scenario == "Tier 3 - March 13", scenario := "Tier 3"]
# tps[scenario == "Tier 4 - March 19", scenario := "Tier 4"]
# tps[, scenario := factor(scenario, levels = c("Observed", "Tier 2", "Tier 3", "Tier 4",
#                                               "Tier 4 - March 30", "Tier 4 - April 15"))]


## strong lockdown plot -----------
tsp <- as.data.table(total_smoothed_plot)
cases_p <- tsp[, lt := "solid"][scenario == "Tier 4 - March 30", lt := "longdash"][scenario == "Tier 4 - April 15", lt := "dotted"][] %>%
  filter(date >= "2020-12-01" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario, linetype = lt), size = 1) +
  scale_linetype_identity() +
  scale_color_manual(values = colores4) +
  geom_vline(data = total_smoothed_plot %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_label_repel(data = total_smoothed_plot %>% 
                     group_by(scenario) %>% 
                     filter(fitted == max(fitted)) %>% 
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     filter(!(scenario %in% c("Observed", "No intervention"))) %>%
                     add_row(scenario = "Observed", date = as.Date("2021-05-03"), fitted = 414280) %>%
                     mutate(fitted_val = case_when(scenario == "Observed" ~ tsp[scenario == "Observed" & incidence == max(incidence), fitted], T ~ fitted)) %>% distinct(), 
                    aes(x = date, 
                        y = fitted_val, 
                        label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
                        color = scenario,
                        family = "Helvetica Neue"), 
                   nudge_y = 100000, 
                   nudge_x = -10, 
                   size = 4, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text(data = tsp[, .SD[date == min(date)], by = scenario][, .SD[1], by = scenario][, .(scenario, date, fitted)][, `:=` (
    text = c("Observed data", "February 19\nModerate PHI\n(non-lockdown)", "March 13\nStrengthened PHI\n(non-lockdown)", "March 19\nModerate\nlockdown", "March 30\nModerate\nlockdown", "April 15\nModerate\nlockdown"), 
    x    = as.Date(c("2021-04-10", "2021-02-07", "2021-03-05", "2021-03-13", "2021-03-24", "2021-04-09")), 
    y    = c(125000, rep(350000, 5)) 
  )][],
  aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Helvetica Neue"),
  size = 4, hjust = c(0, 1, 1, 0, 0, 0), show.legend = FALSE) +
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = tmp_title,
       y        = "Daily cases",
       x        = "",
       color    = "Date of intervention") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") +
  theme_classic() +
  theme(
    text            = element_text(family = "Helvetica Neue"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

full_plt <- cases_p +
  labs(
    title    = "Predicted number of daily COVID-19 cases under various interventions with waning immunity",
    subtitle = glue("{format_date(cases_p$data[, min(date)])} to {format_date(cases_p$data[, max(date)])}"),
    caption  = glue("**Notes:** Observations and prediction period until {format_date(cases_p$data[, max(date)])}. ",
                    "Figures in boxes show peak number of cases for each intervention.<br>",
                    "**\uA9 COV-IND-19 Study Group**")
  )


# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = full_plt,
       height   = 5,
       width    = 15,
       units = "in", device = cairo_pdf)
