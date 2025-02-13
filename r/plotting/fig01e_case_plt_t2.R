# libraries ----------
pacman::p_load(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
               ggtext, patchwork, data.table)
f <- list.files(here("src"))
for (i in seq_along(f)) {source(here("src", f[i]))}

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2021-06-30")
r_0      <- 2

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
  filter(date >= start_date)

d0 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-02-19_20pct_t2_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - February 19")
d1 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-03-13_20pct_t2_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - March 13")
d2 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-03-19_20pct_t2_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - March 19")
d3 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-03-30_20pct_t2_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - March 30")
d4 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-04-15_20pct_t2_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - April 15")


d <- bind_rows(
  d0, d1, d2, d3, d4
)

p <- d %>% filter(scenario %in% c("Tier 2 - February 19", "Tier 2 - March 13", "Tier 2 - March 19", "Tier 2 - March 30", "Tier 2 - April 15"))

tmp_outname  <- glue("fig01_case_plot_t2_r{r_0}")
tmp_title    <- "Effect of different interventions at different times"

# prepare data ----------
clean_prep <- function(x) {
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = end_date + 14, end_date = end_date + 14, scen = "No intervention")
  feb_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-02-19", end_date = end_date, scen = "Tier 2 - February 19")
  mar_13 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-13", end_date = end_date, scen = "Tier 2 - March 13")
  mar_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-19", end_date = end_date, scen = "Tier 2 - March 19")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", end_date = end_date, scen = "Tier 2 - March 30")
  apr_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", end_date = end_date, scen = "Tier 2 - April 15")
  
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
              filter(scenario == "Tier 2 - February 19",
                     date >= "2021-02-10")) %>%
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 2 - March 13", 
                     date >= "2021-03-04")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 2 - March 19", 
                     date >= "2021-03-10")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 2 - March 30", 
                     date >= "2021-03-21")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 2 - April 15", 
                     date >= "2021-04-06")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 2 - February 19", "Tier 2 - March 13", "Tier 2 - March 19", "Tier 2 - March 30", "Tier 2 - April 15"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)
# total_mh_smoothed_plot <- clean_prep(x = p_mh)

obs <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                col_types = cols(), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(
    daily_cases  = daily_confirmed,
    daily_deaths = daily_deceased,
    total_cases  = total_confirmed,
    total_deaths = total_deceased
  ) %>%
  select(-date) %>%
  rename(date = date_ymd) %>%
  filter(date >= "2021-02-01")


# plot -----------
strong_cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Strong lockdown"]])

## strong lockdown plot -----------
tps <- as.data.table(total_smoothed_plot)[, scenario := factor(scenario, levels = c("Observed", "Tier 2 - February 19", "Tier 2 - March 13", "Tier 2 - March 19", "Tier 2 - March 30", "Tier 2 - April 15"))]
# tps[scenario == "Tier 2 - February 19", scenario := "Tier 2"]
# tps[scenario == "Tier 3 - March 13", scenario := "Tier 3"]
# tps[scenario == "Tier 4 - March 19", scenario := "Tier 4"]
# tps[, scenario := factor(scenario, levels = c("Observed", "Tier 2", "Tier 3", "Tier 4",
#                                               "Tier 4 - March 30", "Tier 4 - April 15"))]

colors <- c("black", pal_lancet()(length(tps[, unique(scenario)]) - 1))
names(colors) <- tps[, unique(scenario)]
# names(colores4) <- tps[, unique(scenario)]

cases_p <- tps[data.table::between(date, start_date, end_date)]%>%
  # filter(date >= "2021-02-01" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_color_manual(values = colors) +
  
  # vertical lines
  geom_vline(data = tps[, .SD[date == min(date)], by = scenario][, .(scenario, date)][!(scenario %in% c("Observed", "No intervention"))],
             aes(xintercept = date, color = scenario),
             linetype = "dashed") +
  
  # peak case count labels
  geom_label_repel(data = rbindlist(list(
    tps[, .SD[fitted == max(fitted)], by = scenario][, .(scenario, date, fitted)][!(scenario %in% c("Observed", "No intervention"))][, fitted_val := fitted][],
    data.table(scenario = "Observed", date = as.Date("2021-05-03"), fitted = 414280, fitted_val = tps[scenario == "Observed" & date == "2021-05-03", fitted])), fill = TRUE),
    aes(x = date, y = fitted_val, label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"), color = scenario, family = "Arial"),
    nudge_y = 100000,
    nudge_x = -10,
    size = 3,
    show.legend = FALSE,
    segment.size = 1) +
  
  # date labels
  geom_label(data = tps[, .SD[date == min(date)], by = scenario][, .(scenario, date, fitted)][, `:=` (
    text = c("Observed data", "February 19\nModerate PHI\n(non-lockdown)\nR(t)>1", "March 13\nModerate PHI\n(non-lockdown)\nR(t)>1.2", "March 19\nModerate PHI\n(non-lockdown)\nR(t)>1.4", "March 30\nModerate PHI\n(non-lockdown)", "April 15\nModerate PHI\n(non-lockdown)"), 
    x    = as.Date(c("2021-04-10", "2021-02-09", "2021-03-03", "2021-03-11", "2021-03-22", "2021-04-07")), 
    y    = c(125000, rep(tps[, max(fitted)]*0.9, 3), tps[, max(fitted)]*0.65, tps[, max(fitted)]*0.9) 
  )][],
  aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Arial"),
  size = 3, hjust = c(0, 1, 1, 0, 0, 0), show.legend = FALSE) +
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = tmp_title,
       y        = "Daily cases",
       x        = "",
       color    = "Date of intervention") +
  
  # other stuff
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  theme_classic() +
  theme(
    text            = element_text(family = "Arial"),
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

# combine plots -----------
full_plt <- cases_p +
  labs(
    title    = "Predicted number of daily COVID-19 cases under various interventions",
    subtitle = glue("{format(start_date, '%B %e, %Y')} to {format(end_date, '%B %e, %Y')}"),
    caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}. ",
                    "Figures in boxes show peak number of cases for each intervention. Assumes starting R\u2080={r_0}.<br>\"R(t)\" in annotations represents trailing 7-day average effective reproduction number threshold.<br>",
                    "**\uA9 COV-IND-19 Study Group**")
  )


# save output ----------
ggsave(filename = here("fig", "sensitivity", glue("{tmp_outname}.pdf")),
       plot     = full_plt,
       height   = 5,
       width    = 15,
       units = "in", device = cairo_pdf)
ggsave(filename = here("fig", "sensitivity", glue("{tmp_outname}.png")),
       plot     = full_plt,
       height   = 5,
       width    = 15,
       units = "in", dpi = 320, device = png)

