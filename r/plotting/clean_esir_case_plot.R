# libraries ----------
ally::libri(tidyverse, lubridate, ggsci, ggrepel, janitor, glue,
            ggtext, patchwork, data.table, scales)

source("esir_ally.R")

start_date <- as.Date("2021-01-01")
end_date   <- as.Date("2021-06-30")
r_0        <- 2

output_filename <- glue("clean_esir_case_plot_r{r_0}")
plot_title      <- "Effect of different interventions at different times"

# load data ----------
obs <- fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/covid19india_national_counts_20211031.csv",
             showProgress = FALSE)[, date := as.Date(date)][date >= start_date]

d0 <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-02-19_20pct_t2_r{r_0}_data.txt"))[, `:=` (start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - February 19")]
d1 <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-03-13_t3_r{r_0}_data.txt"))[, `:=` (start_date = "2021-03-13", tier = "Tier 3", scenario = "Tier 3 - March 13")]
d2 <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-03-19_t4_r{r_0}_data.txt"))[, `:=` (start_date = "2021-03-19", tier = "Tier 4", scenario = "Tier 4 - March 19")]
d3 <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-03-30_t4_r{r_0}_data.txt"))[, `:=` (start_date = "2021-03-30", tier = "Tier 4", scenario = "Tier 4 - March 30")]
d4 <- fread(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-04-15_t4_r{r_0}_data.txt"))[, `:=` (start_date = "2021-04-15", tier = "Tier 4", scenario = "Tier 4 - April 15")]

model_output_data <- rbindlist(list(d0, d1, d2, d3, d4))

# prepare data ----------
clean_prep <- function(x) {
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = end_date + 14, end_date = end_date + 14, scen = "No intervention")
  feb_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-02-19", end_date = end_date, scen = "Tier 2 - February 19")
  mar_13 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-13", end_date = end_date, scen = "Tier 3 - March 13")
  mar_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-19", end_date = end_date, scen = "Tier 4 - March 19")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", end_date = end_date, scen = "Tier 4 - March 30")
  apr_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", end_date = end_date, scen = "Tier 4 - April 15")
  
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
              filter(scenario == "Tier 3 - March 13", 
                     date >= "2021-03-04")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - March 19", 
                     date >= "2021-03-10")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - March 30", 
                     date >= "2021-03-21")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - April 15", 
                     date >= "2021-04-06")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 2 - February 19", "Tier 3 - March 13", "Tier 4 - March 19", "Tier 4 - March 30", "Tier 4 - April 15"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

smoothed_plot_data <- as.data.table(clean_prep(x = model_output_data))

obs <- obs[date >= "2021-02-01"]

# plot -----------
smoothed_plot_data <- smoothed_plot_data[
  scenario == "Tier 2 - February 19", scenario := "Tier 2"][
    scenario == "Tier 3 - March 13", scenario := "Tier 3"][
      scenario == "Tier 4 - March 19", scenario := "Tier 4"][
        , scenario := factor(scenario, levels = c("Observed", "Tier 2",
                                                  "Tier 3", "Tier 4",
                                                  "Tier 4 - March 30",
                                                  "Tier 4 - April 15"))]

case_plot <- smoothed_plot_data[data.table::between(date, start_date, end_date)][, lt := "solid"][scenario == "Tier 4 - March 30", lt := "longdash"][scenario == "Tier 4 - April 15", lt := "dotted"][] %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario, linetype = lt), size = 1) +
  scale_linetype_identity() +
  scale_color_manual(values = colores4) +
  
  # vertical lines
  geom_vline(data = smoothed_plot_data[
    , .SD[date == min(date)], by = scenario][
      , .(scenario, date)][
        !(scenario %in% c("Observed", "No intervention"))],
    aes(
      xintercept = date,
      color      = scenario
      ),
    linetype = "dashed") +
  
  # peak case count labels
  geom_label_repel(data = rbindlist(
    list(smoothed_plot_data[, .SD[fitted == max(fitted)], by = scenario][
      , .(scenario, date, fitted)][
        !(scenario %in% c("Observed", "No intervention"))][
          , fitted_val := fitted][],
      data.table(
        scenario   = "Observed",
        date       = as.Date("2021-05-03"),
        fitted     = 414280,
        fitted_val = smoothed_plot_data[scenario == "Observed" & date == "2021-05-03", fitted])),
    fill = TRUE),
    aes(
      x      = date,
      y      = fitted_val,
      label  = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
      color  = scenario,
      family = "Arial"),
    nudge_y = 100000,
    nudge_x = -10,
    size = 3.5,
    show.legend = FALSE,
    segment.size = 1) +
  
  # date labels
  geom_text(data = smoothed_plot_data[, .SD[date == min(date)], by = scenario][
    , .(scenario, date, fitted)][
      , `:=` (
        text = c("Observed data", "February 19\nModerate PHI\n(non-lockdown)\nR(t)>1",
                 "March 13\nStrengthened PHI\n(non-lockdown)\nR(t)>1.2",
                 "March 19\nModerate\nlockdown\nR(t)>1.4",
                 "March 30\nModerate\nlockdown",
                 "April 15\nModerate\nlockdown"), 
        x    = as.Date(c("2021-04-10", "2021-02-09", "2021-03-03", "2021-03-11", "2021-03-22", "2021-04-07")), 
        y    = c(125000, rep(350000, 5)) 
  )][],
  aes(
    x = x,
    y = y,
    label = text,
    color = scenario,
    vjust = 1,
    family = "Arial"),
  size = 3.5, hjust = c(0, 1, 1, 0, 0, 0), show.legend = FALSE) +
  labs(title    = plot_title,
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
full_plot <- case_plot +
  labs(
    title    = "Predicted number of daily COVID-19 cases under various interventions",
    subtitle = glue("{format_date(start_date)} to {format_date(end_date)}"),
    caption  = glue("**Notes:** Observations and prediction period until {format_date(end_date)}. ",
                    "Figures in boxes show peak number of cases for each intervention. Assumes starting R\u2080={r_0}.<br>\"R(t)\" in annotations represents trailing 7-day average effective reproduction number threshold.<br>",
                    "**\uA9 COV-IND-19 Study Group**")
  )


# save output ----------
ggsave(filename = glue("fig/{output_filename}.pdf"),
       plot     = full_plot,
       height   = 5,
       width    = 15,
       device   = cairo_pdf)

ggsave(filename = glue("fig/{output_filename}.png"),
       plot     = full_plot,
       height   = 5,
       width    = 15,
       units    = "in",
       dpi      = 320,
       device   = png)


