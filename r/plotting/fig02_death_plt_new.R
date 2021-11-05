# libraries ----------
ally::libri(tidyverse, lubridate, ggsci, ggrepel, janitor, glue,
            here, ggtext, patchwork)

f <- list.files(here("src"))
for (i in seq_along(f)) {source(here("src", f[i]))}

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2021-06-30")
r_0 <- 2

# # use maharashtra pi schedule? use kerala cfr schedule? ----------
mh <- TRUE
if (mh == TRUE) {
  tmp_outname <- glue("fig02_death_r{r_0}_plot.pdf")
  plt_title   <- "Predicted number of daily COVID-19 deaths under various interventions"
  cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Moderate lockdown"]])
  names(cols)[names(cols) == "MH Pre-lock"] <- "Early intervention"
} else {
  tmp_outname <- "fig03_death_plot.pdf"
  plt_title   <- "Predicted number of daily COVID-19 deaths under strong lockdown effect"
  cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Strong lockdown"]])
  names(cols)[names(cols) == "MH Pre-lock"] <- "Early intervention"
}


# load data -----------
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
  filter(date >= start_date) %>% 
  mutate(cfr = daily_deaths/daily_cases)

d1 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-03-13_t3_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-03-13", tier = "Tier 3", scenario = "Tier 3 - March 13")
d2 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-03-19_t4_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-03-19", tier = "Tier 4", scenario = "Tier 4 - March 19")
d3 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-03-30_t4_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-03-30", tier = "Tier 4", scenario = "Tier 4 - March 30")
d4 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/2021-04-15_t4_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-04-15", tier = "Tier 4", scenario = "Tier 4 - April 15")
d0 <- read_tsv(glue("/Volumes/tiny/projects/covid/science_revision/data/early_intervention/2021-02-19_20pct_t2_r{r_0}_data.txt"), col_types = cols()) %>%
  mutate(start_date = "2021-02-19", tier = "Tier 2", scenario = "Tier 2 - February 19")


m <- bind_rows(
  d0, d1, d2, d3, d4
)

p <- m %>% filter(scenario %in% c("Tier 2 - February 19", "Tier 3 - March 13", "Tier 4 - March 19", "Tier 4 - March 30", "Tier 4 - April 15"))

# extract CFR schedule and get plot defaults -----------
d <- extract_cfr(end_date = end_date)[date >= as.Date(start_date)]

# prepare data -----------
clean_prep <- function(x, var) {
  
  none   <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = end_date + 14, scen = "No intervention")
  feb_19 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-02-19", scen = "Tier 2 - February 19")
  mar_13 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-13", scen = "Tier 3 - March 13")
  mar_19 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-19", scen = "Tier 4 - March 19")
  mar_30 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-30", scen = "Tier 4 - March 30")
  apr_15 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-04-15", scen = "Tier 4 - April 15")
  
  
  total <- obs %>% 
    filter(date <= end_date) %>% 
    select(date, daily_deaths) %>% 
    rename(incidence = daily_deaths) %>% 
    add_column(scenario = "Observed") %>% 
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
  
  total_smoothed_plot <- total.smoothed %>% 
    filter(scenario == "Observed") %>% 
    filter(date <= end_date)  %>% 
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
                     date >= "2021-03-22")) %>%
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - April 15", 
                     date >= "2021-04-08")) %>%
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 2 - February 19", "Tier 3 - March 13", "Tier 4 - March 19", "Tier 4 - March 30", "Tier 4 - April 15")))%>%
    filter(date <= end_date) 
  
  return(total_smoothed_plot)
  
}

# tsp_india <- total_smoothed_plot
tsp_india <- clean_prep(x = p, var = cfr_t7)
tsp_mh    <- clean_prep(x = p, var = cfr_mh_t7)
tsp_kl    <- clean_prep(x = p, var = cfr_kl_t7)

make_tps <- function(x) {
  tmp <- as.data.table(x)
  tmp[scenario == "Tier 2 - February 19", scenario := "Tier 2"]
  tmp[scenario == "Tier 3 - March 13", scenario := "Tier 3"]
  tmp[scenario == "Tier 4 - March 19", scenario := "Tier 4"]
  tmp[, scenario := factor(scenario, levels = c("Observed", "Tier 2", "Tier 3", "Tier 4", "Tier 4 - March 30", "Tier 4 - April 15"))]
  
  return(tmp[])
}

tps_india <- make_tps(tsp_india)
tps_mh    <- make_tps(tsp_mh)
tps_kl    <- make_tps(tsp_kl)

# plot ----------
death_plt <- function(dat, title,
                      tmp_nudge = 500, tmp_repel_y = c(2000, rep(4000, 5))) {
    
  # plt_def <- get_plt_def(mh = FALSE, kl = FALSE)
    
  deaths_p <- dat[data.table::between(date, start_date, end_date)][, lt := "solid"][scenario == "Tier 4 - March 30", lt := "longdash"][scenario == "Tier 4 - April 15", lt := "dotted"][] %>% 
  ggplot() + 
  geom_line(aes(x = date, y = fitted, color = scenario, linetype = lt), size = 1) +
    scale_linetype_identity() +
  scale_color_manual(values = colores4) +
    labs(y        = "Daily cases",
         x        = "",
         color    = "Date of intervention") +
    
    # vertical lines
    geom_vline(data = dat[, .SD[date == min(date)], by = scenario][, .(scenario, date)][!(scenario %in% c("Observed", "No intervention"))],
               aes(xintercept = date, color = scenario),
               linetype = "dashed") +  
    
    # peak case count labels
  geom_label_repel(data = rbindlist(list(
    unique(dat[, .SD[fitted == max(fitted)], by = scenario][, .(scenario, date, fitted)][!(scenario %in% c("Observed", "No intervention"))][, fitted_val := fitted][]),
    data.table(scenario = "Observed", date = as.Date("2021-05-18"), fitted = 4529, fitted_val = dat[scenario == "Observed" & date == "2021-05-18", fitted])), fill = TRUE),
    aes(x = date, y = fitted_val, label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " deaths"), color = scenario, family = "Lato"),
    nudge_y = tmp_nudge,
    nudge_x = -10,
    size = 4,
    show.legend = FALSE,
    segment.size = 1) + 
  
  # date labels
  geom_text(data = dat[, .SD[date == min(date)], by = scenario][, .(scenario, date, fitted)][, `:=` (
    text = c("Observed data", "February 19\nTier II", "March 13\nTier III", "March 19\nTier IV", "March 30\nTier IV", "April 15\nTier IV"), 
    x    = as.Date(c("2021-04-22", "2021-02-09", "2021-03-03", "2021-03-11", "2021-03-23", "2021-04-09")), 
    y    = tmp_repel_y)][],
  aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Lato"),
  size = 4, hjust = c(0, 1, 1, 0, 0, 0), show.legend = FALSE) +
  
    # other stuff
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = title,
       y        = "Daily deaths",
       x        = "",
       color    = "Date of intervention") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
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
  }

deaths_p    <- death_plt(dat = tps_india, title = "Moderate CFR")
deaths_p_mh <- death_plt(dat = tps_mh, title = "High CFR")
deaths_p_kl <- death_plt(dat = tps_kl, title = "Low CFR")

patched <- deaths_p_mh / deaths_p / deaths_p_kl

full_plt <- patched +
  plot_annotation(
    title    = plt_title,
    subtitle = glue("{format(start_date, '%B %e, %Y')} to {format(end_date, '%B %e, %Y')}"),
    caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}. ",
                    "Figures in boxes show peak number of deaths for each intervention. Assumes starting R\u2080={r_0}.<br>",
                    "**Abbrev:** CFR, case-fatality rate<br>",
                    "**\uA9 COV-IND-19 Study Group**"),
    tag_levels = c("A")
  ) &
  theme(
    text              = element_text(family = "Lato"),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 12, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Lato", face = "bold")
  )

full_plt


# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = full_plt,
       height   = 12,
       width    = 15,
       units = "in", device = cairo_pdf)
