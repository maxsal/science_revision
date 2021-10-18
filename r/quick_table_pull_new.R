pacman::p_load(tidyverse, glue, here, janitor, zoo)

source(here("r", "period_summary_new.R"))

cfr_sched <- "kl"
mh        <- FALSE
wane      <- FALSE

cfr_type  <- ifelse(cfr_sched == "india", "mod", ifelse(cfr_sched == "mh", "high", ifelse(cfr_sched == "kl", "low", NA)))
le_type   <- ifelse(mh == TRUE, "mod", ifelse(mh == FALSE, "strong", NA))
wane_type <- ifelse(wane == TRUE, "wane", "main")

feb_19_mar_30 <- period_summary(base_path = "early_intervention", scen = "2021-02-19_20pct_smooth1", end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_apr_15 <- period_summary(base_path = "early_intervention", scen = "2021-02-19_20pct_smooth1", end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_apr_30 <- period_summary(base_path = "early_intervention", scen = "2021-02-19_20pct_smooth1", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_may_15 <- period_summary(base_path = "early_intervention", scen = "2021-02-19_20pct_smooth1", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_may_30 <- period_summary(base_path = "early_intervention", scen = "2021-02-19_20pct_smooth1", end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_jun_15 <- period_summary(base_path = "early_intervention", scen = "2021-02-19_20pct_smooth1", end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

mar_13_mar_30 <- period_summary(scen = "2021-03-13_t3", end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_apr_15 <- period_summary(scen = "2021-03-13_t3", end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_apr_30 <- period_summary(scen = "2021-03-13_t3", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_may_15 <- period_summary(scen = "2021-03-13_t3", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_may_30 <- period_summary(scen = "2021-03-13_t3", end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_jun_15 <- period_summary(scen = "2021-03-13_t3", end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

mar_19_mar_30 <- period_summary(scen = "2021-03-19_t4", end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_apr_15 <- period_summary(scen = "2021-03-19_t4", end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_apr_30 <- period_summary(scen = "2021-03-19_t4", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_may_15 <- period_summary(scen = "2021-03-19_t4", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_may_30 <- period_summary(scen = "2021-03-19_t4", end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_jun_15 <- period_summary(scen = "2021-03-19_t4", end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

print_cases <- function(x) {
  
  a <- x$observed_period_cases
  b <- x$predicted_period_cases
  d <- x$pred_cases_averted
  f <- x$pct_cases_averted
  
  glue("
       {format(a, nsmall = 1)}
       {format(b[1], nsmall = 1)} [{format(b[2], nsmall = 1)}, {format(b[3], nsmall = 1)}]
       {format(d[1], nsmall = 1)} [{format(d[2], nsmall = 1)}, {format(d[3], nsmall = 1)}]
       {format(f[1], nsmall = 1)}% [{format(f[2], nsmall = 1)}%, {format(f[3], nsmall = 1)}%]
       ")
  
}

cases_out <- tibble(
  "Tier II" = c(print_cases(feb_19_mar_30), print_cases(feb_19_apr_15), print_cases(feb_19_apr_30), print_cases(feb_19_may_15), print_cases(feb_19_may_30), print_cases(feb_19_jun_15)),
  "Tier III" = c(print_cases(mar_13_mar_30), print_cases(mar_13_apr_15), print_cases(mar_13_apr_30), print_cases(mar_13_may_15), print_cases(mar_13_may_30), print_cases(mar_13_jun_15)),
  "Tier IV" = c(print_cases(mar_19_mar_30), print_cases(mar_19_apr_15), print_cases(mar_19_apr_30), print_cases(mar_19_may_15), print_cases(mar_19_may_30), print_cases(mar_19_jun_15))
)
write_csv(cases_out, glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/tables/tiers_{cfr_type}cfr_{wane_type}_cases.csv"))
# write_csv(cases_out, here("data", "early_lockdown", "tables", glue("tiers_{cfr_type}cfr_{wane_type}_cases.csv")))

print_deaths <- function(x) {
  
  a <- x$observed_period_deaths
  b <- x$predicted_period_deaths
  d <- x$pred_deaths_averted
  f <- x$pct_deaths_averted
  
  glue("
       {format(a, nsmall = 1)}
       {format(b[1], nsmall = 1)} [{format(b[2], nsmall = 1)}, {format(b[3], nsmall = 1)}]
       {format(d[1], nsmall = 1)} [{format(d[2], nsmall = 1)}, {format(d[3], nsmall = 1)}]
       {format(f[1], nsmall = 1)}% [{format(f[2], nsmall = 1)}%, {format(f[3], nsmall = 1)}%]
       ")
  
}

deaths_out <- tibble(
  "Tier II" = c(print_deaths(feb_19_mar_30), print_deaths(feb_19_apr_15), print_deaths(feb_19_apr_30), print_deaths(feb_19_may_15), print_deaths(feb_19_may_30), print_deaths(feb_19_jun_15)),
  "Tier III" = c(print_deaths(mar_13_mar_30), print_deaths(mar_13_apr_15), print_deaths(mar_13_apr_30), print_deaths(mar_13_may_15), print_deaths(mar_13_may_30), print_deaths(mar_13_jun_15)),
  "Tier IV" = c(print_deaths(mar_19_mar_30), print_deaths(mar_19_apr_15), print_deaths(mar_19_apr_30), print_deaths(mar_19_may_15), print_deaths(mar_19_may_30), print_deaths(mar_19_jun_15))
)
write_csv(deaths_out, glue("/Volumes/tiny/projects/covid/science_revision/data/early_lockdown/tables/tiers_{cfr_type}cfr_{wane_type}_deaths.csv"))
# write_csv(deaths_out, here("data", "early_lockdown", "tables", glue("tiers_{cfr_type}cfr_{wane_type}_deaths.csv")))
