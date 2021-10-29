library(data.table)
library(janitor)


d       <- covid19india::get_nat_counts(mohfw = FALSE)
d_state <- covid19india::get_state_counts(mohfw = FALSE)

extract_cfr <- function(end_date = "2021-05-15", day_lag = 14) {
  
  get_state_cfr <- function(abb) {
    
    tmp_taco <- d_state[place == covid19india::pop[abbrev == abb, place]][
      , cfr := daily_deaths / shift(daily_cases, n = day_lag)
    ][
      date == "2021-07-20", cfr := NA
    ]
    
    if (nrow(tmp_taco[date == "2021-02-21"]) == 0) {
      tmp_taco <- rbindlist(list(
        tmp_taco,
        data.table(
          place = covid19india::pop[abbrev == abb, place],
          date  = as.Date("2021-02-21")
        )
      ), fill = TRUE)[order(place, date)]
    }
    
    tmp_taco <- tmp_taco[
      , cfr := nafill(cfr, type = "locf")
    ][
      , cfr_t7 := frollmean(cfr, n = 7)
    ][, .(date, cfr, cfr_t7)][]
    
    setnames(tmp_taco,
             old = c("cfr", "cfr_t7"),
             new = c(paste0("cfr_", abb), paste0("cfr_", abb, "_t7"))
    )
    
    return(tmp_taco)
    
  }
  
  taco_kl <- get_state_cfr(abb = "kl")
  taco_mh <- get_state_cfr(abb = "mh")
  
  d <- data.table::merge.data.table(d, taco_kl, by = "date")
  d <- data.table::merge.data.table(d, taco_mh, by = "date")
  
  d <- d[, cfr := daily_deaths / shift(daily_cases, n = day_lag)][date == "2021-07-20", cfr := NA][is.infinite(cfr), cfr := NA]
  
  if (nrow(d[place == "India" & date == "2021-02-21"]) == 0) {
    d <- rbindlist(list(
      d,
      data.table(
        place = "India",
        date  = as.Date("2021-02-21")
      )
    ), fill = TRUE)[order(place, date)]
  }
  
  d <- d[
    , cfr := nafill(cfr, type = "locf")
  ][, cfr_t7 := frollmean(cfr, n = 7)][between(date, as.Date("2021-02-15"), as.Date(end_date))]
  
  return(d)
  
}


# extract plot defaults -----------
get_plt_def <- function(mh, kl) {
  
  if (mh == TRUE & kl == TRUE) {
    # cfr <- cfr %>% filter(place == "Maharashtra")
    tmp_title    <- "Predicted number of daily COVID-19 deaths using Maharashtra lockdown schedule"
    tmp_subtitle <- "Using Kerala CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(250, 4000, 4000, 4000)
    tmp_nudge    <- 500
    tmp_filename <- "deaths_kl_mh.pdf"
  } else if (mh == TRUE & kl == FALSE) {
    tmp_title    <- "Predicted number of daily COVID-19 deaths using Maharashtra lockdown schedule"
    tmp_subtitle <- "Using Maharashtra CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(400, 9000, 9000, 9000)
    tmp_nudge    <- 1000
    tmp_filename <- "deaths_mh.pdf"
  } else if (kl == TRUE) {
    tmp_title    <- "Predicted number of daily COVID-19 deaths using India lockdown schedule"
    tmp_subtitle <- "Using Kerala CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(250, 4000, 4000, 4000)
    tmp_nudge    <- 500
    tmp_filename <- "deaths_kl.pdf"
  } else {
    # cfr <- cfr %>% filter(place == "India")
    tmp_title    <- "Predicted number of daily COVID-19 deaths using India lockdown schedule"
    tmp_subtitle <- "Using India CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(250, 4000, 4000, 4000)
    tmp_nudge    <- 500
    tmp_filename <- "deaths.pdf"
  }
  
  list(
    "tmp_title"    = tmp_title,
    "tmp_subtitle" = tmp_subtitle,
    "tmp_repel_y"  = tmp_repel_y,
    "tmp_nudge"    = tmp_nudge,
    "tmp_filename" = tmp_filename
  )
  
}

# clean scenario data -----------
clean_scenario <- function(dat, p, stop_obs, scen, end_date = "2021-05-15") {
  dat %>% 
    filter(date <= stop_obs) %>% 
    select(date, daily_cases) %>% 
    rename(incidence = daily_cases) %>% 
    add_row(p %>%  
              filter(scenario == scen) %>% 
              select(date, incidence) %>% 
              drop_na()) %>% 
    add_column(scenario = scen) %>%
    filter(date <= end_date)
}

# clean scenario data for cfr -----------
clean_scenario_cfr <- function(dat, p, stop_obs, scen, use_cfr) {
  
  dat %>% 
    filter(date <= stop_obs) %>% 
    select(date, daily_deaths) %>% 
    rename(incidence = daily_deaths) %>% 
    add_row(p %>%  
              filter(scenario == scen) %>% 
              select(date, incidence) %>% 
              drop_na() %>% 
              left_join(d %>% select(date, cfr = {{ use_cfr }}), by = "date") %>% 
              mutate(incidence = incidence*cfr) %>% 
              select(-cfr)) %>% 
    add_column(scenario = scen) 
  
}