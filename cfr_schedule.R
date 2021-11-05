library(data.table)
library(janitor)
library(covid19india)

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

cfr_sched <- extract_cfr()[order(date), day := 1:.N][, .(day, cfr_mod = cfr_t7, cfr_high = cfr_mh_t7, cfr_low = cfr_kl_t7)][]

fwrite(cfr_sched, "~/Downloads/cfr_schedule.txt")
