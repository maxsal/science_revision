library(data.table)
library(janitor)

d       <- fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/covid19india_national_counts_20211031.csv", showProgress = FALSE)[, date := as.Date(date)][daily_deaths == 6139, daily_deaths := (d[date == "2021-06-08", daily_deaths] + d[date == "2021-06-10", daily_deaths]) / 2][]
d_state <- fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/covid19india_state_counts_20211031.csv", showProgress = FALSE)[, date := as.Date(date)]

extract_cfr <- function(end_date = "2021-05-15", day_lag = 14) {
  
  get_state_cfr <- function(abb) {
    
    tmp_taco <- d_state[place == fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/population.csv", showProgress = FALSE)[abbrev == abb, place]][
      , cfr := daily_deaths / shift(daily_cases, n = day_lag)
    ][
      date == "2021-07-20", cfr := NA
    ]
    
    if (nrow(tmp_taco[date == "2021-02-21"]) == 0) {
      tmp_taco <- rbindlist(list(
        tmp_taco,
        data.table(
          place = fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/population.csv", showProgress = FALSE)[abbrev == abb, place],
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

cfr_sched <- extract_cfr(end_date = "2021-07-31")[order(date), day := 1:.N][
  , `:=` (
    cfr_smooth = predict(loess(cfr ~ day, span = 0.3)),
    cfr_mh_smooth = predict(loess(cfr_mh ~ day, span = 0.3)),
    cfr_kl_smooth = predict(loess(cfr_kl ~ day, span = 0.3))
  )
][]

tmp_cfr <- fread("data/cfr_schedule_14day_lag.txt")

cfr_sched <- merge.data.table(cfr_sched, tmp_cfr[, .(date, tmp_old_mod = cfr_mod)], by = "date")

cfr_sched <- cfr_sched[, .(
  day, date,
  cfr_mod_daily = cfr, cfr_mod_t7 = cfr_t7, cfr_mod_smooth = cfr_smooth,
  cfr_high_daily = cfr_mh, cfr_high_t7 = cfr_mh_t7, cfr_high_smooth = cfr_mh_smooth,
  cfr_low_daily = cfr_kl, cfr_low_t7 = cfr_kl_t7, cfr_low_smooth = cfr_kl_smooth
)][]

cfr_sched <- extract_cfr(end_date = "2021-07-31")[order(date), day := 1:.N][, .(day, date, cfr_daily = cfr, cfr_mod = cfr_t7, cfr_high = cfr_mh_t7, cfr_low = cfr_kl_t7)][]

colors <- c(
  "new_mod" = "#e0101a",
  "old_mod" = "#e0101a",
  "new_high" = "#9900d1",
  "old_high" = "#9900d1",
  "new_low" = "#e6811c",
  "old_low" = "#e6811c"
)

melt(cfr_sched[, .(date, new_mod = cfr_mod_smooth, new_high = cfr_high_smooth, new_low = cfr_low_smooth,
        old_mod = tmp_old_mod, old_high = cfr_high_t7, old_low = cfr_low_t7)],
     id.vars = "date")[, `:=` (lt = "solid", sz = 1)][variable %in% c("old_mod", "old_high", "old_low"), `:=` (lt = "dashed", sz = 0.5)] |>
  ggplot(aes(x = date, y = value, color = variable, linetype = lt)) +
  geom_line(aes(size = sz)) +
  scale_color_manual(values = colors) +
  scale_linetype_identity() +
  scale_size_identity() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "New and old CFR schedules",
    x = "Date",
    y = "Case-fatality rate",
    caption = "New CFR schedules are represents by solid lines; old CFR schedules are represented by dashed lines.<br>New schedules are LOESS-smoothed daily CFRs with span = 0.3; old schedules were trailing 7-day averages."
  )


cfr_sched <- extract_cfr(end_date = "2021-07-31")[order(date), day := 1:.N][
  , `:=` (
    cfr_smooth = predict(loess(cfr ~ day, span = 0.3)),
    cfr_mh_smooth = predict(loess(cfr_mh ~ day, span = 0.3)),
    cfr_kl_smooth = predict(loess(cfr_kl ~ day, span = 0.3))
  )
][]

cfr_sched <- cfr_sched[, .(
  day, date,
  cfr_mod_daily = cfr, cfr_mod_t7 = cfr_t7, cfr_mod_smooth = cfr_smooth,
  cfr_high_daily = cfr_mh, cfr_high_t7 = cfr_mh_t7, cfr_high_smooth = cfr_mh_smooth,
  cfr_low_daily = cfr_kl, cfr_low_t7 = cfr_kl_t7, cfr_low_smooth = cfr_kl_smooth
)][]

fwrite(cfr_sched, "~/Downloads/cfr_schedule_14day_lag.txt")
