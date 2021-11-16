# libraries ----------
ally::libri(tidyverse, chron, rjags, gtools, here, devtools,
  lilywang1988/eSIR, glue, zoo, data.table)

source("esir_ally.R")

today   <- Sys.Date() - 1
arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") { 
  data_repo <- "~/projects/science_revision/data/early_lockdown/"
  Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
  data_repo <- "~/projects/science_revision/test/early_lockdown/"
  Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

# specifications ----------
R_0            <- as.numeric(Sys.getenv("R_0")) # basic reproduction number
save_files     <- FALSE
save_mcmc      <- TRUE
save_plot_data <- TRUE

dat <- read_csv("~/projects/science_revision/data_for_lockdown_extended.csv", col_types = cols()) %>%
  filter(date <= "2021-07-31")

pi_sched  <- read_tsv("~/projects/science_revision/pi_schedule_extended.txt", col_types = cols())

# directory ----------
setwd(data_repo)

# models ---------
# Tier II interventions -----------
if (arrayid == 1) {
  
  message("Tier II on Tier II start date: February 19, 2021")
  last_obs   <- as.Date("2021-02-18")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_pis <- pi_sched %>%
    select(-c(r_est)) %>%
    dplyr::filter(place == "MH Pre-lock +20%") %>%
    arrange(date) %>%
    pull(smooth_pis) %>%
    c(1, .)%>%
    head(., -1)
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_t2_r{R_0}")
  
  feb19_20pct_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- feb19_20pct_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "MH Pre-lockdown + 20%")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}

# Tier III interventions -----------
if (arrayid == 2) {
  
  message("Tier III on Tier III start date: March 13, 2021")
  last_obs   <- as.Date("2021-03-12")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_pis <- pi_sched %>%
    select(-c(r_est)) %>%
    dplyr::filter(place == "Maharashtra early") %>%
    arrange(date) %>%
    pull(smooth_pis) %>%
    c(1, .)%>%
    head(., -1)
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_t3_r{R_0}")
  
  march13_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- march13_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "March 13")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}

# Tier IV interventions ----------
if (arrayid == 3) {
  
  message("Tier IV on Tier IV start date: March 19, 2021")
  last_obs   <- as.Date("2021-03-18")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_pis <- pi_sched %>%
    select(-c(r_est)) %>%
    dplyr::filter(place == "Maharashtra") %>%
    arrange(date) %>%
    pull(smooth_pis) %>%
    c(1, .)%>%
    head(., -1)
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_t4_r{R_0}")
  
  march19_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- march19_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "March 19")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}

if (arrayid == 4) {
  
  message("Tier IV on Tier IV start date: March 30, 2021")
  last_obs   <- as.Date("2021-03-29")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_pis <- pi_sched %>%
    select(-c(r_est)) %>%
    dplyr::filter(place == "Maharashtra") %>%
    arrange(date) %>%
    pull(smooth_pis) %>%
    c(1, .)%>%
    head(., -1)
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_t4_r{R_0}")
  
  march19_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- march19_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "March 19")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}

if (arrayid == 5) {
  
  message("Tier IV on Tier IV start date:April 15, 2021")
  last_obs   <- as.Date("2021-04-14")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_pis <- pi_sched %>%
    select(-c(r_est)) %>%
    dplyr::filter(place == "Maharashtra") %>%
    arrange(date) %>%
    pull(smooth_pis) %>%
    c(1, .)%>%
    head(., -1)
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_t4_r{R_0}")
  
  march19_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- march19_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "March 19")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}
