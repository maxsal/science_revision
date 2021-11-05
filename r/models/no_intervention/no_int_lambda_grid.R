# libraries ----------
librarian::shelf(
  tidyverse, chron, rjags, gtools, here, devtools, lilywang1988/eSIR, glue,
  zoo, cli, VGAM
)

f <- list.files("~/projects/science_revision/src/")
for (i in seq_along(f)) {source(paste0("~/projects/science_revision/src/", f[i]))}

g <- list.files("~/projects/science_revision/esir_mod/")
for (i in seq_along(g)) {source(paste0("~/projects/science_revision/esir_mod/", g[i]))}

arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")

start_date <- as.Date(Sys.getenv("start_date"))
set.seed(20192020) # default: 20192020

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") { 
  data_repo <- "~/projects/science_revision/data/no_intervention/"
  Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
  data_repo <- "~/projects/science_revision/test/no_intervention/"
  Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

# specifications ----------
R_0                <- as.numeric(Sys.getenv("R_0"))     # basic reproduction number
save_files         <- TRUE
save_mcmc          <- TRUE
save_plot_data     <- TRUE

rayleigh           <- FALSE # not incorporating waning immunity
if (rayleigh == TRUE) { sigma0 = 310 }

### Option for using seroprevalence estimate, default is to use theta prior as before.
use_sero <- FALSE
# seroprev <- as.numeric(Sys.getenv("seroprev")) # January seroprevalence in India [Murhekar et al. 2021]

### Option for changing kappa and lambda prior precisions, all default to 0.0001.
# kappaprec = as.numeric(Sys.getenv("kappaprec"))
# lambdaYprec = 0.01
# lambdaRprec = 0.01

dat <- read_csv("~/projects/science_revision/data_for_lockdown_extended.csv", col_types = cols()) %>%
  filter(date <= "2021-07-31")

# directory ----------
setwd(data_repo)

# no intervention -----------
  cli::cli_alert_info("No intervention beginning {start_date}")
  last_obs   <- start_date - 1
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 150
  cli::cli_alert_info("Projecting through {last_proj}")
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  l_values <- expand.grid(lY = 10^c(-1, -2, -3), lR = 10^c(-1, -2, -3))
  
  
if (arrayid == 1) {
  
  arr_id <- 1
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
      Y,
      R,
      begin_str = format(start_obs, "%m/%d/%Y"),
      death_in_R = 0.2,
      T_fin = esir_days,
      R0       = R_0,
      rayleigh = rayleigh,
      # sigma0 = sigma0,
      use_sero = use_sero,
      # seroprev = seroprev,
      # kappaprec = kappaprec,
      lambdaYprec = l_values$lY[arr_id],
      lambdaRprec = l_values$lR[arr_id],
      dic = F,
      casename = casename,
      save_files = save_files,
      save_mcmc = save_mcmc,
      save_plot_data = save_plot_data,
      M = Ms,
      nburnin = nburnins
    )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}
  
if (arrayid == 2) {
  
  arr_id <- 2
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}  
  
if (arrayid == 3) {
  
  arr_id <- 3
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}

if (arrayid == 4) {
  
  arr_id <- 4
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}

if (arrayid == 5) {
  
  arr_id <- 5
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}

if (arrayid == 6) {
  
  arr_id <- 6
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}

if (arrayid == 7) {
  
  arr_id <- 7
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}

if (arrayid == 8) {
  
  arr_id <- 8
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}

if (arrayid == 9) {
  
  arr_id <- 9
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}_lY{l_values$lY[arr_id]}_lR{l_values$lR[arr_id]}_mod")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str = format(start_obs, "%m/%d/%Y"),
    death_in_R = 0.2,
    T_fin = esir_days,
    R0       = R_0,
    rayleigh = rayleigh,
    # sigma0 = sigma0,
    use_sero = use_sero,
    # seroprev = seroprev,
    # kappaprec = kappaprec,
    lambdaYprec = l_values$lY[arr_id],
    lambdaRprec = l_values$lR[arr_id],
    dic = F,
    casename = casename,
    save_files = save_files,
    save_mcmc = save_mcmc,
    save_plot_data = save_plot_data,
    M = Ms,
    nburnin = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  
  cli::cli_alert_info("Look for output under casename: {casename}")
  cli::cli_alert_success("*beep boop* complete!!!")
}
