# libraries ----------
librarian::shelf(
  tidyverse, chron, rjags, gtools, here, devtools, lilywang1988/eSIR, glue,
  zoo, cli
)

f <- list.files("~/projects/science_revision/src/")
for (i in seq_along(f)) {source(paste0("~/projects/science_revision/src/", f[i]))}

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

dat <- read_csv("~/projects/science_revision/data_for_lockdown_extended.csv", col_types = cols()) %>%
  filter(date <= "2021-07-31")

# directory ----------
setwd(data_repo)

# no intervention -----------
  cli::cli_alert_info("No intervention beginning March 19")
  last_obs   <- as.Date("2021-03-18")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 30
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
  
  casename   <- glue("{last_obs + 1}_no_intervention_r{R_0}")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir2(N = N, adj = T, adj_len = 2, name = "No intervention")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$data2, file = paste0("./", casename, "_data2.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
  cli::cli_alert_success("*beep boop* complete!!!")
