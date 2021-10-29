library(data.table)
library(covid19india)

national_counts <- get_nat_counts(mohfw = FALSE)
state_counts    <- get_state_counts(mohfw = FALSE)

national_tests  <- get_nat_tests()
state_tests     <- get_state_tests()

vaccine_data    <- get_state_vax(mohfw = FALSE)

all_data        <- get_all_data(mohfw = FALSE)

fwrite(
  x = national_counts,
  file = "data/covid19india_national_counts_20211029.csv"
)

fwrite(
  x = state_counts,
  file = "data/covid19india_state_counts_20211029.csv"
)

fwrite(
  x = national_tests,
  file = "data/covid19india_national_tests_20211029.csv"
)

fwrite(
  x = state_tests,
  file = "data/covid19india_state_tests_20211029.csv"
)

fwrite(
  x = vaccine_data,
  file = "data/covid19india_vaccine_data_20211029.csv"
)

fwrite(
  x = all_data,
  file = "data/covid19india_all_data_20211029.csv"
)
