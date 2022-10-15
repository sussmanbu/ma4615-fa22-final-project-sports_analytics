library(tidyverse)

MLB_batting_raw_data <- read_csv(here::here("dataset",
                                            "MLB_batting_raw_data.csv"))

## CLEAN the data
MLB_bat_data_2021 <- MLB_batting_raw_data %>% filter(!is.na(pa))

write_csv(MLB_bat_data_2021, file = here::here("dataset",
                                               "MLB_bat_data_2021.csv"))

save(MLB_bat_data_2021, file = here::here("dataset/MLB_bat_data_2021.RData"))
