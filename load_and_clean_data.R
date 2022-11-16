library(tidyverse)
library(here)
data <-read_csv(here::here("dataset-ignore/raw_nba_data.csv"),show_col_types = FALSE)
data <-data %>%  filter(!row_number() %in% c(110510	))

save(data, file = here::here("dataset/clean_nba_data.RData"))
load(here::here("dataset-ignore", "clean_nba_data.RData"))
