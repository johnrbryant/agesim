
library(dplyr)
library(readr)
library(dembase)

propn_age_fert_maori <- read_csv("data-raw/DFM168104_20200824_044821_8.csv",
                                 skip = 3,
                                 col_names = c("age", "rate"),
                                 n_max = 8) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    filter(age != "10-14") %>%
    mutate(propn = rate / sum(rate)) %>%
    dtabs(propn ~ age)
    
save(propn_age_fert_maori,
     file = "data/propn_age_fert_maori.rda")
