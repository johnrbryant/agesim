
library(dembase)
library(demogR)
library(dplyr)

age_breaks <- seq(0, 70, 5)

Lx_south_female <- cdmlts(sex = "F")$nLx
Lx_south_male <- cdmlts(sex = "M")$nLx

Lx_south_female <- Lx_south_female %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

Lx_south_male <- Lx_south_male %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

Lx_south <- bind_rows(Lx_south_female, Lx_south_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level)) %>%
    dtabs(Freq ~ age + sex + level) %>%
    Counts() %>%
    collapseIntervals(dimension = "age",
                      breaks = age_breaks) %>%
    as("array")

save(Lx_south,
     file = "data/Lx_south.rda")
