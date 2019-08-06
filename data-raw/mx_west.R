    
library(methods)
library(dembase)
library(demogR)
library(dplyr)

age_breaks <- seq(0, 70, 5)

mx_west_female <- cdmltw(sex = "F")$nmx
mx_west_male <- cdmltw(sex = "M")$nmx

load("data-raw/Lx_west_tmp.rda")

mx_west_female <- mx_west_female %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

mx_west_male <- mx_west_male %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

mx_west <- bind_rows(mx_west_female, mx_west_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level)) %>%
    dtabs(Freq ~ age + sex + level) %>%
    Values() %>%
    collapseIntervals(dimension = "age",
                      breaks = age_breaks,
                      weights = Lx_west_tmp) %>%
    as("array")

save(mx_west,
     file = "data/mx_west.rda")
