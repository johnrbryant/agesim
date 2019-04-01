
library(methods)
library(dembase)
library(demogR)
library(dplyr)

Lx_west_female <- cdmltw(sex = "F")$nLx
Lx_west_male <- cdmltw(sex = "M")$nLx

Lx_west_female <- Lx_west_female %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

Lx_west_male <- Lx_west_male %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

Lx_west_tmp <- bind_rows(Lx_west_female, Lx_west_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level)) %>%
    dtabs(Freq ~ age + sex + level) %>%
    Counts()

save(Lx_west_tmp,
     file = "data-raw/Lx_west_tmp.rda")
