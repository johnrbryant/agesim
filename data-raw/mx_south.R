    
library(dembase)
library(demogR)
library(dplyr)
library(tidyr)

## The calculations assume the open
## age group starts at 70.

labels_04 <- c("0", "1-4")
labels_0569 <- paste(seq(5, 65, 5), seq(9, 70, 5), sep = "-")
labels_70plus <- c(paste(seq(70, 90, 5), seq(74, 94, 5), sep = "-"),
                   "95+")
labels_cleaned <- c(paste(seq(0, 65, 5), seq(4, 69, 5), sep = "-"),
                    "70+")

mx_south_female <- (cdmlts(sex = "F")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

mx_south_male <- (cdmlts(sex = "M")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

mx_south <- bind_rows(mx_south_female, mx_south_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level))

Lx_south_female <- (cdmlts(sex = "F")$nLx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

Lx_south_male <- (cdmlts(sex = "M")$nLx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

Lx_south <- bind_rows(Lx_south_female, Lx_south_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level))

Lx_south_04 <- Lx_south %>%
    filter(age %in% labels_04) %>%
    mutate(age = sub("-",  "", age),
           age = paste("Lx", age, sep = ".")) %>%
    spread(key = age, value = Freq) %>%
    mutate(level = as.integer(level))

mx_south_04 <- mx_south %>%
    filter(age %in% labels_04) %>%
    mutate(age = sub("-", "", age),
           age = paste("mx", age, sep = ".")) %>%
    spread(key = age, value = Freq) %>%
    left_join(Lx_south_04, by = c("level", "sex")) %>%
    mutate(Lower = ((4.5 * Lx.0 * mx.0 + 2 * Lx.14 * mx.14)
        / (4.5 * Lx.0 + 2 * Lx.14)),
           Upper = ((0.5 * Lx.0 * mx.0 + 3 * Lx.14 * mx.14)
        / (0.5 * Lx.0 + 3 * Lx.14))) %>%
    select(-Lx.0, -mx.0, -Lx.14, -mx.14) %>%
    gather(key = triangle, value = value, Lower, Upper) %>%
    mutate(age = "0-4")

mx_south_0569 <- mx_south %>%
    filter(age %in% labels_0569) %>%
    rename(Lower = Freq) %>%
    mutate(Upper = Lower) %>%
    gather(key = triangle, value = value, Lower, Upper)

Lx_south <- Lx_south %>%
    dtabs(Freq ~ age + sex + level) %>%
    Counts()

mx_south_70plus <- mx_south %>%
    filter(age %in% labels_70plus) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level)) %>%
    dtabs(Freq ~ age + sex + level) %>%
    Values() %>%
    collapseIntervals(dimension = "age", breaks = 70, weights = Lx_south) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate(level = as.integer(level)) %>%
    rename(Lower = value) %>%
    mutate(Upper = Lower) %>%
    gather(key = triangle, value = value, Lower, Upper)

mx_south <- bind_rows(mx_south_04, mx_south_0569, mx_south_70plus) %>%
    mutate(age = factor(age, levels = labels_cleaned)) %>%
    dtabs(value ~ age + sex + triangle + level)

save(mx_south,
     file = "data/mx_south.rda")
