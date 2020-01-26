    
library(methods)
library(dembase)
library(demogR)
library(dplyr)
library(tidyr)

## The calculations assume the open
## age group starts at 70.

labels_04 <- c("0", "1-4")
labels_0569 <- paste(seq(5, 65, 5), seq(9, 69, 5), sep = "-")
labels_70plus <- c(paste(seq(70, 90, 5), seq(74, 94, 5), sep = "-"),
                   "95+")
labels_cleaned <- c(paste(seq(0, 65, 5), seq(4, 69, 5), sep = "-"),
                    "70+")

mx_west_female <- (cdmltw(sex = "F")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

mx_west_male <- (cdmltw(sex = "M")$nmx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

mx_west <- bind_rows(mx_west_female, mx_west_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level))

Lx_west_female <- (cdmltw(sex = "F")$nLx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Female")

Lx_west_male <- (cdmltw(sex = "M")$nLx) %>%
    as.data.frame.table(stringsAsFactors = FALSE) %>%
    mutate(sex = "Male")

Lx_west <- bind_rows(Lx_west_female, Lx_west_male) %>%
    rename(level = Var1, age = Var2) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level))

Lx_west_04 <- Lx_west %>%
    filter(age %in% labels_04) %>%
    mutate(age = sub("-",  "", age),
           age = paste("Lx", age, sep = ".")) %>%
    spread(key = age, value = Freq) %>%
    mutate(level = as.integer(level))

mx_west_04 <- mx_west %>%
    filter(age %in% labels_04) %>%
    mutate(age = sub("-", "", age),
           age = paste("mx", age, sep = ".")) %>%
    spread(key = age, value = Freq) %>%
    left_join(Lx_west_04, by = c("level", "sex")) %>%
    mutate(Lower = ((4.5 * Lx.0 * mx.0 + 2 * Lx.14 * mx.14)
        / (4.5 * Lx.0 + 2 * Lx.14)),
           Upper = ((0.5 * Lx.0 * mx.0 + 3 * Lx.14 * mx.14)
        / (0.5 * Lx.0 + 3 * Lx.14))) %>%
    select(-Lx.0, -mx.0, -Lx.14, -mx.14) %>%
    gather(key = triangle, value = value, Lower, Upper) %>%
    mutate(age = "0-4")

mx_west_0569 <- mx_west %>%
    filter(age %in% labels_0569) %>%
    mutate(Lower = Freq,
           Upper = Freq) %>%
    gather(key = triangle, value = Freq, Lower, Upper) %>%
    rename(value = Freq)

Lx_west <- Lx_west %>%
    dtabs(Freq ~ age + sex + level) %>%
    Counts()

mx_west_70plus <- mx_west %>%
    filter(age %in% labels_70plus) %>%
    mutate(age = cleanAgeGroup(age)) %>%
    mutate(level = as.integer(level)) %>%
    dtabs(Freq ~ age + sex + level) %>%
    Values() %>%
    collapseIntervals(dimension = "age", breaks = 70, weights = Lx_west) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    mutate(level = as.integer(level)) %>%
    mutate(Lower = value,
           Upper = value) %>%
    gather(key = triangle, value = value, Lower, Upper)

mx_west <- bind_rows(mx_west_04, mx_west_0569, mx_west_70plus) %>%
    mutate(age = factor(age, levels = labels_cleaned)) %>%
    dtabs(value ~ age + sex + triangle + level)

save(mx_west,
     file = "data/mx_west.rda")
