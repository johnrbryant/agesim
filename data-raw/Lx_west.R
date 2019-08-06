
library(methods)
library(dembase)
library(demogR)
library(dplyr)

age_breaks <- seq(0, 70, 5)

load("data-raw/Lx_west_tmp.rda")

Lx_west <- Lx_west_tmp %>%
    collapseIntervals(dimension = "age",
                      breaks = age_breaks) %>%
    as("array")

save(Lx_west,
     file = "data/Lx_west.rda")
