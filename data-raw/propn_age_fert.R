
library(methods)
library(dplyr)


## Yx from Booth, 1984, "Transforming Gompertz's Function
## for Fertility Analysis", Table 2
Yx <- c(-Inf, -1.77306, -0.69130, 0.02564, 0.70000, 1.47872, 2.62602, 4.80970, Inf)

values <- diff(exp(-exp(-Yx)))

labels <- paste(seq(10, 45, 5), seq(14, 49, 5), sep = "-")

propn_age_fert <- array(values,
                        dim = length(labels),
                        dimnames = list(age = labels))

save(propn_age_fert,
     file = "data/propn_age_fert.rda")
