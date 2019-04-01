
## Turn a plain Lx array into a Values object

make_propn_sex <- function(sex_ratio) {
    check_sex_ratio(sex_ratio)
    pr_male <- sex_ratio / (100 + sex_ratio)
    values <- c(1 - pr_male, pr_male)
    labels <- c("Female", "Male")
    dembase::ValuesOne(values,
                       labels = labels,
                       name = "sex")
}





