
check_logical_flag <- function(value, name) {
    if (!is.logical(value))
        stop(gettextf("'%s' is does not have type \"%s\"",
                      name, "logical"))
    if (!identical(length(value), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (is.na(value))
        stop(gettextf("'%s' is missing",
                      name))
    NULL
}
    

check_numeric <- function(value, name) {
    if (!is.numeric(value))
        stop(gettextf("'%s' is non-numeric",
                      name))
    if (!identical(length(value), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (is.na(value))
        stop(gettextf("'%s' is missing",
                      name))
    NULL
}

check_nonnegative_numeric <- function(value, name) {
    check_numeric(value = value,
                  name = name)
    if (value < 0)
        stop(gettextf("'%s' less than 0",
                      name))
    NULL
}

check_positive_numeric <- function(value, name) {
    check_numeric(value = value,
                  name = name)
    if (value <= 0)
        stop(gettextf("'%s' is non-positive",
                      name))
    NULL
}

check_propn_age_fert <- function(propn_age_fert) {
    if (!methods::is(propn_age_fert, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "propn_age_fert", class(propn_age_fert)))
    if (!identical(length(dim(propn_age_fert)), 1L))
        stop(gettextf("'%s' does not have %d dimension",
                      "propn_age_fert", 1L))
    if (any(is.na(propn_age_fert)))
        stop(gettextf("'%s' has missing values",
                      "propn_age_fert"))
    if (any(propn_age_fert < 0))
        stop(gettextf("'%s' has negative values",
                      "propn_age_fert"))
    dimtypes <- dembase::dimtypes(propn_age_fert, use.names = FALSE)
    if (!identical(dimtypes, "age"))
        stop(gettextf("'%s' does not have dimension with %s \"%s\"",
                      "propn_age_fert", "dimtype", "age"))
    NULL
}


check_agesex <- function(value, name) {
    if (!identical(length(dim(value)), 2L))
        stop(gettextf("'%s' does not have %d dimensions",
                      name, 2L))
    if (any(is.na(value)))
        stop(gettextf("'%s' has missing values",
                      name))
    if (any(value < 0))
        stop(gettextf("'%s' has negative values",
                      name))
    dimtypes <- dembase::dimtypes(value, use.names = FALSE)
    if (!setequal(dimtypes, c("age", "sex")))
        stop(gettextf("'%s' does not have dimensions with %s \"%s\" and \"%s\"",
                      name, "dimtype", "age", "sex"))
    NULL
}

check_agesex_Count <- function(value, name) {
    if (!methods::is(value, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(value)))
    check_agesex(value = value,
                 name = name)
    NULL
}

check_agesex_Value <- function(value, name) {
    if (!methods::is(value, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(value)))
    check_agesex(value = value,
                 name = name)
    NULL
}

check_whole_number <- function(value, name) {
    check_numeric(value = value,
                  name = name)
    if (!all.equal(as.integer(value), value))
        stop(gettextf("'%s' has a non-integer value",
                      name))
    NULL
}


