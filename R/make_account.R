
#' Make an initial demographic account
#'
#' Make the initial values for a demographic account
#' consisting of population, deaths, and births
#'
#' The account will not typically be consistent -
#' that is, the demographic accounting equations will
#' not typically be satisfied. The account can be
#' made consistent using function \code{\link[dembase]{makeConsistent}}.
#'
#' If \code{Lx} and \code{propn_age_fert} use 5-year time
#' steps, then \code{time_end - time_start} should be a
#' multiple of 5.
#'
#' @inheritParams make_system_models
#' @param time_start The first year for the account.
#' @param time_end The last year for the account.
#'
#' @return A
#' \code{\link[dembase:DemographicAccount-class]{DemographicAccount}}.
#'
#' @examples
#' Lx <- dembase::Values(Lx_west[ , , 20])
#' mort_rates <- dembase::Values(mx_west[ , , 20])
#' propn_age_fert <- dembase::Values(propn_age_fert_booth)
#' expected_popn <- make_expected_popn(popn_size = 100,
#'                                     Lx = Lx,
#'                                     sex_ratio = 105)
#' fert_rates <- make_stationary_fert_rates(Lx = Lx,
#'                                propn_age_fert = propn_age_fert,
#'                                sex_ratio = 105)
#' account <- make_account(expected_popn = expected_popn,
#'                         mort_rates = mort_rates,
#'                         fert_rates = fert_rates,
#'                         time_start = 1000,
#'                         time_end = 1020)
#' dembase::summary(account)
#' @export
make_account <- function(expected_popn, mort_rates, fert_rates,
                         time_start, time_end) {
    check_val_agesex(value = expected_popn,
                     name = "expected_popn")
    check_val_agesex(value = mort_rates,
                     name = "mort_rates")
    check_val_agesex(value = fert_rates,
                     name = "fert_rates")
    check_whole_number(value = time_start,
                       name = "time_start")
    check_whole_number(value = time_end,
                       name = "time_end")
    if (time_end <= time_start)
        stop(gettextf("'%s' is less than or equal to '%s'",
                      "time_end", "time_start"))
    step <- dembase::ageTimeStep(expected_popn)
    dimvalues_time <- seq(from = time_start,
                          to = time_end,
                          by = step)
    DimScale_points <- methods::new("Points", dimvalues = dimvalues_time)
    DimScale_intervals <- methods::new("Intervals", dimvalues = dimvalues_time)
    labels_points <- dembase::labels(DimScale_points)
    labels_intervals <- dembase::labels(DimScale_intervals)
    expected_popn <- dembase::addDimension(expected_popn,
                                           name = "time",
                                           labels = labels_points,
                                           dimtype = "time",
                                           dimscale = "Points")
    mort_rates <- dembase::addDimension(mort_rates,
                                        name = "time",
                                        labels = labels_intervals,
                                        dimtype = "time",
                                        dimscale = "Intervals")
    fert_rates <- dembase::addDimension(fert_rates,
                                        name = "time",
                                        labels = labels_intervals,
                                        dimtype = "time",
                                        dimscale = "Intervals")
    population <- methods::as(expected_popn, "Counts")
    exposure <- dembase::exposure(population)
    exposure_births <- dembase::exposureBirths(population,
                                               births = fert_rates)
    deaths <- mort_rates * exposure
    births <- fert_rates * exposure_births
    population <- dembase::toInteger(population, force = TRUE)
    deaths <- dembase::toInteger(deaths, force = TRUE)
    births <- dembase::toInteger(births, force = TRUE)
    dembase::Movements(population = population,
                       births = births,
                       exits = list(deaths = deaths))
}
