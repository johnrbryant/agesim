
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
#' If \code{popn}, \code{mort_rates}, or \code{fert_rates}
#' do not have a time dimension, \code{make_account} adds one.
#'
#' @inheritParams make_system_models
#' @param popn A \code{\link[dembase:DemographicAccount-class]{Counts}}
#' object.
#' @param time_start The first year for the account.
#' @param time_end The last year for the account.
#'
#' @return A
#' \code{\link[dembase:DemographicAccount-class]{DemographicAccount}}.
#'
#' @examples
#' Lx <- dembase::Counts(Lx_west[ , , 20])
#' mort_rates <- dembase::Values(mx_west[ , , 20])
#' propn_age_fert <- dembase::Values(propn_age_fert_booth)
#' popn <- make_stationary_popn(popn_size = 100,
#'                              Lx = Lx,
#'                              sex_ratio = 105,
#'                              time_start = 1000,
#'                              time_end = 1020)
#' fert_rates <- make_stationary_fert_rates(Lx = Lx,
#'                                propn_age_fert = propn_age_fert,
#'                                sex_ratio = 105)
#' account <- make_account(popn = popn,
#'                         mort_rates = mort_rates,
#'                         fert_rates = fert_rates,
#'                         time_start = 1000,
#'                         time_end = 1020)
#' dembase::summary(account)
#' @export
make_account <- function(popn, mort_rates, fert_rates,
                         time_start, time_end) {
    check_agesextime_Count(value = popn,
                           name = "popn")
    check_agesextime_Value(value = mort_rates,
                           name = "mort_rates")
    check_agesextime_Value(value = fert_rates,
                           name = "fert_rates")
    check_whole_number(value = time_start,
                       name = "time_start")
    check_whole_number(value = time_end,
                       name = "time_end")
    if (time_end <= time_start)
        stop(gettextf("'%s' is less than or equal to '%s'",
                      "time_end", "time_start"))
    step <- dembase::ageTimeStep(popn)
    dimvalues_time <- seq(from = time_start,
                          to = time_end,
                          by = step)
    DimScale_points <- methods::new("Points", dimvalues = dimvalues_time)
    DimScale_intervals <- methods::new("Intervals", dimvalues = dimvalues_time)
    labels_points <- dembase::labels(DimScale_points)
    labels_intervals <- dembase::labels(DimScale_intervals)
    if (!("time" %in% dimtypes(popn)))
        popn <- dembase::addDimension(popn,
                                      name = "time",
                                      labels = labels_points,
                                      dimtype = "time",
                                      dimscale = "Points")
    if (!("time" %in% dimtypes(mort_rates)))        
        mort_rates <- dembase::addDimension(mort_rates,
                                            name = "time",
                                            labels = labels_intervals,
                                            dimtype = "time",
                                            dimscale = "Intervals")
    if (!("time" %in% dimtypes(fert_rates)))        
        fert_rates <- dembase::addDimension(fert_rates,
                                            name = "time",
                                            labels = labels_intervals,
                                            dimtype = "time",
                                            dimscale = "Intervals")
    exposure <- dembase::exposure(popn)
    exposure_births <- dembase::exposureBirths(popn,
                                               births = fert_rates)
    expected_deaths <- mort_rates * exposure
    expected_births <- fert_rates * exposure_births
    popn <- round(popn)
    deaths <- stats::rpois(n = length(expected_deaths),
                           lambda = expected_deaths)
    births <- stats::rpois(n = length(expected_births),
                           lambda = expected_births)
    deaths <- array(deaths,
                    dim = dim(exposure),
                    dimnames = dimnames(exposure))
    births <- array(births,
                    dim = dim(exposure_births),
                    dimnames = dimnames(exposure_births))
    deaths <- methods::new("Counts",
                           .Data = deaths,
                           metadata = exposure@metadata)
    births <- methods::new("Counts",
                           .Data = births,
                           metadata = exposure_births@metadata)
    ans <- dembase::Movements(population = popn,
                              births = births,
                              exits = list(deaths = deaths))
    ans <- dembase::makeConsistent(ans)
    ans
}
