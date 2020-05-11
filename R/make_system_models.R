
#' Make system models for a demographic account
#'
#' Make system models for population, deaths, and births,
#' for a demographic account
#'
#' @param expected_popn A \code{\link[dembase:Counts-class]{Counts}}
#' array giving expected population counts by age and sex. Typically
#' created using functions \code{\link{make_stationary_popn}},
#' or \code{\link{make_stable_popn}}.
#' @param mort_rates A \code{\link[dembase:Values-class]{Values}}
#' array giving mortality rates by age and sex.
#' @param fert_rates A \code{\link[dembase:Values-class]{Values}}
#' array giving mean fertility rates by age and sex. Typically
#' generated using function \code{\link{make_stationary_fert_rates}}.
#' @param time_trend_popn Whether to use random walk model
#' for the time effect in population. Defaults to
#' \code{TRUE}.
#' @param sd_intercept The standard deviation term to be used
#' in priors for the intercept.
#' @param sd_time The standard deviation term to be used
#' in exchangeable priors for time.
#' @param sd_agesex The standard deviation term to be used
#' in priors for age-sex or age-sex-triangle interactions.
#' @param scale_sd_popn The scale to be used in the prior
#' for standard deviation in the prior model for population.
#' @param scale_sd_rates The scale to be used in the priors
#' for standard deviation in prior models for mortality
#' and fertility rates.
#'
#' @return A list of three model specifications.
#'
#' @examples
#' Lx <- dembase::Counts(Lx_west[ , , 20])
#' mort_rates <- dembase::Values(mx_west[ , , , 20])
#' propn_age_fert <- dembase::Values(propn_age_fert_booth)
#' expected_popn <- make_stationary_popn(popn_size = 100,
#'                                       Lx = Lx,
#'                                     sex_ratio = 105)
#' fert_rates <- make_stationary_fert_rates(Lx = Lx,
#'                                          propn_age_fert = propn_age_fert,
#'                                          sex_ratio = 105)
#' make_system_models(expected_popn = expected_popn,
#'                    mort_rates = mort_rates,
#'                    fert_rates = fert_rates)
#' @export
make_system_models <- function(expected_popn, mort_rates, fert_rates,
                               time_trend_popn = TRUE, 
                               sd_intercept = 0, sd_time = 0,
                               sd_agesex = 0,
                               scale_sd_popn = 1,
                               scale_sd_rates = 0) {
    check_agesex_Count(value = expected_popn,
                       name = "expected_popn")
    check_agesextriangletime_Value(value = mort_rates,
                                   name = "mort_rates")
    check_agesex_Value(value = fert_rates,
                       name = "fert_rates")
    check_logical_flag(time_trend_popn)
    check_nonnegative_numeric(value = sd_intercept,
                              name = "sd_intercept")
    check_nonnegative_numeric(value = sd_time,
                              name = "sd_time")
    check_nonnegative_numeric(value = sd_agesex,
                              name = "sd_agesex")
    check_nonnegative_numeric(value = scale_sd_popn,
                              name = "scale_sd_popn")
    check_nonnegative_numeric(value = scale_sd_rates,
                              name = "scale_sd_rates")
    expected_popn <- methods::as(expected_popn, "Values")
    prior_intercept <- demest::ExchFixed(mean = 0, sd = sd_intercept)
    prior_age <- demest::Zero()
    prior_sex <- demest::Zero()
    prior_triangle <- demest::Zero()
    prior_time <- demest::ExchFixed(sd = sd_time)
    if (time_trend_popn)
        prior_time_popn <- demest::DLM(level = Level(scale = HalfT(scale = 0.025)),
                                       trend = Trend(initial = Initial(sd = 0.025),
                                                     scale = HalfT(scale = 0.025)),
                                       damp = NULL,
                                       error = Error(scale = HalfT(scale = 0.025)))
    else
        prior_time_popn <- prior_time
    prior_agesex_popn <- demest::Known(mean = log(expected_popn), sd = sd_agesex)
    prior_agesex_mort <- demest::Zero()
    prior_agetriangle_mort <- demest::Zero()
    prior_sextriangle_mort <- demest::Zero()
    prior_agesextriangle_mort <- demest::Known(mean = log(mort_rates), sd = sd_agesex)
    prior_agesex_fert <- demest::Known(mean = log(fert_rates), sd = sd_agesex)
    prior_priorSD_popn <- demest::HalfT(df = Inf, scale = scale_sd_popn)
    prior_priorSD_rates <- demest::HalfT(df = Inf, scale = scale_sd_rates)
    mod_popn <- demest::Model(population ~ demest::Poisson(mean ~ age * sex + time,
                                                           useExpose = FALSE),
                              `(Intercept)` ~ prior_intercept,
                              age ~ prior_age,
                              sex ~ prior_sex,
                              time ~ prior_time_popn,
                              age:sex ~ prior_agesex_popn,
                              priorSD = prior_priorSD_popn,
                              jump = 0.05)
    mod_mort <- demest::Model(deaths ~ demest::Poisson(mean ~ age * sex * triangle + time),
                              `(Intercept)` ~ prior_intercept,
                              age ~ prior_age,
                              sex ~ prior_sex,
                              triangle ~ prior_triangle,
                              time ~ prior_time,
                              age:sex ~ prior_agesex_mort,
                              age:triangle ~ prior_agetriangle_mort,
                              sex:triangle ~ prior_sextriangle_mort,
                              age:sex:triangle ~ prior_agesextriangle_mort,
                              priorSD = prior_priorSD_rates)
    mod_fert <- demest::Model(births ~ demest::Poisson(mean ~ age * sex + triangle + time),
                              `(Intercept)` ~ prior_intercept,
                              age ~ prior_age,
                              sex ~ prior_sex,
                              triangle ~ prior_triangle,
                              time ~ prior_time,
                              age:sex ~ prior_agesex_fert,
                              priorSD = prior_priorSD_rates)
    list(mod_popn, mod_mort, mod_fert)
}
