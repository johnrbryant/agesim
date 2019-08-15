
#' Calculate the age-specific fertility rates that
#' produce a given total fertility rate
#'
#' Given an age pattern for fertility and a sex ratio at birth,
#' calculate the age-sex-specific fertility rates that would
#' yield a target total fertility rate (TFR).
#'
#' \code{propn_age_fert} must be a
#' \code{\link[dembase:Counts-class]{Counts}}
#' array, with a single dimension. The dimension must have
#' \code{\link[dembase]{dimtype}} \code{"age"},
#' and \code{\link[dembase]{dimscale}} \code{"Intervals"}.
#'
#' @inheritParams make_stationary_popn
#' @param tfr The target total fertility rate. A positive number.
#' @param propn_age_fert A \code{\link[dembase:Values-class]{Values}}
#' with the age-distribution of fertility rates.
#'
#' @return A \code{\link[dembase]{Values}} object with
#' \code{"age"} and \code{"sex"} dimensions.
#'
#' @seealso \code{\link{propn_age_fert_booth}},
#'
#' @examples
#' propn_age_fert <- dembase::Values(propn_age_fert_booth)
#' make_tfr_fert_rates(tfr = 5,
#'                     propn_age_fert = propn_age_fert,
#'                     sex_ratio = 105)
#' @export
make_tfr_fert_rates <- function(tfr, propn_age_fert, sex_ratio) {
    check_positive_numeric(value = tfr,
                           name = "tfr")
    check_propn_age_fert(propn_age_fert)
    check_positive_numeric(value = sex_ratio,
                           name = "sex_ratio")
    propn_sex <- make_propn_sex(sex_ratio)
    propn_age_sex_fert <- propn_age_fert * propn_sex
    step <- ageTimeStep(propn_age_fert)
    tfr * propn_age_sex_fert / step
}

