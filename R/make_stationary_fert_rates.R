
#' Calculate the fertility rates that would produce a
#' stationary population
#'
#' Given a set of Lx, and an age pattern for fertility,
#' calculate the age-sex-specific fertility rates that would
#' produce a stationary (ie zero growth rate) population.
#'
#' \code{Lx} must be a \code{\link[dembase:Values-class]{Values}}
#' array with two dimensions. The dimensions of \code{Lx} must have
#' \code{\link[dembase]{dimtype}} \code{"age"} and \code{"sex"}.
#' The \code{"age"} dimension must have \code{\link[dembase]{dimscale}}
#' \code{"Intervals"}.
#'
#' \code{propn_age_fert} must be a
#' \code{\link[dembase:Values-class]{Values}}
#' array, with a single dimension. The dimension must have
#' \code{\link[dembase]{dimtype}} \code{"age"},
#' and \code{\link[dembase]{dimscale}} \code{"Intervals"}.
#'
#' @inheritParams make_expected_popn
#' @param propn_age_fert A \code{\link[dembase:Values-class]{Values}}
#' with the age-distribution of fertility rates.
#'
#' @return A \code{\link[dembase]{Values}} object with
#' \code{"age"} and \code{"sex"} dimensions.
#'
#' @seealso \code{\link{Lx_west}}, \code{\link{propn_age_fert_booth}},
#' \code{\link{make_stationary_fert_rates}}
#'
#' @examples
#' Lx <- dembase::Values(Lx_west[ , , "10"])
#' propn_age_fert <- dembase::Values(propn_age_fert_booth)
#' make_stationary_fert_rates(Lx = Lx,
#'                            propn_age_fert = propn_age_fert,
#'                            sex_ratio = 105)
#' @export
make_stationary_fert_rates <- function(Lx, propn_age_fert, sex_ratio) {
    check_agesex_Value(value = Lx,
                     name = "Lx")
    check_propn_age_fert(propn_age_fert)
    check_positive_numeric(value = sex_ratio,
                           name = "sex_ratio")
    propn_sex <- make_propn_sex(sex_ratio)
    propn_age_sex_fert <- propn_age_fert * propn_sex
    popn_reprod_age <- dembase::makeCompatible(x = Lx,
                                               y = propn_age_sex_fert,
                                               subset = TRUE)
    product <- popn_reprod_age * propn_age_sex_fert
    i_sex <- match("sex", dembase::dimtypes(product))
    DS_sex <- dembase::DimScales(product)[[i_sex]]
    i_female <- dembase::iFemale(DS_sex)
    product_female <- dembase::slab(product,
                                    dimension = i_sex,
                                    elements = i_female)
    multiplier <- 1 / sum(product_female)
    multiplier * propn_age_sex_fert
}

