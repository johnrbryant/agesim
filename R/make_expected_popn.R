
#' Calculate an implied stationary population
#'
#' Calculation the stationary (ie zero growth rate) population
#' implied by a given combination of total population size,
#' life table Lx, and sex ratio.
#'
#' \code{Lx} must be a \code{\link[dembase:Counts-class]{Counts}}
#' array with two dimensions. The dimensions must have
#' \code{\link[dembase]{dimtype}} \code{"age"} and \code{"sex"}.
#' The \code{"age"} dimension must have \code{\link[dembase]{dimscale}}
#' \code{"Intervals"}.
#'
#' @param popn_size The total population size.
#' @param Lx A \code{\link[dembase:Counts-class]{Counts}}
#' array with the life table Lx values.
#' @param sex_ratio The number of male births per 100 female
#' births.
#'
#' @return A \code{\link[dembase]{Counts}} object,
#' with the same dimensions as \code{Lx}.
#'
#' @seealso \code{\link{Lx_west}},
#' \code{\link{make_stationary_fert_rates}}
#' 
#' @examples
#' Lx <- dembase::Counts(Lx_west[ , , "10"])
#' make_expected_popn(popn_size = 100,
#'                    Lx = Lx,
#'                    sex_ratio = 105)
#' @export
make_expected_popn <- function(popn_size, Lx, sex_ratio) {
    check_positive_numeric(value = popn_size,
                           name = "popn_size")
    check_agesex_Count(value = Lx,
                       name = "Lx")
    check_positive_numeric(value = sex_ratio,
                           name = "sex_ratio")
    propn_sex <- make_propn_sex(sex_ratio)
    unnormalized <- Lx * propn_sex
    popn_size * prop.table(unnormalized)
}

