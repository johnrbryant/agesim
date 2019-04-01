
#' Calculate an implied stationary population
#'
#' Calculation the stationary (ie zero growth rate) population
#' implied by a given combination of total population size,
#' life table Lx, and sex ratio.
#'
#' \code{Lx} must have a dimension called \code{"age"}
#' and a dimension called \code{"sex"}. The labels for the
#' age dimension must have the format expected for age
#' intervals by function \code{\link[dembase]{Counts}}.
#' The labels for the sex dimension must be \code{"Female"}
#' and \code{"Male"}.
#'
#' @param popn_size The total population size.
#' @param Lx An array.
#' @param sex_ratio The number of males per 100 females.
#'
#' @return A \code{\link[dembase]{Values}} object,
#' with the same dimensions and dimnames as \code{Lx}.
#'
#' @examples
#' Lx <- Lx_west[ , , "10"]
#' make_expected_popn(popn_size = 100,
#'                    Lx = Lx,
#'                    sex_ratio = 105)
#' @export
make_expected_popn <- function(popn_size, Lx, sex_ratio) {
    check_popn_size(popn_size)
    check_Lx(Lx)
    check_sex_ratio(sex_ratio)
    Lx <- dembase::Values(Lx, dimscales = c(age = "Intervals"))
    propn_sex <- make_propn_sex(sex_ratio)
    unnormalized <- Lx * propn_sex
    popn_size * prop.table(unnormalized)
}

