
#' Calculate an implied stable population
#'
#' Calculation the stable (ie constant age structure) population
#' implied by a given combination of total population size,
#' life table Lx, age-specific fertility rates, and sex ratio.
#'
#' \code{Lx} must be a \code{\link[dembase:Counts-class]{Counts}}
#' array with two dimensions. The dimensions must have
#' \code{\link[dembase]{dimtype}} \code{"age"} and \code{"sex"}.
#' The \code{"age"} dimension must have \code{\link[dembase]{dimscale}}
#' \code{"Intervals"}.
#'
#' If \code{time_start} and \code{time_end} arguments
#' are supplied, a time dimension is added to the
#' result.
#' 
#' @inheritParams make_stationary_popn
#' @param fert_rates Age-sex specific fertility rates
#'
#' @return A \code{\link[dembase]{Counts}} object,
#' with the same dimensions as \code{Lx}.
#'
#' @seealso \code{\link{Lx_west}},
#' \code{\link{make_stationary_popn}},
#' \code{\link{make_tfr_fert_rates}}
#' 
#' @examples
#' Lx <- dembase::Counts(Lx_west[ , , "10"])
#' propn_age_fert <- dembase::Values(propn_age_fert_booth)
#' fert_rates <- make_tfr_fert_rates(tfr = 5,
#'                                   propn_age_fert = propn_age_fert,
#'                                   sex_ratio = 105)
#' make_stable_popn(popn_size = 100,
#'                  Lx = Lx,
#'                  sex_ratio = 105,
#'                  fert_rates = fert_rates)
#' x <- make_stable_popn(popn_size = 100,
#'                  Lx = Lx,
#'                  sex_ratio = 105,
#'                  fert_rates = fert_rates,
#'                  time_start = 2000,
#'                  time_end = 2025)
#' @export
make_stable_popn <- function(popn_size, Lx, fert_rates, sex_ratio,
                             time_start = NULL, time_end = NULL) {
    check_positive_numeric(value = popn_size,
                           name = "popn_size")
    check_agesex_Count(value = Lx,
                       name = "Lx")
    check_agesex_Value(value = fert_rates,
                       name = "fert_rates")
    check_positive_numeric(value = sex_ratio,
                           name = "sex_ratio")
    reprod_Lx <- dembase::makeCompatible(x = Lx,
                                         y = fert_rates,
                                         subset = TRUE)
    female_reprod_Lx <- dembase::slab(reprod_Lx,
                                      dimension = "sex",
                                      elements = "Female")
    female_fert <- dembase::slab(fert_rates,
                                 dimension = "sex",
                                 elements = "Female")
    midpoints_reprod <- as.data.frame(female_fert, midpoints = "age")$age
    product_Lx_fert <- as.numeric(female_reprod_Lx * female_fert)
    f <- function(r) {
        prod_exp <- sum(product_Lx_fert * exp(-r * midpoints_reprod))
        abs(prod_exp - 1)
    }
    l <- stats::optimize(f, interval = c(-10, 10))
    r <- l$minimum
    midpoints_all <- as.data.frame(dembase::slab(Lx,
                                                 dimension = "sex",
                                                 elements = "Female"),
                                   midpoints = "age")$age
    multiplier_all <- ValuesOne(exp(-r * midpoints_all),
                                labels = dimnames(Lx)$age,
                                name = "age")
    propn_sex <- make_propn_sex(sex_ratio)
    ans <- popn_size * prop.table(multiplier_all * Lx * propn_sex)
    ans <- methods::as(ans, "Counts")
    if (!is.null(time_start)) {
        step <- dembase::ageTimeStep(ans)
        check_time_start_end(time_start = time_start,
                             time_end = time_end,
                             step = step)
        labels <- seq(from = time_start,
                      to = time_end,
                      by = step)
        scale <- (1 + r)^(labels - time_start)
        ans <- addDimension(ans,
                            name = "time",
                            labels = labels,
                            scale = scale,
                            dimtype = "time",
                            dimscale = "Points")
    }
    ans
}

