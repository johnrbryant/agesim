
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
#' If \code{time_start} and \code{time_end} arguments
#' are supplied, a time dimension is added to the
#' result.
#'
#' @param popn_size The total population size.
#' @param Lx A \code{\link[dembase:Counts-class]{Counts}}
#' array with the life table Lx values.
#' @param sex_ratio The number of male births per 100 female
#' births.
#' @param time_start First time point. Optional.
#' @param time_end Last time point. Optional.
#'
#' @return A \code{\link[dembase]{Counts}} object,
#' with the same dimensions as \code{Lx}.
#'
#' @seealso \code{\link{Lx_west}},
#' \code{\link{make_stationary_fert_rates}}
#' 
#' @examples
#' Lx <- dembase::Counts(Lx_west[ , , "10"])
#' make_stationary_popn(popn_size = 100,
#'                      Lx = Lx,
#'                      sex_ratio = 105)
#' make_stationary_popn(popn_size = 100,
#'                      Lx = Lx,
#'                      sex_ratio = 105,
#'                      time_start = 2000,
#'                      time_end = 2010)
#' @export
make_stationary_popn <- function(popn_size, Lx, sex_ratio,
                                 time_start = NULL, time_end = NULL) {
    check_positive_numeric(value = popn_size,
                           name = "popn_size")
    check_agesex_Count(value = Lx,
                       name = "Lx")
    check_positive_numeric(value = sex_ratio,
                           name = "sex_ratio")
    propn_sex <- make_propn_sex(sex_ratio)
    unnormalized <- Lx * propn_sex
    ans <- popn_size * prop.table(unnormalized)
    ans <- methods::as(ans, "Counts")
    if (!is.null(time_start)) {
        step <- dembase::ageTimeStep(ans)
        check_time_start_end(time_start = time_start,
                             time_end = time_end,
                             step = step)
        labels <- seq(from = time_start,
                      to = time_end,
                      by = step)
        ans <- addDimension(ans,
                            name = "time",
                            labels = labels,
                            dimtype = "time",
                            dimscale = "Points")
    }
    ans
}

