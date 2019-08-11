
#' Generate random initial population based on
#' an expected population counts.
#'
#' Randomly enerate an initial population 
#' based on expected population counts.
#'
#' When \code{poisson} is \code{TRUE} (the default)
#' the initial population counts are drawn
#' from Poisson
#' distributions centered at the expected values,
#' with some extra variation.
#' If \eqn{c_{a,s}} is the expected population size for
#' age group \eqn{a} and sex \eqn{s},
#' then the initial population is generated using
#'   \deqn{n_{a,s} \sim Poisson(v_{a,s} c_{a, s})}
#' where
#'   \deqn{\log(v_{a,s}) \sim N(0, b^2)}.
#' A value for \eqn{b}, which may be 0, is supplied by the user,
#' via the \code{sd} argument.
#'
#' When \code{poisson} is \code{FALSE}, the initial
#' population counts are obtained simply by rounding
#' the expected values to the nearest integer.
#'
#' @inheritParams make_system_models
#' @param poisson If \code{TRUE}
#' @param sd A non-negative number, defaulting to 0.
#' Governs extra variability in initial population,
#' on top of Poisson variability.
#'
#' @return A \code{\link[dembase]{Counts}} object,
#' with the same dimensions as \code{expected_popn}.
#'
#' @seealso \code{make_expected_popn}
#'
#' @examples
#' Lx <- dembase::Counts(Lx_west[ , , 20])
#' expected_popn <- make_expected_popn(popn_size = 100,
#'                                     Lx = Lx,
#'                                     sex_ratio = 105)
#' make_initial_popn(expected_popn = expected_popn)
#' make_initial_popn(expected_popn = expected_popn,
#'                   sd = 0.05)
#' make_initial_popn(expected_popn = expected_popn,
#'                   poisson = FALSE)
#' @export
make_initial_popn <- function(expected_popn, poisson = FALSE, sd = 0) {
    check_logical_flag(value = poisson,
                       name = "poisson")
    check_nonnegative_numeric(value = sd,
                              name = "sd")
    if (poisson) {
        n <- length(expected_popn)
        log_v <- stats::rnorm(n = n,
                              mean = 0,
                              sd = sd)
        v <- exp(log_v)
        lambda <- v * expected_popn
        .Data <- stats::rpois(n = n,
                              lambda = lambda)
    }
    else
        .Data <- round(expected_popn)
    metadata <- expected_popn@metadata
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    methods::new("Counts",
                 .Data = .Data,
                 metadata = metadata)
}

