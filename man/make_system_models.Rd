% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_system_models.R
\name{make_system_models}
\alias{make_system_models}
\title{Make system models for a demographic account}
\usage{
make_system_models(
  expected_popn,
  mort_rates,
  fert_rates,
  time_trend_popn = TRUE,
  sd_intercept = 0,
  sd_time = 0,
  sd_agesex = 0,
  scale_sd_popn = 1,
  scale_sd_rates = 0
)
}
\arguments{
\item{expected_popn}{A \code{\link[dembase:Counts-class]{Counts}}
array giving expected population counts by age and sex. Typically
created using functions \code{\link{make_stationary_popn}},
or \code{\link{make_stable_popn}}.}

\item{mort_rates}{A \code{\link[dembase:Values-class]{Values}}
array giving mortality rates by age and sex.}

\item{fert_rates}{A \code{\link[dembase:Values-class]{Values}}
array giving mean fertility rates by age and sex. Typically
generated using function \code{\link{make_stationary_fert_rates}}.}

\item{time_trend_popn}{Whether to use random walk model
for the time effect in population. Defaults to
\code{TRUE}.}

\item{sd_intercept}{The standard deviation term to be used
in priors for the intercept.}

\item{sd_time}{The standard deviation term to be used
in exchangeable priors for time.}

\item{sd_agesex}{The standard deviation term to be used
in priors for age-sex or age-sex-triangle interactions.}

\item{scale_sd_popn}{The scale to be used in the prior
for standard deviation in the prior model for population.}

\item{scale_sd_rates}{The scale to be used in the priors
for standard deviation in prior models for mortality
and fertility rates.}
}
\value{
A list of three model specifications.
}
\description{
Make system models for population, deaths, and births,
for a demographic account
}
\examples{
Lx <- dembase::Counts(Lx_west[ , , 20])
mort_rates <- dembase::Values(mx_west[ , , , 20])
propn_age_fert <- dembase::Values(propn_age_fert_booth)
expected_popn <- make_stationary_popn(popn_size = 100,
                                      Lx = Lx,
                                    sex_ratio = 105)
fert_rates <- make_stationary_fert_rates(Lx = Lx,
                                         propn_age_fert = propn_age_fert,
                                         sex_ratio = 105)
make_system_models(expected_popn = expected_popn,
                   mort_rates = mort_rates,
                   fert_rates = fert_rates)
}
