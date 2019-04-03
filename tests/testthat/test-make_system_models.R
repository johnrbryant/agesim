
context("make_system_models")

test_that("make_system_models creates expected outputs", {
    Lx <- dembase::Values(Lx_west[,,15])
    mort_rates <- dembase::Values(mx_west[,,15])
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    expected_popn <- make_expected_popn(popn_size = 123,
                                        Lx = Lx,
                                        sex_ratio = 111)
    fert_rates <- make_stationary_fert_rates(Lx = Lx,
                                             propn_age_fert = propn_age_fert,
                                             sex_ratio = 111)
    ans <- make_system_models(expected_popn = expected_popn,
                              mort_rates = mort_rates,
                              fert_rates = fert_rates)
    expect_identical(length(ans), 3L)
    expect_true(all(sapply(ans, class) == "SpecPoissonVarying"))
})


test_that("make_system_models works with zero variances", {
    Lx <- dembase::Values(Lx_west[,,15])
    mort_rates <- dembase::Values(mx_west[,,15])
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    expected_popn <- make_expected_popn(popn_size = 123,
                                        Lx = Lx,
                                        sex_ratio = 111)
    fert_rates <- make_stationary_fert_rates(Lx = Lx,
                                             propn_age_fert = propn_age_fert,
                                             sex_ratio = 111)
    ans <- make_system_models(expected_popn = expected_popn,
                              mort_rates = mort_rates,
                              fert_rates = fert_rates,
                              sd_intercept = 0,
                              sd_time = 0,
                              sd_agesex = 0,
                              scale_sd_popn = 0.1,
                              scale_sd_rates = 0)
    expect_identical(length(ans), 3L)
    expect_true(all(sapply(ans, class) == "SpecPoissonVarying"))
})
