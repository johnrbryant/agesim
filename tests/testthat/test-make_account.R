
context("make_account")

test_that("make_account creates expected outputs", {
    Lx <- dembase::Values(Lx_west[,,15])
    mort_rates <- dembase::Values(mx_west[,,15])
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    expected_popn <- make_expected_popn(popn_size = 123,
                                        Lx = Lx,
                                        sex_ratio = 111)
    fert_rates <- make_stationary_fert_rates(Lx = Lx,
                                             propn_age_fert = propn_age_fert,
                                             sex_ratio = 111)
    ans <- make_account(expected_popn = expected_popn,
                        mort_rates = mort_rates,
                        fert_rates = fert_rates,
                        time_start = 1000,
                        time_end = 1050)
    expect_is(ans, "Movements")
    expect_true(validObject(ans))
})