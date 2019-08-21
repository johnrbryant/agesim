
context("make_account")

test_that("make_account creates expected outputs", {
    Lx <- dembase::Counts(Lx_west[,,15])
    mort_rates <- dembase::Values(mx_west[,,,15])
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    expected_popn <- make_stationary_popn(popn_size = 123,
                                          Lx = Lx,
                                          sex_ratio = 111)
    initial_popn <- make_initial_popn(expected_popn = expected_popn,
                                      sd = 0.02)
    fert_rates <- make_stationary_fert_rates(Lx = Lx,
                                             propn_age_fert = propn_age_fert,
                                             sex_ratio = 111)
    ans <- make_account(popn = initial_popn,
                        mort_rates = mort_rates,
                        fert_rates = fert_rates,
                        time_start = 1000,
                        time_end = 1050)
    expect_is(ans, "Movements")
    expect_true(validObject(ans))
    expect_equal(subarray(population(ans), time == "1000"),
                 initial_popn)
})
