
context("make_stationary_fert_rates")

test_that("make_stationary_fert_rates gives net reproduction rates of 1", {
    Lx <- dembase::Values(Lx_west[,,5])
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    fert_rates <- make_stationary_fert_rates(Lx = Lx,
                                             propn_age_fert = propn_age_fert,
                                             sex_ratio = 110)
    NRR <- sum(fert_rates[, "Female"] * Lx[3:10 , "Female"])
    expect_equal(NRR, 1)
})
