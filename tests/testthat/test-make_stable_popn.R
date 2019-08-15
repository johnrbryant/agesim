
context("make_stable_popn")

test_that("make_stable_popn consistent with make_stationary_popn", {
    popn_size <- 123
    Lx <- dembase::Counts(Lx_west[,,15])
    sex_ratio <- 111
    ans_stationary <- make_stationary_popn(popn_size = popn_size,
                                           Lx = Lx,
                                           sex_ratio = sex_ratio)
    propn_age_fert <- dembase::Values(propn_age_fert_booth)
    fert_stationary <- make_stationary_fert_rates(Lx = Lx,
                                                  propn_age_fert = propn_age_fert,
                                                  sex_ratio = sex_ratio)
    ans_stable <- make_stable_popn(popn_size = popn_size,
                                   Lx = Lx,
                                   sex_ratio = sex_ratio,
                                   fert_rates = fert_stationary)
    expect_equal(ans_stationary, ans_stable, tol = 0.001)
    ans_stationary <- make_stationary_popn(popn_size = popn_size,
                                           Lx = Lx,
                                           sex_ratio = sex_ratio,
                                           time_start = 0,
                                           time_end = 20)
    propn_age_fert <- dembase::Values(propn_age_fert_booth)
    fert_stationary <- make_stationary_fert_rates(Lx = Lx,
                                                  propn_age_fert = propn_age_fert,
                                                  sex_ratio = sex_ratio)
    ans_stable <- make_stable_popn(popn_size = popn_size,
                                   Lx = Lx,
                                   sex_ratio = sex_ratio,
                                   fert_rates = fert_stationary,
                                   time_start = 0,
                                   time_end = 20)
    expect_equal(ans_stationary, ans_stable, tol = 0.001)
})
