
context("make_tfr_fert_rates")

test_that("make_tfr_fert_rates gives rates that sum to tfr (accounting for length of age step)", {
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    fert_rates <- make_tfr_fert_rates(tfr = 5,
                                      propn_age_fert = propn_age_fert,
                                      sex_ratio = 110)
    expect_equal(dembase:::tfr(fert_rates), 5)
})


test_that("make_tfr_fert_rates has age and sex dimensions", {
    propn_age_fert = dembase::Values(propn_age_fert_booth)
    fert_rates <- make_tfr_fert_rates(tfr = 5,
                                      propn_age_fert = propn_age_fert,
                                      sex_ratio = 110)
    expect_equal(sort(names(dimnames(fert_rates))), c("age", "sex"))
})

