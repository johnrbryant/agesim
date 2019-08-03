
context("make_expected_popn")

test_that("make_expected_popn creates population of expected size", {
    set.seed(1)
    expected_popn <- make_expected_popn(popn_size = 123,
                                           Lx = dembase::Counts(Lx_west[,,15]),
                                           sex_ratio = 111)
    ans_obtained <- sum(make_initial_popn(expected_popn = expected_popn))
    ans_expected <- 123
    expect_equal(ans_obtained, ans_expected, tol = 0.1)
    ans_obtained <- sum(make_initial_popn(expected_popn = expected_popn,
                                           sd = 0.02))
    ans_expected <- 123
    expect_equal(ans_obtained, ans_expected, tol = 0.1)
})

test_that("make_expected_popn creates population with expected age structure", {
    expected_popn <- make_expected_popn(popn_size = 123,
                                        Lx = dembase::Counts(Lx_west[,,15]),
                                        sex_ratio = 111)
    popn <- make_initial_popn(expected_popn = expected_popn,
                              sd = 0.01)
    expect_equal(prop.table(popn[, "Female"]), prop.table(Lx_west[,"Female",15]), tol = 0.1)
    expect_equal(prop.table(popn[, "Male"]), prop.table(Lx_west[,"Male",15]), tol = 0.1)
})
