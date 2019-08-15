
context("make_stationary_popn")

test_that("make_stationary_popn creates population of expected size", {
    ans_obtained <- sum(make_stationary_popn(popn_size = 123,
                                             Lx = dembase::Counts(Lx_west[,,15]),
                                             sex_ratio = 111))
    ans_expected <- 123
    expect_identical(ans_obtained, ans_expected)
    ans_obtained <- as.numeric(collapseDimension(make_stationary_popn(popn_size = 123,
                                                                      Lx = dembase::Counts(Lx_west[,,15]),
                                                                      sex_ratio = 111,
                                                                      time_start = 0,
                                                                      time_end = 10),
                                                 margin = "time"))
    ans_expected <- rep(123, 3)
    expect_equal(ans_obtained, ans_expected)
})

test_that("make_stationary_popn creates population with expected age structure", {
    popn <- make_stationary_popn(popn_size = 123,
                               Lx = dembase::Counts(Lx_west[,,15]),
                               sex_ratio = 111)
    expect_equal(prop.table(popn[, "Female"]), prop.table(Lx_west[,"Female",15]))
    expect_equal(prop.table(popn[, "Male"]), prop.table(Lx_west[,"Male",15]))
})
