
context("make_expected_popn")

test_that("make_expected_popn creates population of expected size", {
    ans_obtained <- sum(make_expected_popn(popn_size = 123,
                                           Lx = dembase::Values(Lx_west[,,15]),
                                           sex_ratio = 111))
    ans_expected <- 123
    expect_identical(ans_obtained, ans_expected)
})

test_that("make_expected_popn creates population with expected age structure", {
    popn <- make_expected_popn(popn_size = 123,
                               Lx = dembase::Values(Lx_west[,,15]),
                               sex_ratio = 111)
    expect_equal(prop.table(popn[, "Female"]), prop.table(Lx_west[,"Female",15]))
    expect_equal(prop.table(popn[, "Male"]), prop.table(Lx_west[,"Male",15]))
})
