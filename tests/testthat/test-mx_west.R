
context("mx_west")

test_that("mx values sum to expected total", {
    ## only compare a subset, since we have collapesed
    ## ages 0, 1-14 and ages 60+ to make mx_west
    ans_obtained <- sum(mx_west[2:12,,])
    ans_expected <- (sum(demogR::cdmltw(sex = "F")$nmx[,3:13])
        + sum(demogR::cdmltw(sex = "M")$nmx[,3:13]))
    expect_equal(ans_obtained, ans_expected)
})
