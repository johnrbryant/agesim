
context("mx_west")

test_that("mx values sum to expected total", {
    ## only compare a subset, since we have collapesed
    ## ages 0, 1-14 and ages 80+ to make mx_west
    ans_obtained <- sum(mx_west[2:16,,])
    ans_expected <- (sum(demogR::cdmltw(sex = "F")$nmx[,3:17])
        + sum(demogR::cdmltw(sex = "M")$nmx[,3:17]))
    expect_equal(ans_obtained, ans_expected)
})
