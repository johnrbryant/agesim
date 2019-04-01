
context("mx_west")

test_that("mx values sum to expected total", {
    ans_obtained <- sum(mx_west)
    ans_expected <- (sum(demogR::cdmltw(sex = "F")$nmx)
        + sum(demogR::cdmltw(sex = "M")$nmx))
    expect_equal(ans_obtained, ans_expected)
})
