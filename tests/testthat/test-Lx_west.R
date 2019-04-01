
context("Lx_west")

test_that("Lx values sum to expected total", {
    ans_obtained <- sum(Lx_west)
    ans_expected <- (sum(demogR::cdmltw(sex = "F")$nLx)
        + sum(demogR::cdmltw(sex = "M")$nLx))
    expect_equal(ans_obtained, ans_expected)
})
