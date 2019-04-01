
check_popn_size <- function(popn_size) {
    stopifnot(is.numeric(popn_size))
    stopifnot(identical(length(popn_size), 1L))
    stopifnot(!is.na(popn_size))
    stopifnot(popn_size > 0)
    NULL
}

check_Lx <- function(Lx) {
    stopifnot(is.numeric(Lx))
    stopifnot(is.array(Lx))
    stopifnot(!any(is.na(Lx)))
    stopifnot(all(Lx >= 0))
    stopifnot(!is.null(dimnames(Lx)))
    stopifnot(!is.null(names(dimnames(Lx))))
    stopifnot(setequal(names(dimnames(Lx)), c("age", "sex")))
    stopifnot(identical(dimnames(Lx)[["sex"]], c("Female", "Male")))
    NULL
}

check_sex_ratio <- function(sex_ratio) {
    stopifnot(is.numeric(sex_ratio))
    stopifnot(identical(length(sex_ratio), 1L))
    stopifnot(!is.na(sex_ratio))
    stopifnot(sex_ratio > 0)
    NULL
}


