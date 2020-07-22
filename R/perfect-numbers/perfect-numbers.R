# takes a numeric vector `n` as input, and returns a character vector with each
# element providing information about the number type of the corresponding
# element of `n`, and taking either the value `"deficient"`, `"perfect"`, or
# `"abundant"`.  A number is classified to one of these three categories based
# on its aliquot sum, which is defined as the sum of all of the factors of a
# number not including the number itself.  A number is then categorized
# according to the following rules:
#
# * deficient:  the aliquot sum is less than the number
# * perfect:    the aliquot sum is equal to the number
# * abundant:   the aliquot sum is greater than the number

number_type <- function(n) {

  calc_aliquot_sum <- function(n) {
    n_factors <- find_factors(n)
    vapply(n_factors, sum, integer(1L))
  }

  stopifnot(
    is.numeric(n),
    ! is.na(n),
    n >= 1
  )

  n_floor <- floor(n)
  aliquot_sum <- calc_aliquot_sum(n_floor)
  aliquot_diff <- aliquot_sum - n_floor

  paste0(
    ifelse(aliquot_diff < 0, "deficient", ""),
    ifelse(aliquot_diff == 0, "perfect", ""),
    ifelse(aliquot_diff > 0, "abundant", "")
  )
}


# takes a numeric vector `n` as input, and returns a list such that each element
# is an integer vector containing the factors of the corresponding element of
# `n`.  Note that this is a highly inefficient version of such a factorization
# algorithm.
find_factors <- function(n) {
  find_factors_single <- function(n_single) {
    maybe_factors <- seq_len(n_single - 1)
    maybe_factors[(n_single %% maybe_factors) == 0L]
  }
  stopifnot(
    is.numeric(n),
    ! is.na(n),
    n >= 1
  )
  lapply(n, find_factors_single)
}
