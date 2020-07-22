# takes a positive whole number `number` and returns either NULL if the value
# for `number` is either 1 or 1L, and returns a vector of the prime factors of
# `number` in increasing order otherwise.
#
# Note that that a simple optimization that could be made is to start the
# `find_smallest_factor` function with the size of the last factor that was
# found (rather than starting from 2 each time), since any smaller factors would
# already have been divided out of the current dividend.

prime_factors <- function(number) {

  # returns a named list with elements `has_factor` and `value`.  `has_factor`
  # is either `TRUE` or `FALSE` depending on whether `number` is factorizeable,
  # and `value` is given by the smallest divisor of `number` (with the exception
  # of when `number` is 1, in which case the value of `value` is `NULL`).
  find_smallest_factor <- function(number) {

    # this function only works for `number >= 4` due to the call to `seq.int`
    # starting at 2
    find_smallest_factor_ge4 <- function(number) {
      stopifnot(number >= 4)
      number_root <- as.integer(sqrt(number))
      for (k in seq.int(2, number_root)) {
        if ((number %% k) == 0L) {
          return(
            list(
              has_factor = TRUE,
              value      = k)
          )
        }
      }
      list(
        has_factor = FALSE,
        value      = number
      )
    }

    # list out some special cases
    if (number == 1L) {
      out <- list(has_factor = FALSE, value = NULL)
    } else if (number < 4) {
      out <- list(has_factor = FALSE, value = number)
    } else {
      out <- find_smallest_factor_ge4(number)
    }
    out
  }

  stopifnot(
    is.numeric(number),
    ! is.na(number),
    length(number) == 1L,
    number >= 1
  )

  # pick off the smallest factor (note that this will be a prime factor), and
  # recursively call `prime_factors` on the result of dividing `number` by its
  # smallest factor
  smallest_factor <- find_smallest_factor(floor(number))
  `if`(
    smallest_factor$has_factor,
    c(smallest_factor$value, prime_factors(number %/% smallest_factor$value)),
    smallest_factor$value
  )
}
