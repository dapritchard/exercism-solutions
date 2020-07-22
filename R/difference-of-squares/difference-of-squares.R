# takes a numeric vector of natural numbers `natural_number` and returns a
# double vector such that for a given element with value `N` in the input
# vector, the corresponding value in the return vector is the difference between
# the square of the sum and the sum of the squares of the first `N` natural
# numbers
difference_of_squares <- function(natural_number) {
  difference_of_squares_single <- function(v) {
    v_seq <- seq_len(v)
    sum(v_seq)^2 - sum(v_seq^2)
  }
  stopifnot(
    is.numeric(natural_number),
    natural_number >= 0
  )
  vapply(
    X         = natural_number,
    FUN       = difference_of_squares_single,
    FUN.VALUE = numeric(1L)
  )
}
