# takes a numeric vector of inputs `n` such that each element is a value between
# 1 and 64, and returns a double vector such that for a given element `x` in the
# input vector, the corresponding element of the return vector has the value of
# `2^(x - 1)`
square <- function(n) {
  stopifnot(
    is.numeric(n),
    1 <= n,
    n <= 64
  )
  sum(2^(n - 1))
}


# get the total number of grains on a chessboard
total <- function() {
  square(seq_len(64))
}
