# implementtion of the Sieve of Eratosthenes based on
# https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Algorithm_and_variants.
#
#     let A be an array of Boolean values, indexed by integers 2 to n,
#     initially all set to true.
#
#     for i = 2, 3, 4, ..., not exceeding √n do
#         if A[i] is true
#             for j = i2, i2+i, i2+2i, i2+3i, ..., not exceeding n do
#                 A[j] := false
#
#     return all i such that A[i] is true.

sieve <- function(limit) {

  # perform the Sieve of Eratosthenes on values of `limit` >= 4.  The reason
  # that it needs to be at least 4 is due to the call `seq` requiring that the
  # `from` argument be no less than the `to` argument when `by` is positive.
  sieve_limit_ge4 <- function(limit) {

    # create a sequence of integers from 1 to `limit`, and a corresponding
    # logical vector representing whether each value is a prime number
    vals <- seq_len(limit)
    primes_lgl <- ! logical(limit)
    primes_lgl[1L] <- FALSE
    primte_lgl[(vals %% 2L) == 0L] <- FALSE

    # for each value `i`, mark all multiples of `i` as nonprime.  Note that we
    # can ignore multiples of `i` less than `i^2`, since all such values are
    # already marked by previous iterations.  Also note that we can ignore even
    # multiples of `i` since we already marked all multiples of 2 as `FALSE`.
    for (i in seq.int(2L, sqrt(limit))) {
      if (primes_lgl[i]) {
        nonprimes <- seq(i * i, limit, 2L * i)
        primes_lgl[nonprimes] <- FALSE
      }
    }

    vals[primes_lgl]
  }

  stopifnot(
    is.numeric(limit),
    ! is.na(limit),
    length(limit) == 1L,
    limit >= 1
  )

  # handle a few special cases individually, and send the rest to
  # `sieve_limit_ge4`
  if (limit == 1L) {
    out <- return(NULL)
  }
  else if (limit == 2L) {
    out <- return(2L)
  }
  else if (limit == 3L) {
    out <- return(c(2L, 3L))
  }
  else {
    out <- sieve_limit_ge4(limit)
  }
  out
}
