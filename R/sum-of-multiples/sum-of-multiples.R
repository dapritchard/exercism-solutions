# find the sum of all the unique multiples of numbers specified by `factors` up
# to but not including the number `limit`
sum_of_multiples <- function(factors, limit) {
  `if`(
    (is.vector(factors) || is.atomic(factors)) && (length(factors) == 0L),
    0,
    sum_of_multiples_nonempty(factors, limit)
  )
}


# calculates the same value as for `sum_of_multiples`, but for the stronger
# assumption that the length of `factors` is at least 1
sum_of_multiples_nonempty <- function(factors, limit) {

  calc_multiples <- function(factor, max_multiple) {
    factor * seq_len(max_multiple)
  }

  stopifnot(
    is.numeric(factors),
    length(factors) >= 1L,
    ! is.na(factors),
    factors > 0,
    is.numeric(limit),
    ! is.na(limit),
    limit >= 0,
    length(limit) == 1L
  )

  # calculate the multiples for each element of `factors` that are no greater
  # than `limit`
  max_multiples <- floor(limit / factors)
  multiples_list <- Map(
    f            = calc_multiples,
    factor       = factors,
    max_multiple = max_multiples
  )

  # create a numeric vector containing all the multiples of the elements of
  # `factors` that are no greater than `limit`
  multiples <- unique(unlist(multiples_list))
  multiples_below_limit <- multiples[multiples < limit]

  sum(multiples_below_limit)
}
