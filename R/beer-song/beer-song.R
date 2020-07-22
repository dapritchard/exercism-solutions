# takes two nonnegative values `first` and `last`, and returns a string
# containing the lyrics of "99 bottles of beer on the wall" for all of the
# verses with a quantity of beer bottles between `first` and `last` (inclusive)
lyrics <- function(first, last) {
  stopifnot(
    is.numeric(first),
    ! is.na(first),
    length(first) == 1L,
    first >= 0,
    is.numeric(last),
    ! is.na(last),
    length(last) == 1L,
    last >= 0
  )
  bottle_idx <- floor(first):floor(last)
  out_vec <- verse(bottle_idx)
  paste(out_vec, collapse = "\n")
}


# takes a numeric vector `number` as input, and returns a character vector such
# that each element is the lyric from "99 bottles of beer on the wall" with the
# quantity of beer bottles given by the corresponding element of `number`
verse <- function(number) {

  create_verse_3_or_more <- function(number_single) {
    str <- paste0(
      "%d bottles of beer on the wall, %d bottles of beer.\n",
      "Take one down and pass it around, %d bottles of beer on the wall.\n"
    )
    sprintf(str, number_single, number_single, number_single - 1)
  }

  create_verse_2 <- function(number_single) {
    paste0(
      "2 bottles of beer on the wall, 2 bottles of beer.\n",
      "Take one down and pass it around, 1 bottle of beer on the wall.\n"
    )
  }

  create_verse_1 <- function(number_single) {
    paste0(
      "1 bottle of beer on the wall, 1 bottle of beer.\n",
      "Take it down and pass it around, no more bottles of beer on the wall.\n"
    )
  }

  create_verse_0 <- function(number_single) {
    paste0(
      "No more bottles of beer on the wall, no more bottles of beer.\n",
      "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    )
  }
  
  stopifnot(
    is.numeric(number),
    ! is.na(number),
    0 <= number
  )

  # create a data frame for verse creation functions and corresponding labels
  verse_fcn_map <- tibble::tibble(
    fcn = list(
      create_verse_3_or_more,
      create_verse_2,
      create_verse_1,
      create_verse_0
    ),
    type = c("3 or more", "2", "1", "0")
  )

  # create a data frame of the input numbers (after truncating them to whole
  # numbers) and their corresponding labels
  number_floor <- floor(number)
  numbers_df <- data.frame(
    number = number,
    type   = ifelse(number_floor >= 3, "3 or more", as.character(number_floor)),
    order  = seq_along(number)
  )

  # create a data frame with the verse creation functions merged in to the
  # numbers data frame (and restore the original ordering)
  numbers_fcn_unordered_df <- merge(
    x     = numbers_df,
    y     = verse_fcn_map,
    by    = "type",
    all.x = TRUE
  )
  order_idx <- order(numbers_fcn_unordered_df$order)
  numbers_fcn_df <- numbers_fcn_unordered_df[order_idx, ]

  # use the verse creation functions to map the numbers to the appropriate
  # verses
  out_list <- Map(
    f   = function(fcn, n) fcn(n),
    fcn = numbers_fcn_df$fcn,
    n   = numbers_fcn_df$number
  )
  unlist(out_list)
}
