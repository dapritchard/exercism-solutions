# takes a character vector as input and returns a numeric vector such that each
# element has the value given by the sum of the scrabble values of each letter
# in the corresponding string from the input vector
scrabble_score <- function(input){

  score_single <- function(chars, letter_vals) {
    sum(letter_vals[chars])
  }

  stopifnot(
    is.character(input),
    ! is.na(input)
  )

  letter_vals = create_letter_vals()
  chars_list <- strsplit(tolower(input), "")
  vapply(
    X           = chars_list,
    FUN         = score_single,
    FUN.VALUE   = numeric(1L),
    letter_vals = letter_vals
  )
}


# creates an associative array of the letter values in a scrabble game
create_letter_vals <- function() {
  c(
    "a" = 1,
    "e" = 1,
    "i" = 1,
    "o" = 1,
    "u" = 1,
    "l" = 1,
    "n" = 1,
    "r" = 1,
    "s" = 1,
    "t" = 1,
    "d" = 2,
    "g" = 2,
    "b" = 3,
    "c" = 3,
    "m" = 3,
    "p" = 3,
    "f" = 4,
    "h" = 4,
    "v" = 4,
    "w" = 4,
    "y" = 4,
    "k" = 5,
    "j" = 8,
    "x" = 8,
    "q" = 10,
    "z" = 10
  )
}
