# encrypt each element of character vector `text` using a rotational cipher with
# shift value `key`
rotate <- function(text, key) {

  # perform the rotational cipher on all letters in the string `string` using a
  # rotational cipher with shift value `key`
  rotate_letters <- function(string, key) {

    # the ASCII values of "a" and "A"
    ascii_a <- 97L
    ascii_A <- 65L

    # split the string into a vector of characters
    chars_list <- strsplit(string, "")
    chars <- chars_list[[1L]]

    # determine whether each character is a letter, as well as the offset which
    # is defined to be the either 0 if the character is not a letter, the ASCII
    # value of "a" if character is a lowercase letter, and the value of "A" if
    # character is an uppercase letter
    is_lower <- chars %in% letters
    is_upper <- chars %in% LETTERS
    is_letter <- (is_upper | is_lower)
    offset <- (is_upper * ascii_A) + (is_lower * ascii_a)

    # get the ASCII values of the characters after encrypting them according to
    # the cipher.  The values are only meaningful for letters.
    ascii_vals <- utf8ToInt(string)
    ascii_vals_shifted <- offset + ((ascii_vals - offset + key) %% 26L)

    # user the encrypted values for letters and the original values for
    # non-letters, and then convert the ASCII values back to a string
    ascii_vals_shifted_maybe <- ifelse(
      test = is_letter,
      yes  = ascii_vals_shifted,
      no   = ascii_vals
    )
    intToUtf8(ascii_vals_shifted_maybe)
  }

  stopifnot(
    is.character(text),
    ! is.na(text),
    ! grepl("[^[:ascii:]]", text, perl = TRUE),
    is.numeric(key),
    ! is.na(key),
    length(key) == 1L
  )

  out <- vapply(text, rotate_letters, character(1L), key = key)
  unname(out)
}
