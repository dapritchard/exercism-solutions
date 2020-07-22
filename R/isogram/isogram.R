# takes a character vector `word` as input, and returns a logical vector such
# that each element of the return value is `TRUE` if the corresponding element
# of `word` is an isogram, and `FALSE` otherwise.  An isogram is defined to be a
# word that doesn't include any (case-insensitive) letters more than once.
# Non-letter characters have no effect in determining whether a word is an
# anogram.

is_isogram <- function(word) {

  all_singluar <- function(v) {
    all(v <= 1L)
  }

  stopifnot(
    is.character(word),
    ! is.na(word)
  )

  # count the number of characters in each word (case-insensitive)
  word_lowercase <- tolower(word)
  word_letters <- gsub("[^[:alpha:]]", "", word_lowercase, perl = TRUE)
  char_list <- strsplit(word_letters, "")
  char_count_list <- lapply(char_list, table)

  # determine whether each word has any multiple
  vapply(char_count_list, all_singluar, logical(1L))
}
