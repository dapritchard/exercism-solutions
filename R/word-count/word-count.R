# takes a string `input` as input, and returns a named list such that the name
# of each element is a word from `input` (after converting uppercase letters to
# lowercase and stripping punctuation), and the value of the corresponding
# element is the number of times that the word is seen in `input`
word_count <- function(input) {

  stopifnot(is_string(input))

  # strip punctuation, remove leading and trailing spaces, and convert uppercase
  # letters to lowercase
  input_without_punct <- gsub("[^[:alnum:] ]", "", input, perl = TRUE)
  input_without_punct_space <- gsub("(^ +| +$)", "", input_without_punct, perl = TRUE)
  input_nrm <- tolower(input_without_punct_space)

  # count the number of times each word is seen
  word_split_list <- strsplit(input_nrm, " +", perl = TRUE)
  word_split <- word_split_list[[1L]]
  as.list(table(word_split))
}


# returns `TRUE` if `x` is a character vector without any missing values, and
# `FALSE` otherwise
is_chr_nomiss <- function(x) {
  is.character(x) && (! any(is.na(x)))
}


# returns `TRUE` if `x` is a string and `FALSE` otherwise
is_string <- function(x) {
  is_chr_nomiss(x) && (length(x) == 1L)
}
