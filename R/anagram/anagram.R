# takes a string `subject` and a character vector `candidates` as inputs, and
# returns the subset of candidates that are a case-insensitive anagram of
# `subject`, but are not a case-insensitive exact match
anagram <- function(subject, candidates) {

  stopifnot(
    is_string(subject),
    is_chr_nomiss(candidates)
  )

  # lowercase versions of strings to perform case-insensitive matching
  subject_nrm <- tolower(subject)
  candidates_nrm <- tolower(candidates)

  # the i-th element of `is_diff_anagram` is `TRUE` when the i-th element of
  # candidates is a case-insensitive anagram with `subject`, but is not a
  # case-insensitive exact match, and is `FALSE` otherwise
  is_diff_word <- (candidates_nrm != subject_nrm)
  is_anagram <- check_anagram(candidates_nrm, subject_nrm)
  is_diff_anagram <- is_anagram & is_diff_word
  `if`(
    any(is_diff_anagram),
    candidates[is_diff_anagram],
    NULL
  )
}


# takes a string `subject` and a character vector `candidates` as inputs, and
# returns a logical vector such that the i-th element of `candidates` is TRUE
# when the i-th element of candidates is an anagram with `target`
check_anagram <- function(candidates, target) {

  check_equal_name_and_contents <- function(x, y) {
    eq_names <- identical(names(x), names(y))
    eq_contents <- isTRUE(all.equal(x, y, check.attributes = FALSE))
    (eq_names && eq_contents)
  }

  stopifnot(
    is_chr_nomiss(candidates),
    is_string(target)
  )

  freq_subject_list <- calc_letter_freq(target)
  freq_subject <- freq_subject_list[[1L]]
  freq_candidates_list <-  calc_letter_freq(candidates)

  vapply(
    X         = freq_candidates_list,
    FUN       = check_equal_name_and_contents,
    FUN.VALUE = logical(1L),
    y         = freq_subject
  )
}


# takes a character vector `v` as input, and returns a list such that each
# element is a frequency table of the number of times each character is seen in
# the word
calc_letter_freq <- function(v) {
  stopifnot(is_chr_nomiss(v))
  lapply(strsplit(v, ""), table)
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
