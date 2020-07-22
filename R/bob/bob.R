# takes a character vector `input` as input and returns a character vector of
# the same length as the input such that each element is determined by the
# following rules (without consideration for whitespace) based on the
# corresponding element of the input:
#
# * 'Fine. Be that way!' if it is an empty string.
#
# * 'Whatever.' if the string doesn't end in a question mark and is not all
#   capital letters (not including punctuation).
#
# * 'Whoa, chill out!' if the string doesn't end in a question mark and is all
#   capital letters (not including punctuation).
#
# * 'Sure.' if the string does end in a question mark and is not all capital
#   letters (not including punctuation).
#
# * 'Calm down, I know what I'm doing!' if the string does end in a question mark
#   and is all capital letters (not including punctuation).

bob <- function(input) {
  stopifnot(
    is.character(input),
    ! is.na(input),
    ! grepl("[^[:ascii:]]", input, perl = TRUE)
  )
  `if`(
    length(input) == 0L,
    character(0L),
    bob_impl(input)
  )
}


# takes the same input as `bob` with the additional restriction that `input` may
# not be a length-0 vector, and otherwise returns an output according to the
# same rules

bob_impl <- function(input) {

  create_response <- function(is_empty_single,
                              is_all_uppercase_single,
                              is_question_single) {
    `if`(
      is_empty_single,
      "Fine. Be that way!",
      `if`(
        is_all_uppercase_single,
        `if`(
          is_question_single,
          "Calm down, I know what I'm doing!",
          "Whoa, chill out!"
        ),
        `if`(
          is_question_single,
          "Sure.",
          "Whatever."
        )
      )
    )
  }

  stopifnot(
    is.character(input),
    length(input) > 0L,
    ! is.na(input),
    ! grepl("[^[:ascii:]]", input, perl = TRUE)
  )

  # strip any whitespace from input
  input_no_whitespace <- gsub("\\s", "", input, perl = TRUE)

  # calculate `is_empty`, `is_question`, and `is_all_uppercase` variables
  is_empty <- (nchar(input_no_whitespace) == 0L)
  is_question <- endsWith(input_no_whitespace, "?")
  any_lowercase <- grepl("[a-z]", input_no_whitespace, perl = TRUE)
  any_uppercase <- grepl("[A-Z]", input_no_whitespace, perl = TRUE)
  is_all_uppercase <- ((! any_lowercase) & any_uppercase)

  # calculate the appropriate response for each element
  out_list <- Map(
    f                       = create_response,
    is_empty_single         = is_empty,
    is_all_uppercase_single = is_all_uppercase,
    is_question_single      = is_question
  )
  unlist(out_list)
}
