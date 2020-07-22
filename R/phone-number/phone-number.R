# change `failure_action` to e.g. `stop` to throw an error when invalid numbers
# are provided
parse_phone_number <- function(number_string, failure_action = function(msg) NULL) {

  # strip any non-digit characters
  strip_non_digit_chars <- function(number_string) {
    digits_only <- gsub("[^\\d]", "", number_string, perl = TRUE)
    decorated_numbers(digits_only, TRUE, "")
  }

  # remove leading digit from numbers with 11 digits
  remove_leading_1_from_11_digit_nums <- function(number_string) {

    # conditionally remove leading 1
    has_11_digits <- (nchar(number_string) == 11L)
    without_leading_1 <- ifelse(
      has_11_digits,
      sub("^1", "", number_string, perl = TRUE),
      number_string
    )

    # construct error message
    has_leading_1 <- startsWith(number_string, "1")
    has_11_digits_without_leading_1 <- (has_11_digits & (! has_leading_1))
    msg <- err_msg(
      "the following numbers have 11 digits but do not have a leading 1:",
      number_string[has_11_digits_without_leading_1]
    )

    decorated_numbers(without_leading_1, any(! has_11_digits_without_leading_1), msg)
  }

  # ensure that all numbers have 10 digits
  check_all_nums_have_10_digits <- function(number_string) {
    has_10_digits <- (nchar(number_string) == 10L)
    msg <- err_msg(
      "the following numbers do not have either 10 digits or 11 digits with a leading 1:",
      number_string[! has_10_digits]
    )
    decorated_numbers(number_string, all(has_10_digits), msg)
  }

  # ensure that all area codes start with digits 2-9
  check_area_code_2_through_9 <- function(number_string) {
    stopifnot(nchar(number_string) == 10L)
    area_code_2_through_9 <- grepl("^[2-9]", number_string, perl = TRUE)
    msg <- err_msg(
      "the following numbers do not have an area code starting with 2-9:",
      number_string[! area_code_2_through_9]
    )
    decorated_numbers(number_string, all(area_code_2_through_9), msg)
  }

  # ensure that all exchange codes start with digits 2-9
  check_exchange_code_2_through_9 <- function(number_string) {
    stopifnot(nchar(number_string) == 10L)
    exchange_code_2_through_9 <- grepl("^\\d{3}[2-9]", number_string, perl = TRUE)
    msg <- err_msg(
      "the following numbers do not have an exchange code starting with 2-9:",
      number_string[! exchange_code_2_through_9]
    )
    decorated_numbers(number_string, all(exchange_code_2_through_9), msg)
  }

  decorated_numbers <- function(number_string, is_success, msg) {
    stopifnot(
      is_chr_nomiss(number_string),
      is_truth(is_success),
      is_string(msg)
    )
    list(
      num        = number_string,
      is_success = is_success,
      msg        = msg
    )
  }

  err_msg <- function(msg, entries) {
    paste(
      c(msg, entries),
      collapse = "\n  "
    )
  }

  pipeline_fcn_wrapper <- function(f) {
    stopifnot(is.function(f))
    function(dec_nums) {
      `if`(
        dec_nums$is_success,
        f(dec_nums$num),
        dec_nums
      )
    }
  }

  make_pipeline_fcn <- function(f_list) {
    Reduce(function(f, g) function(x) g(f(x)), f_list, identity)
  }

  stopifnot(
    is.character(number_string),
    ! is.na(number_string)
  )

  # create the pipeline function, which is simply a function that pipes the data
  # through each of the functions in the `pipeline_fcn_list` list from left to
  # right, with the output of one function being provided as the input to the
  # next function
  pipeline_fcn_list <- list(
    strip_non_digit_chars,
    remove_leading_1_from_11_digit_nums,
    check_all_nums_have_10_digits,
    check_area_code_2_through_9,
    check_exchange_code_2_through_9
  )
  wrapped_fcns <- lapply(pipeline_fcn_list, pipeline_fcn_wrapper)
  pipline_fcn <- make_pipeline_fcn(wrapped_fcns)

  start_dec_nums <- decorated_numbers(number_string, TRUE, "")
  output <- pipline_fcn(start_dec_nums)
  `if`(
    output$is_success,
    output$num,
    failure_action(output$msg)
  )
}


is_chr_nomiss <- function(x) {
  is.character(x) && (! any(is.na(x)))
}


is_string <- function(x) {
  is_chr_nomiss(x) && (length(x) == 1L)
}


is_lgl_nomiss <- function(x) {
  is.logical(x) && (! any(is.na(x)))
}


is_truth <- function(x) {
  is_lgl_nomiss(x) && (length(x) == 1L)
}
