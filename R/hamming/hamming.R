hamming <- function(strand1, strand2) {
  stopifnot(
    is.character(strand1),
    ! is.na(strand1),
    is.character(strand2),
    ! is.na(strand2),
    nchar(strand1) == nchar(strand2)
  )
  by_char_list <- strsplit(c(strand1, strand2), "")
  equal_chars_list <- Map(`!=`, by_char_list[[1L]], by_char_list[[2L]])
  `if`(
    is.null(equal_chars_list),
    0L,
    sum(unlist(equal_chars_list))
  )
}
