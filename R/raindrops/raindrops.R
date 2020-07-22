raindrops <- function(number) {

  stopifnot(is.numeric(number))

  fac_str_pairs <- data.frame(
    fac              = c(3, 5, 7),
    str              = c("Pling", "Plang", "Plong"),
    stringsAsFactors = FALSE
  )

  is_fac <- ((number %% fac_str_pairs$fac) == 0)
  `if`(
    any(is_fac),
    paste(ifelse(is_fac, fac_str_pairs$str, ""), collapse = ""),
    as.character(number)
  )
}
