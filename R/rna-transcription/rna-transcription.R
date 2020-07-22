# takes a character vector such that each element is a string comprised of only
# the letters A, C, G, and T, and returns a character vector such that the
# letters A, C, G, and T are replaced by the letters U, G, C, and A
to_rna <- function(dna) {

  transcription <- function(chars, char_map) {
    stopifnot(chars %in% names(char_map))
    paste0(char_map[chars], collapse = "")
  }

  stopifnot(
    is.character(dna),
    ! is.na(dna),
    ! grepl("[^ACGT]", dna, perl = TRUE)
  )

  char_map <- c(
    G = "C",
    C = "G",
    T = "A",
    A = "U"
  )

  char_list <- strsplit(dna, "")
  vapply(
    X         = char_list,
    FUN       = transcription,
    FUN.VALUE = character(1L),
    char_map  = char_map
  )
}
