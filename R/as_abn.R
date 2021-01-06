as_abn <- function(x) {
  # Any number of 9 digits or less has the appropriate checksum added

  # Any number of 10 digits or more will result in an error, unless the option truncate = TRUE,
  # in which case only the first 9 digits will be considered

  x <- as.character(x)
  x <- stringr::str_replace_all(x, pattern = "\\s", replacement = "")
  x <- stringr::str_trunc(x, width = 9, side = "left", ellipsis = "")
  x <- stringr::str_pad(x, width = 9, side = "left", pad = "0")

  abn_matrix <- simplify2array(lapply(strsplit(x, "", fixed = TRUE), as.numeric))
  abn_sums <- colSums(abn_matrix * c(3, 5, 7, 9, 11, 13, 15, 17, 19))
  checksums <- 99 - (abn_sums %% 89)
  paste0(checksums, x)
}
