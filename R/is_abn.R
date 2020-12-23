is_abn <- function(abns) {

  if (is.list(abns)) {
    abns <- unlist(abns)
  }

  if (!is.character(abns)) {
    abns <- as.character(abns)
  }

  abns <- gsub(" ", "", abns, fixed = TRUE)
  abns[nchar(abns, keepNA = FALSE) != 11] <- "00000000000"
  abn_matrix <- simplify2array(lapply(strsplit(abns, "", fixed = TRUE), as.numeric))
  abn_sums <- colSums(abn_matrix * c(10, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19))
  checksums <- (abn_sums - 10) %% 89
  checksums == 0
}
