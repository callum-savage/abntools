is_acn <- function(x) {

  if (is.list(x)) {
    acns <- unlist(x)
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  x <- gsub("\\s", "", x)
  x[nchar(x, keepNA = FALSE) != 9] <- "000000001"
  acn_matrix <- simplify2array(lapply(strsplit(x, "", fixed = TRUE), as.numeric))
  acn_sums <- colSums(acn_matrix * c(8, 7, 6, 5, 4, 3, 2, 1, 1))
  checksums <- (acn_sums %% 10)
  checksums == 0
}
