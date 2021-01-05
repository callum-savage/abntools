#' Use a checksum to identify if a given input is in the format of an ABN.
#'
#' This function identifies if an input is in the format of a valid ABN using
#' the rules oultined [here](https://abr.business.gov.au/Help/AbnFormat). Note
#' that this function does not confirm that the ABN is valid or active; it
#' simply checks if it *could* be valid.
#'
#' @param x A numeric or character vector of ABNs to check
#'
#' @return A logical vector of the same length as \code{x} identifying if each
#'   element is in the format of a valid ABN
#' @export
#'
#' @examples
#' # Single ABN
#' is_abn(19621994018)
#'
#' # Numeric vector
#' is_abn(c(19621994018, 81601798609))
#'
#' # Character vector which may include arbitrary white space
#' is_abn(c("50 110 219 460", "79 769 424 861"))
is_abn <- function(x) {

  if (!is.character(x)) {
    x <- as.character(x)
  }

  x <- gsub("\\s", "", x)
  x[nchar(x, keepNA = FALSE) != 11] <- "00000000000"
  abn_matrix <- simplify2array(lapply(strsplit(x, "", fixed = TRUE), as.numeric))
  abn_sums <- colSums(abn_matrix * c(10, 1, 3, 5, 7, 9, 11, 13, 15, 17, 19))
  checksums <- (abn_sums - 10) %% 89
  checksums == 0
}
