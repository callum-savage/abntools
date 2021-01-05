#' Use a checksum to identify if a given input is in the format of an ACN
#'
#' This function identifies if an input is in the format of a valid ACN using
#' the rules oultined [here](https://asic.gov.au/for-business/registering-a-company/steps-to-register-a-company/australian-company-numbers/australian-company-number-digit-check/). Note
#' that this function does not confirm that the ACN is valid or active; it
#' simply checks if it *could* be valid.
#'
#' @param x A character vector of ACNs to check
#'
#' @return A logical vector of the same length as \code{x} identifying if each
#'   element is in the format of a valid ACN
#' @export
#'
#' @examples
#' # Single ACN
#' is_acn("110219460")
#'
#' # Character vector which may include arbitrary white space
#' is_acn(c("001 250 004", "001 999 999"))
is_acn <- function(x) {

  # TODO add warning for non-character vectors

  x <- gsub("\\s", "", x)
  x[nchar(x, keepNA = FALSE) != 9] <- "000000001"
  acn_matrix <- simplify2array(lapply(strsplit(x, "", fixed = TRUE), as.numeric))
  acn_sums <- colSums(acn_matrix * c(8, 7, 6, 5, 4, 3, 2, 1, 1))
  checksums <- (acn_sums %% 10)
  checksums == 0
}
