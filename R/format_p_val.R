#' Format p-value with a specified significant digit cutoff
#'
#' \code{format_p_value} will take a set of p-values, round to the specified
#' significant digits, and convert values that are less than the specified
#' cutoff to "<'cutoff'"
#'
#' @param x A vector of p-values
#' @param digits how many significant digits are to be used.
#' @return A vector of formatted p-values.
#' @export
format_p_val <- function(x, digits = 3) {
    if (is.character(x)) {
        x <- as.numeric(x)
    }
    cutoff <- 10^(-digits)
    idx <- which(x < cutoff)
    x <- as.character(round(x, digits))
    x[idx] <- paste0("<", cutoff)
    return(x)
}