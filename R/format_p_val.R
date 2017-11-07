#' Format p-value with a specified significant digit cutoff
#'
#' Takes a set of p-values, rounds to the specified
#' significant digits, and convert values that are less than the specified
#' cutoff to "<'cutoff'". Maintains consistent numbers of significant digits.
#'
#' @param x A vector of p-values
#' @param digits how many significant digits are to be used.
#' @param cutoff_action Determines the behavior of the function for values that
#' are less than the specified number of digits. If set to \code{"inequality"},
#' then the function replaces these values with \code{"<cutoff"}. If set to
#' \code{"sci"}, then the values are converted to scientific notation.
#' @return A vector of formatted p-values.
#' @export
format_p_val <- function(x, digits = 3, cutoff_action = "inequality") {
    if (is.character(x)) {
        x <- as.numeric(x)
    }
    cutoff <- 10^(-digits)

    idx1 <- which(x < cutoff)
    if (cutoff_action == "inequality") {
        less <- paste0("<", cutoff)
    } else if(cutoff_action == "sci") {
        less <- format(x[idx1], digits = digits)
    }

    idx2 <- which(x >= cutoff)
    greater <- format(round(x[idx2], digits = digits), nsmall = digits)

    x[idx1] <- less
    x[idx2] <- greater

    return(x)
}