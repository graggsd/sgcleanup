#' Convert values to NA
#'
#' \code{convert_to_NA} finds values specified by \code{from} and sets them
#' to NA.
#' @param x A data frame
#' @param from One or more values that should be converted to NA.
#' @param cols 0 or more column names or positions.
#' @seealso \code{\link[sgcleanup]{convert_from_NA}}
#' @export
convert_to_NA <- function(x, from, cols = NULL) {
    if (is.null(cols)) {
        cols <- colnames(x)
    }
    for (col in cols){

        idx <- which(x[,col] %in% from)
        x[idx, col] <- NA
    }
    return(x)
}

#' Convert values from NA to an alternative value
#'
#' \code{convert_from_NA} finds NA values and changes them to a value specified
#' by \code{to}
#' @param x A data frame
#' @param to Values that NA values should be converted to
#' @param cols 0 or more column names or positions.
#' @seealso \code{\link[sgcleanup]{convert_to_NA}}
#' @export
convert_from_NA <- function(x, to, cols = NULL) {
    if (is.null(cols)) {
        cols <- colnames(x)
    }
    for (col in cols){
        idx <- which(is.na(x[,col]))
        x[idx, col] <- to
    }
    return(x)
}