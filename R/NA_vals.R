#' Convert values to NA
#'
#' \code{convert_to_NA} finds values sepcified by \code{na_vals} and sets them
#' to NA.
#' @param data A data frame
#' @param keys One or more values that should be converted to NA.
#' @param values 0 or more column names or positions.
#' @seealso \code{\link[tidyr]{spread}}
#' @export
convert_to_NA <- function(x, na_vals, cols = NULL) {

    if (is.null(cols)) {
        cols <- colnames(x)
    }
    for (col in cols){

        idx <- which(x[,col] %in% na_vals)
        x[idx, col] <- NA
    }

    return(x)

}