#' @export
edit_values <- function(data, ref, row_key, column_key,
                        value){

    # Check for consistency of rows in reclass dictionary
    if (length(setdiff(ref[, row_key], data[, row_key])) > 0) {
        stop("Rows not in dataset include:",
             setdiff(ref[, row_key], data[, row_key]))
    }

    # Check for consistency of columns in reclass dictionary
    cols <- unique(ref[, column_key])
    if (length(setdiff(cols, colnames(data))) > 0) {
        stop("Columns not in dataset include:",
             setdiff(setdiff(cols, colnames(data))))
    }

    for (i in 1:nrow(ref)) {
        row <- ref[i, row_key]
        col <- ref[i, column_key]
        data[data[, row_key] == row, col] <- ref[i, value]
    }

    # return results
    return(data)

}