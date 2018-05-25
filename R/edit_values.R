#' @export
edit_values <- function(data,
                        ref,
                        row_key,
                        column_key,
                        value){

    # Check that row_key is contained in both data and ref
    stopifnot(row_key %in% colnames(data),
              row_key %in% colnames(ref))

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

    # For each row in the reference dictionary, find the value in the
    # row_key column and the value in the column_key column
    # Use the value in the column key to select the appropriate column in
    # data and (assuming the row_key column name is the same for both
    # the reference and the dataset are the same - verified above)
    for (i in 1:nrow(ref)) {
        row <- ref[i, row_key]
        col <- ref[i, column_key]
        data[data[, row_key] == row, col] <- ref[i, value]
    }

    # return results
    return(data)

}