spread_multi_values <- function(data, key, values) {

    if (length(setdiff(c(key, values), colnames(data))) != 0) {
        stop("One or more specified columns are not in colnames(data).")
    }

    # To transform columns into appropriate classes later
    value_classes <-
        sapply(values, function(x) {data %>% pull(x) %>% class})

    # To form a name for combined columns
    comb_val_name <- paste(values, collapse = "_")

    # To iterate over columns later
    unique_keys <- unique(data %>% pull(key))

    # To facilitate spreading one column
    out <- data %>%
        ungroup() %>%
        unite_(col = comb_val_name,
               from = values,
               sep = "_")

    out <- out %>%
        spread_(key_col = key,
                value_col = comb_val_name)

    # To separate combined column values
    for(col in unique_keys[unique_keys %in% colnames(out)]) {
        out <- out %>%
            separate_(col = col, into = paste0(col, ".", values), sep = "_")
    }

    # To change resulting columns back into originating class
    for (i in 1:length(value_classes)) {
        regex <- paste0("\\.", names(value_classes)[i], "$")
        for (col in unique(grep(regex, colnames(out), value = TRUE))) {
            tmp_col <- out %>% pull(col)
            class(tmp_col) <- value_classes[i]
            out[,col] <- tmp_col
        }
    }

    return(out)

}

#' Spread key-value pairs composed of multiple keys and values
#'
#' \code{spread_sg} allows the use of the \code{spread} function from
#' \code{tidyr} over multiple keys and values. Essentially, keys and values
#' will be combined into one key and value, then separated in the final data
#' frame as appropriate.
#' @param data A data frame
#' @param keys One or more column names.
#' @param values One or more column names.
#' @seealso \code{\link[tidyr]{spread}}
#' @export
spread_sg <- function(data, keys, values) {

    if (length(setdiff(c(keys, values), colnames(data))) != 0) {
        stop("One or more specified columns are not in colnames(data).")
    }

    # Unify the keys into one column if more than one key exists
    if (length(keys) > 1) {
        key <- paste(keys, collapse = ".")
        data <- data %>%
            unite_(col = key, from = keys, sep = ".")

    } else {
        key <- keys
    }

    # Use spread_multi_values if there is more than one value
    if (length(values) > 1) {
        data <- data %>%
            spread_multi_values(key = key, values = values)
    } else {
        data <- data %>%
            spread_(key_col = key, value_col = values)
    }

    return(data)

}