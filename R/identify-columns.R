
#' @export
idx_value_col <- function(df, to_search) {
    return(which(sapply(df, function(x){sum(to_search %in% x,
                                            na.rm = TRUE) > 0})))
}

#' @export
idx_regex_col <- function(df, to_search) {
    to_search <- paste(to_search, collapse = "|")
    return(which(sapply(df, function(x){sum(grepl(to_search, x),
                                            na.rm = TRUE) > 0})))
}

#' @export
idx_logical_col <- function(df, match_type = "value"){
    to_search = c("TRUE", "FALSE", "T", "F", "True", "False", "false", "true")
    if (match_type == "value") {
        return(idx_value_col(df = df, to_search = to_search))
    } else if (match_type == "regex") {
        return(idx_regex_col(df = df, to_search = to_search))
    }
}
