
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
