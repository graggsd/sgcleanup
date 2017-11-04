#' Substitute exact matches within a vector with another value
#'
#' \code{sub_value} finds exact matches within \code{x} specified by
#' \code{from} and substitutes them with values specified in \code{to}. Values
#' specified by \code{from} will be replaced in a 1:1 or many:1 fashion. Only
#' values within \code{from} that are contained in \code{x} will be replaced.
#'
#' @param x A vector of values, some (or all) of which are equivalent to values
#' in \code{from} and will be replaced by values in \code{to}
#' @param from A vector of values potentially contained within \code{x} that are
#' to be substituted
#' @param to The corresponding value (or values) that \code{from} will be
#' converted to
#' @return A new vector, derived from \code{x}, with 0 or more values
#' substituted for new ones
#' @export
sub_value <- function(x, from, to) {

    stopifnot(length(from) == length(to) |
                  length(to) == 1)

    idx <- which(from %in% x)
    from <- from[idx]

    if (length(to) > 1) {
        to <- to[idx]
        for (i in 1:length(from)) {
            x[x == from[i]] <- to[i]
        }
    } else {
        for (i in 1:length(from)) {
            x[x == from[i]] <- to
        }
    }
    return(x)
}

#' Substitute regular expressions within a vector with another string
#'
#' \code{sub_value} finds regular expressions within \code{x} specified by
#' \code{from} and substitutes them with strings specified in \code{to}. Regexs
#' specified by \code{from} will be replaced in a 1:1 or many:1 fashion. Only
#' regexs within \code{from} that are contained in \code{x} will be replaced.
#'
#' @param x A character vector, that will be searched for a collection of
#' regular expressions (specified \code{to}) replaced by strings specified in
#' \code{from}.
#' @param from A vector of regexs potentially contained within \code{x} that are
#' to be substituted
#' @param to The corresponding strings that will be used to replace
#' elemnts of \code{from}.
#' converted to
#' @return A character vector, derived from \code{x}, will have 0 or more
#' elements altered based on the specified \code{from}:\code{to} mapping
#' @export
sub_regex <- function(x, from, to){

    stopifnot(length(from) == length(to) |
                  length(to) == 1)

    if (length(to) > 1) {
        for(i in 1:length(from)) {
            x <- gsub(from[i], to[i], x)
        }
    } else {
        for(i in 1:length(from)) {
            x <- gsub(from[i], to, x)
        }
    }
    return(x)
}