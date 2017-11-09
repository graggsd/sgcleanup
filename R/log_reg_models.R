#' @export
get_first_class <- function(x, ...) {
    UseMethod("get_first_class", x)
}

get_first_class.list <- function(x, ...) {
    return(sapply(x, function(x) {class(x)[1]}))
}

get_first_class.default <- function(x, ...) {
    return(class(x)[1])
}