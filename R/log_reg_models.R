
#' get_first_class <- function(x, ...) {
#'     UseMethod("get_first_class", x)
#' }
#'
#' get_first_class.list <- function(x, ...) {
#'     return(sapply(x, function(x) {class(x)[1]}))
#' }
#'
#' get_first_class.default <- function(x, ...) {
#'     return(class(x)[1])
#' }
#'

#' Adds an odds Ratio and 95% confidence interval
#'
#' @export
add_OR_bounds <- function(x, ...) {
    UseMethod("add_OR_bounds", x)
}

add_OR_bounds.data.frame <- function(x, ...) {
    stopifnot("estimate" %in% colnames(x),
              "std.error" %in% colnames(x))
    x$OR <- exp(x$estimate)
    x$Lower <- exp(x$estimate - 1.96*x$std.error)
    x$Upper <- exp(x$estimate + 1.96*x$std.error)
    return(x)
}