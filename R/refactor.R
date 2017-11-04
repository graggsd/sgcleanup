#' Create a factor based on specified level:index pairs
#'
#' \code{refactor_by_idx} changes \code{x} to a factor, and specifies level
#' order based the 1:1 pairing of \code{levs} and \code{idx}
#'
#' @param x A vector that will be converted to a factor based with level order
#' specified by \code{levs}:\code{idx} pairing
#' \code{from}.
#' @param levs The levels contained in \code{x}
#' @param idx The order in which \code{levs} will be incorporated into the
#' new factor
#' @return A factor with levels ordered according to \code{levs}:\code{idx}
#' pairing
#' @export
refactor_by_idx <- function(x, levs, idx) {

    stopifnot(length(levs) == length(idx),
              sum(duplicated(levs)) == 0,
              sum(duplicated(idx)) == 0)

    idx <- order(idx)
    x <- factor(x, levels = levs[idx])
    return(x)
}