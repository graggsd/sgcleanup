#' Tabulate the columns of wide data
#'
#' Counts unique values of specified columns and returns these counts as a
#' data.frame
#' @param dat A data.frame
#' @param cols Columns to tabulate
#' @return A data.frame with original \code{cols} as rows and columns containing
#' the counts of individual values
#' @export
tabulate_wd <- function(dat, cols) {
    dat %>%
        dplyr::select(cols) %>%
        dplyr::mutate(idx = 1:nrow(dat)) %>%
        tidyr::gather("column", "value", cols) %>%
        dplyr::group_by(column, value) %>%
        dplyr::summarise(count = n()) %>%
        tidyr::spread(value, count, fill = 0) %>%
        as.data.frame(stringsAsFactors = FALSE)
}

