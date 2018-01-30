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
        select(cols) %>%
        mutate(idx = 1:nrow(dat)) %>%
        gather_("column", "value", cols) %>%
        group_by(column, value) %>%
        summarise(count = n()) %>%
        spread(value, count, fill = 0) %>%
        as.data.frame(stringsAsFactors = FALSE)
}

