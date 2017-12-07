#' @export
wd_histograms <- function(data, variables, log.scale = T) {

    tmp.data <- data[,variables] %>%
        gather(key = variable, value = num.val) %>%
        na.omit()

    figure <- tmp.data %>%
        ggplot(aes(x=num.val)) +
        geom_histogram(fill="red") +
        facet_wrap(~variable)

    if(log.scale) {

        figure <- figure +
            scale_y_log10()
    }
    return(figure)
}