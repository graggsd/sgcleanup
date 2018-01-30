#' @export
wd_histograms <- function(data, variables, log.scale = T) {

    tmp.data <- data[,variables] %>%
        gather(key = variable, value = num.val) %>%
        na.omit()

    figure <- tmp.data %>%
        ggplot(aes(x=num.val)) +
        geom_histogram(fill="red") +
        facet_wrap(~variable,
                   scales = "free")

    if(log.scale) {

        figure <- figure +
            scale_y_log10()
    }

    figure <- figure +
        ggthemes::theme_base() +
        labs(x = "N",
             y = "Value")

    return(figure)
}

#' @export
wd_qnorm <- function(data, variables) {

    tmp.data <- data[,variables] %>%
        gather(key = variable, value = num.val) %>%
        na.omit()

    summary.data <- tmp.data %>%
        group_by(variable) %>%
        summarize(q25 = quantile(num.val, 0.25),
                  q75 = quantile(num.val, 0.75),
                  norm25 = qnorm(0.25),
                  norm75 = qnorm(0.75),
                  slope = (q25 - q75)/(norm25 - norm75),
                  int = q25 - slope * norm25) %>%
        select(variable, slope, int)


    figure <- tmp.data %>%
        ggplot(aes(sample=num.val)) +
        stat_qq(distribution=qnorm, alpha=0.2) +
        geom_abline(data=summary.data,
                    aes(intercept=int, slope=slope),
                    col="red",
                    linetype = 2) +
        facet_wrap(~variable, scales = "free_y") +
        ggthemes::theme_base() +
        labs(x = "Theoretical (Z)",
             y = "Observed (Value)")

    return(figure)
}
