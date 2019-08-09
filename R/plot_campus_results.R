#' An internal function used to generate the QEP master report.
#'
#' @param data The internal master grade dataframe for the report.
#' @param location The campus for which the plot is generated.
#' @return campus_plot A ggplot object.
#' @export plot_campus_results

plot_campus_results <- function(data,location){
  plot_data <- data %>%
    group_by(termcode_new,Campus) %>%
    summarize(size=n(),pro_suc=sum(Success=="Successful")/size)
  campus_plot <- ggplot(plot_data, mapping=aes(x=termcode_new,y=pro_suc,group=1)) +
    geom_line() +
    geom_point() +
    ylim(0, 1) +
    ggtitle(paste("Proportion Successful by Term at ",location)) +
    xlab("Terms") +
    ylab("Proportion of Students Successful")
}
