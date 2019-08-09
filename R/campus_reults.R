#' An internal function used to generate the QEP master report.
#'
#' @param data The internal master grade dataframe for the report.
#' @param location The campus for which the plot is generated.
#' @export campus_results


campus_results <- function(data,location){
  cat(sprintf("### Results for "),location,".  ")
  data <-  data %>%
    filter(Campus == location)
  cat(sprint("   "))
  plot_campus_results(data,location)
}
