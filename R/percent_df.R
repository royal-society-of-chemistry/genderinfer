#' Create a dataframe that will be the input to generate stacked bar chart and
#' bullet chart that show percentage to compare proportions among gender.
#'
#' @name percent_df
#' @param data_df, dataframe containing level, lower_CI, upper_CI, 
#' significance and female and male percentages
#' @returns dataframe with the columns x_values, y_values, gender, labels
#' @examples
#' \dontrun{
#' percent_df <-percent_df(data_dfF)
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @export

percent_df <- function(data_df) {
  percentdf <- {{data_df}} %>% 
    pivot_longer(cols = c(.data$female_percentage, .data$male_percentage),
                 names_to = "gender", values_to = "y_values") %>%
    arrange(desc(.data$gender)) %>% 
    rename(x_values = .data$level) %>%
    mutate(labels = "") %>% 
    select(.data$x_values, .data$y_values, .data$lower_CI, .data$upper_CI,
           .data$significance, .data$labels, .data$gender)
  percentdf
}