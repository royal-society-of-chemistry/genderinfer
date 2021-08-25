#' Create a dataframe that will be the input to generate the bar chart of 
#' the full amount of female and male
#'
#' @name gender_total_df
#' @param data_df, dataframe containing Level, LCI, UCI, 
#' Significance and Male and Female percentages
#' @param level, name of level 
#' @returns dataframe with the columns x_values, total_female_male, gender,
#'  y_values,
#' @examples
#' \dontrun{
#' df <- gender_total_df(authors)
#' }
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr rename
#' @export


gender_total_df <- function(data_df, level) {
  x_values <- y_values <- gender <- total_female_male <- NULL
  long <- reshape(data_df, direction = "long", v.names = "y_values",
                  varying = 2:4, times = names(data_df)[2:4],
                  timevar = "gender",idvar = level)
  names(long)[names(long) == level] <- "x_values"
  long <- subset(long, select = c(x_values, total_female_male, gender,
                                  y_values))
  long <- long[order(rev(long$gender)), ]
  long
  # totalplotdf <- {{data_df}} %>% pivot_longer(cols = c(.data$male, .data$female,
  #                                                     .data$unknown),
  #                                           names_to = "gender",
  #                                           values_to = "y_values") %>%
  #   mutate(x_values = {{level}}) %>%
  #   arrange(desc(.data$gender)) %>%
  #   select(.data$x_values, .data$total_female_male, .data$gender,
  #          .data$y_values) %>%
  #   mutate(gender = factor(.data$gender, 
  #                          levels = c("unknown", "male", "female")))

}
