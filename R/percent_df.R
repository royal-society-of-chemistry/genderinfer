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
#' @export

percent_df <- function(data_df) {
  
  x_values <- y_values <- gender <- lower_CI <- upper_CI <- NULL
  significance <- labels <- gender <- NULL
  long <- reshape(data_df, direction = "long", v.names = "y_values", 
                  timevar = "gender", idvar = "level", varying = 7:8,
                  times = names(data_df)[7:8])
  names(long)[names(long) == "level"] <- "x_values"
  long$label <= ""
  long <- subset(long, select = c(x_values, y_values, lower_CI, upper_CI,
                                      significance, labels, gender))
  long <- long[order(rev(long$gender)), ]
  long
}