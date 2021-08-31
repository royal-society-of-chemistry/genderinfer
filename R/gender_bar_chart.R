#' Function to plot the barchart of the total number by gender
#'
#' @name gender_bar_chart
#'
#' @param data_df, dataframe generated from the function total_df
#' @param x_title, title for x axis.
#' @param y_title, title for y axis
#' @param label_name, label for the total number of male and female
#' @returns a barchart
#' @examples
#' \dontrun{
#' totalbarchart <- gender_bar_chart(data_df)
#' }
#' @importFrom ggplot2 theme_bw
#' @export


gender_bar_chart <- function(data_df, x_title, y_title, label_name) {

  ## define global variable to NULL
  x_values <- y_values <- gender <- total_female_male <- NULL
  plot <- ggplot() +
    geom_bar(aes(x = x_values, y = y_values, fill = gender),
             data = {{data_df}},
             stat = "identity", position = "dodge") +
    scale_fill_manual(values = c(alpha("#DCDCDC", .7), alpha("#2A7886", 0.7),
                               alpha("#512B58", .7)),
                      breaks = c("unknown", "male", "female")) +
    scale_x_discrete(limits = rev(levels(droplevels(data_df$x_values)))) +
    geom_text(data = data_df, aes(x = x_values,
                                  y = total_female_male,
                                  label = paste({{label_name}},
                                                total_female_male, sep = " "),
                                  size = 13 / .pt), show.legend = FALSE) +
  xlab({{x_title}}) + ylab({{y_title}}) + theme_gd()
  plot
}
