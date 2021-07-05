#' Create a Bullet chart together with a line chart
#' to compare different baseline for gender analysis
#'
#' @name bullet_line_chart
#'
#' @param data_df, dataframe in output from the function \code{\link{percent_df}}
#' @param baseline_female, numeric vector containing the baseline for each level.
#' @param x_title, label for x axis
#' @param y_title, label for y axis
#' @param baseline_label, label used to define the baseline name.
#' @param total_number_df, data frame containing the total number of submissions.
#' @param var_name, variable from total_number_df to plot.
#' @param c, factor of conversion for second y-axis
#' @param ysectitle, title of the second y-axis
#' @return This function create a bar chart containing the percentage of submission
#' @param line_label, label used to define the line chart.
#' with the corresponding baseline for a years period.
#' @examples
#' \dontrun{
#' bullet_line_chart(percent_df, baseline, "Submissions (%)", "Months",
#'  "Women baseline", total_number_df, var_name, c, ysectitle, line_label)
#' }
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 .pt
#' @export


bullet_line_chart <- function(data_df, baseline_female,
                              x_title, y_title, baseline_label,
                              total_number_df, var_name, c, ysectitle,
                              line_label) {

  data_df <- {{data_df}} %>% 
    mutate(gender = factor(gsub("_percentage", "", .data$gender),
                           levels = c("male", "female")),
           pos = if_else(.data$gender == "male", 90, .data$y_values),
           label = paste0(.data$y_values, ""),
           label = if_else(.data$gender == "male", .data$significance,
                            .data$label),
           label = if_else(.data$label == "Significant", "*", .data$label))
  baseline_df <- data.frame(Level = unique(data_df$x_values),
                            baseline = baseline_female)
  plot <- ggplot() +
    geom_bar(aes(x = .data$Level, y = baseline, fill = "#D7191C"), 
             data = {{baseline_df}}, width = 0.85, stat = "identity",
             show.legend = FALSE) +
    geom_bar(aes(x = .data$x_values, y = .data$y_values, fill = .data$gender), 
             data = {{data_df}}, stat = "identity", width = 0.4) +
    geom_line(aes(x = {{var_name}}, y = .data$n / {{c}}, group = 1, 
                  color = line_label), data = {{total_number_df}}) +
    scale_y_continuous(name = {{y_title}}, 
                       sec.axis = sec_axis(~ .*{{c}},
                                           name = {{ysectitle}})) + 
    #xlab({{x_title}}) +
    scale_fill_manual(values = c(alpha("#D7191C", .4), alpha("#512B58", .7), 
                               alpha("#2A7886", 0.7)),
                      label = c({{baseline_label}}, "Female", "Male"),
                      name = "") +
    scale_color_manual(name = NULL, values = "black", label = {{line_label}}) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    geom_text(data = {{data_df}}, aes(x = .data$x_values, y = .data$pos, 
                                     label = .data$label), 
              size = 12 / .pt, vjust = -0.5) +
    geom_errorbar(data = {{data_df}}, aes(x = .data$x_values, 
                                          ymin = .data$lower_CI, 
                                          ymax = .data$upper_CI), width = 0.3) + 
    theme_gd() + xlab({{x_title}}) + ylab({{y_title}}) 
  plot 
}