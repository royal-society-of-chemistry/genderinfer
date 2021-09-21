#' Create a Bullet chart together with a line chart
#' to compare different baseline for gender analysis
#'
#' @name bullet_line_chart
#'
#' @param data_df, dataframe in output from \code{\link{percent_df}}
#' @param baseline_female, numeric vector containing the baseline for each level
#' @param x_title, label for x axis for both charts
#' @param y_bar_chart_title, label for y axis of the bar chart
#' @param baseline_label, label used to define the baseline name.
#' @param line_chart_df, data frame containing the total number of submissions
#' @param line_chart_scaling, factor of conversion for second y-axis
#' @param y_line_chart_title, title the y-axis of the line chart
#' @param line_label, label used to define the line chart.
#' @return The function create a bar chart containing the percentage of male and
#' female and the total number of submissions on the top chart.
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 .pt
#' @export


bullet_line_chart <- function(data_df, baseline_female,
                              x_title, y_bar_chart_title, baseline_label,
                              line_chart_df, line_chart_scaling,
                              y_line_chart_title,
                              line_label) {
  
  Level <- n <- label <- x_values <- y_values <- gender <- pos <- NULL
  lower_CI <- upper_CI <- NULL
  data_df$gender <- factor(gsub("_percentage", "", data_df$gender),
                           levels = c("male", "female"))
  data_df$pos <- ifelse(data_df$gender == "male", 90, data_df$y_values)
  data_df$label <- paste0(data_df$y_values, "")
  data_df$label <- ifelse(data_df$gender == "male", data_df$significance,
                          data_df$label)
  data_df$label <- ifelse(data_df$label == "Significant", "*", data_df$label)
  baseline_df <- data.frame(Level = unique(data_df$x_values),
                            baseline = baseline_female)
  plot <- ggplot() +
    geom_bar(aes(x = Level, y = baseline, fill = "#D7191C"),
             data = {{baseline_df}}, width = 0.7, stat = "identity",
             show.legend = FALSE) +
    geom_bar(aes(x = x_values, y = y_values, fill = gender),
             data = {{data_df}}, stat = "identity", width = 0.4) +
    geom_line(aes(x = x_values, y = n / line_chart_scaling, group = 1,
                  color = line_label), size = 0.15, data = {{line_chart_df}}) +
    scale_y_continuous(name = {{y_bar_chart_title}},
                       sec.axis = sec_axis(~ . * {{line_chart_scaling}},
                                           name = {{y_line_chart_title}})) +
    #xlab({{x_title}}) +
    scale_fill_manual(values = c(alpha("#D7191C", .4), alpha("#512B58", .7),
                                 alpha("#2A7886", 0.7)),
                      label = c({{baseline_label}}, "Female", "Male"),
                      name = "") +
    scale_color_manual(name = NULL, values = "#004976", label = {{line_label}}) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    geom_text(data = {{data_df}}, aes(x = x_values, y = pos,
                                      label = label),
              size = 12 / .pt, vjust = -0.5) +
    geom_errorbar(data = {{data_df}}, aes(x = x_values,
                                          ymin = lower_CI,
                                          ymax = upper_CI), width = 0.3) +
    theme_gd() + xlab({{x_title}}) + ylab({{y_bar_chart_title}})
  plot
}
