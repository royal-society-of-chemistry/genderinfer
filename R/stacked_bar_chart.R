#' Create a stacked bar chart with significance bars to compare with the 
#' baseline for gender analysis
#' @name stacked_bar_chart
#' @param data_df, is the output dataframe from the function percent_df
#' @param baseline_female_percentage, female baseline in percentage
#' @param x_title, label for x axis
#' @param y_title, label for y axis
#' @param baseline_label, label used to define the baseline name.
#' @return This function create a bar chart containing the percentage of 
#' submission with the corresponding baseline.
#' @examples
#' \dontrun{
#' stacked_bar_chart(percent_df, baseline,
#' "Submissions (%)", "Months", "Women baseline")
#' }
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 scale_color_manual
#' @export

stacked_bar_chart <- function(data_df, baseline_female_percentage, x_title, 
                              y_title, baseline_label) {
  data_df <- {{data_df}} %>% 
    mutate(gender = factor(gsub("_percentage", "", .data$gender),
                           levels = c("male", "female")),
           pos = if_else(.data$gender == "male", 90, .data$y_values),
           Labels = paste(.data$y_values, "%"),
           Labels = if_else(.data$gender == "male", .data$significance, 
                            .data$Labels))
  plot <- ggplot() +
    geom_bar(aes(x = .data$x_values, y = .data$y_values, fill = .data$gender), 
             data = data_df, stat = "identity") +
    scale_fill_manual(values = c(alpha("#2A7886", 0.7), alpha("#512B58", .7)),
                    labels = c("Male", "Female"), name = "") +
    scale_x_discrete(limits = rev(levels(droplevels(data_df$x_values)))) +
    geom_text(data = {{data_df}}, aes(x = .data$x_values, y = .data$pos,
                                     label = .data$Labels), 
              size = 18/.pt, vjust = 0, nudge_y = 0.5) +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    geom_line() + 
    geom_hline(aes(yintercept = {{baseline_female_percentage}}, 
                   color = paste({{baseline_label}},
                                 {{baseline_female_percentage}}, "%")))  +
    scale_color_manual(values = alpha("#D7191C", .7), name = "") + 
    geom_errorbar(data = {{data_df}}, aes(x = .data$x_values, 
                                          ymin = .data$lower_CI,
                                          ymax = .data$upper_CI), width = 0.3) +
    xlab({{x_title}}) + ylab({{y_title}}) + theme_gd()
  plot
}