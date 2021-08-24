#' Create a bullet chart with significance bars to compare different baselines
#' in percentage for gender analysis
#'
#' @name bullet_chart
#' @param data_df, dataframe in output from the function \code{\link{percent_df}}
#' @param baseline_female, numeric vector containing the baseline for each level.
#' @param x_title, label for x axis
#' @param y_title, label for y axis
#' @param baseline_label, label used to define the baseline name.
#' @return This function create a bar chart containing the percentage of 
#' submission with the corresponding baseline for a years period.
#' @examples
#' \dontrun{
#' bullet_chart(percent_df, baseline_female, "Submissions (%)", "Months",
#' "Women baseline")
#' }
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 alpha
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @export


bullet_chart <- function(data_df, baseline_female, x_title, y_title, 
                                    baseline_label) {
  ## create the dataframe for generating the bullet chart.
  data_df$gender = factor(gsub("_percentage", "", data_df$gender),
                                 levels = c("male", "female"))
  data_df$pos = if_else(data_df$gender == "male", 90, .data$y_values)
  data_df$labels = paste(data_df$y_values, "%")
  data_df$labels = if_else(data_df$gender == "male", data_df$significance,
                            data_df$labels)
  baseline_df <- data.frame(level = unique(data_df$x_values),
                            baseline = baseline_female)
  ## Create the bullet chart
  plot <- ggplot() +
    geom_bar(aes(x = baseline_df$level, y = baseline_df$baseline, fill = "#D7191C"), 
             data = {{baseline_df}}, width = 0.85, stat = "identity",
             show.legend = FALSE) +
    geom_bar(aes(x = .data$x_values, y = .data$y_values, fill = .data$gender),
             data = {{data_df}}, stat = "identity", width = 0.4) +
    scale_fill_manual(values = c(alpha("#D7191C", .4), alpha("#512B58", .7), 
                               alpha("#2A7886", 0.7)),
                      labels = c({{baseline_label}}, "Female", "Male"),
                      name = "") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    scale_x_discrete(limits = rev(levels(droplevels(data_df$x_values)))) +
    geom_text(data = {{data_df}}, aes(x = .data$x_values, y = .data$pos,
                                      label = .data$labels),
              size = 15 / .pt, vjust = -0.2) +
    geom_errorbar(data = {{data_df}}, aes(x = .data$x_values, 
                                          ymin = .data$lower_CI, 
                                          ymax = .data$upper_CI), width = 0.3) +
    theme_gd() + xlab({{x_title}}) + ylab({{y_title}}) + coord_flip()
  plot 
}