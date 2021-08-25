#' Function to create the balloon plot for gender first name
#' @param data_df, data frame containing the submission's data
#' @param first_name, first name column's name
#' @param gender, gender column name
#' @examples  
#' \dontrun{
#' authors_df %>% balloon_plot(first_name = first_name, gender) 
#' }
#' @importFrom stats reorder
#' @importFrom ggplot2 scale_alpha_continuous
#' @importFrom ggplot2 scale_size_area
#' @importFrom ggplot2 unit
#' @export

balloon_plot <- function(data_df, first_name, gender) {
  
  n <- NULL
  df <- subset(data_df, length(data_df$first_name) >1 & gender == gender)
  df <- as.data.frame(table(df[, c("first_name")]))
  df <- df[order(df$Freq, decreasing = TRUE),]
  df$row <- sort(order(df$Freq, decreasing = TRUE), decreasing = FALSE)
  df$col <- 1
  names(df)[names(df) == "Var1"] <- "first_name"
  names(df)[names(df) == "Freq"] <- "n"
  
  gg_m <- ggplot(df, aes(x = factor(col), y = reorder(factor(first_name), n), 
                         size = n, colour = n,
                         alpha = n)) +
    geom_point() +
    geom_text(aes(label = n, x = col + 0.1), alpha = 1.0, size = 3) + # display the value next to the "balloons"
    scale_alpha_continuous(range=c(0.3, 0.7)) +
    scale_size_area(max_size = 6) +              # set the labels on the Y axis
    theme_bw() +
    theme(axis.line = element_blank(),            # disable axis lines
          axis.title = element_blank(),           # disable axis titles
          panel.border = element_blank(),         # disable panel border
          panel.grid.major.x = element_blank(),   # disable lines in grid on X-axis
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = unit(c(0,2,0,2),"cm"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y  = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          text = element_text(color = "navy"),
          panel.background = element_rect(fill = "white"),
          legend.position = "none") +
    ylab("First name") + xlab("")
  gg_m
}