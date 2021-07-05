#' Function to create the balloon plot for gender first name
#' @param data_df, data frame containing the submission's data
#' @param first_name, first name column's name
#' @param gender, gender column name
#' @examples  
#' \dontrun{
#' authors_df %>% balloon_plot(first_name = first_name, gender) 
#' }
#' @importFrom stringr str_count
#' @importFrom dplyr top_n
#' @importFrom ggpubr ggballoonplot
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom ggpubr text_grob
#' @importFrom ggpubr ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom magrittr set_rownames
#' @export

balloon_plot <- function(data_df, first_name, gender) {
## calculate top male and top female author name
## Initial submission
authors_name <- {{data_df}} %>%
  filter(str_count({{first_name}}) > 1) %>%
  count({{first_name}}, {{gender}}) %>% 
  arrange(desc(.data$n))

Male <- as.data.frame(authors_name) %>% 
  filter({{gender}} == "M") %>%
  top_n(20) %>% 
  select(-{{gender}})

## set the first column with the first name as rownames.
Male <- set_rownames(Male, Male[, 1])

top_male <- Male %>% 
  select(-{{first_name}}) %>%
  ggballoonplot(fill = "value", show.label = TRUE,
                font.label = list(size = 14, style = "bold", color = "black"),
                na.value = "grey50",
                ggtheme = theme_minimal(),
                legend.title = "Number of books",
                font.legend = 10) +
  scale_fill_viridis_c(alpha = 0.2, option = "C")  +
  guides(size = "none") + xlab("Number of articles") +
  ylab("") + ggtitle("Male") +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x  = element_text(size = 16),
        axis.title.y = element_text(size = 20, angle = 90),
        axis.text.y  = element_text(size = 16),
        legend.title = element_text(size = 12))

Female <- as.data.frame(authors_name) %>% 
  filter({{gender}} == "F") %>%
  top_n(20) %>% 
  select(-{{gender}}) 
Female <- set_rownames(Female, Female[, 1])


top_female <- Female %>%
  select(-{{first_name}}) %>%
  ggballoonplot(fill = "value", show.label = TRUE,
                font.label = list(size = 14, style = "bold", color = "black"),
                na.value = "grey50",
                ggtheme = theme_minimal(),
                legend.title = "Number of articles",
                font.legend = 10) +
  scale_fill_viridis_c(alpha = 0.2, option = "C")  +
  guides(size = "none") + xlab("") +
  ylab("") + ggtitle("Female") +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x  = element_text(size = 16),
        axis.title.y = element_text(size = 20, angle = 90),
        axis.text.y  = element_text(size = 16),
        legend.title = element_text(size = 12))

Unknown <- as.data.frame(authors_name) %>% 
  filter({{gender}} == "U") %>%
  top_n(20) %>% 
  select(-{{gender}}) 
Unknown <- set_rownames(Unknown, Unknown[, 1])


top_unknown <- Unknown %>%
  select(-{{first_name}}) %>%
  ggballoonplot(fill = "value", show.label = TRUE,
                font.label = list(size = 14, style = "bold", color = "black"),
                na.value = "grey50",
                ggtheme = theme_minimal(),
                legend.title = "Number of articles",
                font.legend = 10) +
  scale_fill_viridis_c(alpha = 0.2, option = "C")  +
  guides(size = "none") + xlab("") +
  ylab("") + ggtitle("Unknown") +
  theme(axis.title.x = element_text(size = 20),
        axis.text.x  = element_text(size = 16),
        axis.title.y = element_text(size = 20, angle = 90),
        axis.text.y  = element_text(size = 16),
        legend.title = element_text(size = 12))

figure <- ggarrange(top_female, top_male, top_unknown,
                    ncol = 3, legend = FALSE)
}