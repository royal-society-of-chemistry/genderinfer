#' Assign gender by first name
#'
#' @name assign_gender
#' @description This function use the data source based on combined US/UK
#' censor data to assign gender based on first name.
#' @param data_df, input dataframe containing the first name
#' @param first_name_col, the column name containing first names to assign gender 
#' to
#' @return the input data frame with the gender column:
#'
#'          gender - assigned gender (F/M/U)
#' @examples
#' gender <- assign_gender(authors, first_name)
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr if_else
#' @importFrom tidyr replace_na
#'
#' @export

assign_gender <- function(data_df, first_name_col) {

  ## read the data frame and convert special character to ASCII to better assign 
  ## the gender. The first name need to be without accent or special character,
  ## this is why we use here the function iconv.
  df1 <- data_df %>% 
    mutate(FNAME = tolower(iconv({{first_name_col}}, from = "UTF-8", 
                                 to = "ASCII//TRANSLIT"))) %>%
    left_join(gender_names, by = c("FNAME" = "Name")) %>% 
    mutate(UKUS_Gender = replace_na(.data$UKUS_Gender, "U")) %>%
    mutate(gender = if_else(.data$UKUS_Gender == "Male", "M",
                            if_else(.data$UKUS_Gender == "Female", "F",
                                    "U"))) %>%
    select(-.data$UKUS_Gender, -.data$FNAME)
  df1
}
