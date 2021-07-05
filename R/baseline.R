#' Calculate the female baseline
#'
#' @name baseline
#' @description \code{baseline} calculate the female baseline giving a dataframe
#' containing the gender informations.
#' @param data_df, dataframe containg the gender column.
#' @param gendercol, the name of the column containing the gender information.
#' @return This function returns a numeric vector containing the baseline values
#' @examples
#' ## df is the dataframe in output from the function assign_gender
#' df <- data.frame(first_name = c("anna", "john", "ernest", "colin", "aileen"), 
#'                  gender = c("F", "M",  "M", "M", "F"), 
#'                  stringsAsFactors = FALSE)
#' baseline <- baseline(df, gendercol = gender)
#' @importFrom dplyr ungroup
#' @importFrom dplyr count
#' @importFrom rlang .data
#' @export

baseline <- function(data_df, gendercol = .data$GENDER) {
  gender_count <- data_df %>% 
    ungroup() %>%
    count({{gendercol}})
  female <- as.numeric(gender_count %>% 
                         filter({{gendercol}} == "F") %>%
                         select(.data$n))
  male <- as.numeric(gender_count %>% 
                       filter({{gendercol}} == "M") %>%
                       select(.data$n))
  baseline <- round(100 * ((female) / (female + male)), 1)
  baseline
}

