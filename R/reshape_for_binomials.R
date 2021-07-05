#' Reshape the dataframe to apply binomial calculations.
#'
#' @name reshape_for_binomials
#' @description reshape dataframe from long format to wide format.
#' @param data_df, dataframe containing the columns gender and counts
#' @param level, variable to compare for the baseline.
#' @param gendercol, the name of the column containing the gender values.
#' @importFrom tidyr pivot_wider
#' @export


reshape_for_binomials <- function(data_df, gendercol, level){
  pivot_wider({{data_df}}, names_from = {{gendercol}}, values_from = .data$n) %>%
    mutate(female = as.numeric(.data$F),
           male = as.numeric(.data$M),
           unknown = as.numeric(.data$U),
           level = factor({{level}}, levels = {{level}}),
           total_for_level = .data$female + .data$male + .data$unknown,
           total_female_male = .data$female + .data$male,
           female_percentage = round((.data$female / .data$total_female_male) * 100,
                                    1),
           male_percentage = round((.data$male / .data$total_female_male) * 100, 
                                  1)) %>%
    select(-.data$F, -.data$M, -.data$U)
}