#' Calculate binomials and significance for multiple baselines.
#'
#'@description Function to calculate the lower CI, upper CI, percentages and 
#' counts, and significance of difference from one or multiple baseline 
#' percentages, given supplied confidence level using
#'@param data_df, dataframe from the function \code{\link{reshape_for_binomials}}
#' containing the columns: female, male, which contain the integer counts of 
#' males and females respectively and must be a numeric vector greater than 0.
#'@param baseline_female, the baseline used for the calculation.
#'@param confidence_level, confidence level to use for significance calculation,
#' default is 0.95
#'@returns dataframe with additional columns than the input one:
#'
#' lower_CI = lower confidence level of confidence interval expressed as a 
#' percentage
#'       
#' upper_CI = upper confidence level of confidence interval expressed as a 
#' percentage
#'        
#' lower_CI_count = lower confidence level of confidence interval expressed as a
#'  count
#'        
#' upper_CI_count = upper confidence level of confidence interval expressed as a
#'  count
#'        
#' significance = flag indicating whether difference of female percentage with 
#' baseline percentage is significant for the row in consideration. It has
#' values "significant" or "" if not.
#'@examples
#'\dontrun{
#'##df dataframe in output from the function reshape_for_binomial
#'calculate_binom_baseline(df, baseline)
#'}
#'@importFrom dplyr mutate_if
#'@export

calculate_binom_baseline <- function(data_df, baseline_female,
                                     confidence_level = 0.95) {

  outdf <- {{data_df}} %>% 
    mutate(lower_CI = 0, upper_CI = 0, lower_CI_count = 0, upper_CI_count = 0,
           baseline = baseline_female)

  rr_row <- sapply(seq_len(length(outdf$female)), function(x) {
    .calculate_binom_proportions(noFirst = as.numeric(outdf$female[x]),
                                noSecond = as.numeric(outdf$male[x]),
                                expectedProportion = as.numeric(outdf$baseline[x]) / 100)
  })
  outbin <- lapply(seq_len(length(outdf$female)), function(x) { 
    outdf[x, ] %>%
      mutate(lower_CI = round(100 * as.numeric(rr_row["LowerCI", x]), 2),
             lower_CI_count = round(outdf$female[x] * rr_row["LowerCI", x] / rr_row["ActualProportion", x], 2),
             upper_CI = round(100 * as.numeric(rr_row["UpperCI", x]), 2),
             upper_CI_count = round(outdf$female[x] * rr_row["UpperCI", x] / rr_row["ActualProportion", x], 2),
             adjusted_p_value = rr_row["AdjustedPValue", x]) %>%
      mutate(significance = if_else(rr_row["AdjustedPValue", x] < (1 - 0.95),
                                    "Significant", ""))
  })
  outbin_df <- bind_rows(outbin) %>%
    mutate(significance = as.character(.data$significance)) %>%
    mutate_if(is.list, as.numeric)
}