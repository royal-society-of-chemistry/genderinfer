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
#'@export

calculate_binom_baseline <- function(data_df, baseline_female,
                                     confidence_level = 0.95) {
  
  #print("Create empty columns")
  outdf <- data_df 
  outdf$lower_CI = 0
  outdf$upper_CI = 0
  outdf$lower_CI_count = 0
  outdf$upper_CI_count = 0
  outdf$adjusted_p_value = 0
  outdf$significance = ""
  outdf$baseline = baseline_female
  outdf
  #print("create binomial vector")
  rr_row <- sapply(seq_len(length(outdf$female)), function(x) {
    .calculate_binom_proportions(noFirst = as.numeric(outdf$female[x]),
                                 noSecond = as.numeric(outdf$male[x]),
                                 expectedProportion = as.numeric(outdf$baseline[x]) / 100)
  })
  rr_row
  #outbin <- lapply(seq_len(length(outdf$female)), function(x) {
  for (x in seq_len(length(outdf$female))) {
    outdf[x, "lower_CI"] <- round(100 * as.numeric(rr_row["LowerCI", x]), 2)
    outdf[x, "lower_CI_count"] <- round(outdf$female[x] * rr_row["LowerCI", x] / rr_row["ActualProportion", x], 2)
    outdf[x, "upper_CI"] <- round(100 * as.numeric(rr_row["UpperCI", x]), 2)
    outdf[x, "upper_CI_count"] <- round(outdf$female[x] * rr_row["UpperCI", x] / rr_row["ActualProportion", x], 2)
    outdf[x, "adjusted_p_value"] <- rr_row["AdjustedPValue", x][[1]]
    outdf[x, "significance"] <- ifelse(rr_row["AdjustedPValue", x][[1]] < (1 - 0.95),
                                       "Significant", "")
    outdf
  }
  return(outdf)
  #outbin_df <- do.call(rbind.data.frame, t(outbin))
  #outbin_df$significance <- as.character(outbin_df$significance)
    #mutate_if(is.list, as.numeric)
}
