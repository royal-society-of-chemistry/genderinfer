#' Function to calculate the binomial proportions
#'
#' @description \code{calculate_binom_proportions} calls Exact Binomial Test
#'  function \code{binom.test}
#' @param noFirst, number of successes, numeric vector
#' @param noSecond, number of failures, numeric vector
#' @param expectedProportion, background population
#' @param noTests, for multinomial test default value 1
#' @param confidenceLevel, confidence level for the returned confidence interval
#'  default is 0.95 using account method mechanism.
#' @return The output will be a vector of the form:
#'
#'   noFirst = first input parameter,
#'
#'   noSecond = second input parameter
#'
#'   AdjustedPValue = binom test P-value corrected for number of tests
#'
#'   PValue = binom test P-value,
#'
#'   ActualProportion = binom test estimated probability of success
#'
#'   ExpectedProportion = binom test probability of success under the null
#'
#'   LowerCI = binom test lower confidence level of returned confidence interval
#'
#'   UpperCI = binom test upper confidence level of returned confidence interval
#'\url{https://stat.ethz.ch/R-manual/R-patched/library/stats/html/binom.test.html}
#' @examples
#' \dontrun{
#' femaleNumber = 24
#' maleNumber = 36
#' baseFemalePercentage = 23.5
#' maleAndfemaleTotalNumber = 60
#' calculate_binom_proportions(as.numeric(femaleNumber), as.numeric(maleNumber),
#'  baseFemalePercentage/100, maleAndfemaleTotalNumber,0.95)
#'}
#' @importFrom binom binom.confint
#' @importFrom stats binom.test
#' @noRd

.calculate_binom_proportions <- function(noFirst, noSecond, expectedProportion,
                                        noTests=1, confidenceLevel=0.95) {
  rr <- vector("numeric", 8)
  result_binom <- binom.test(x = noFirst, n = noSecond + noFirst,
                             p = expectedProportion,
                             conf.level = confidenceLevel)
  rr[1] <- noFirst
  rr[2] <- noSecond
  #simple aka conservative multitest correction
  rr[3] <- result_binom$p.value * noTests
  if (rr[3] > 1) {
      rr[3] <- 1
      }
  rr[4] <- result_binom$p.value
  rr[5] <- result_binom$estimate
  rr[6] <- result_binom$null.value
  #we use AC as we are assuming this is high number count data
  result_confidence <- binom.confint(noFirst, noSecond + noFirst,
                                     methods = "agresti-coull",
                                     conf.level = confidenceLevel)
  rr[7] <- result_confidence$lower
  rr[8] <- result_confidence$upper

  names(rr) <- c("NoFirst", "NoSecond", "AdjustedPValue", "PValue",
               "ActualProportion", "ExpectedProportion", "LowerCI", "UpperCI")
  rr
}
