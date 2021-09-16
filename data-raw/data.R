#'
#' data frame with random names
#' @docType data
#' @format csv data frame
#' @usage data(authors)
#'

## generate a dummy data frame containing names
authors <- read.csv("../data/input_names.csv") %>%
  mutate_if(is.factor, as.character)

usethis::use_data(authors, overwrite = TRUE)
