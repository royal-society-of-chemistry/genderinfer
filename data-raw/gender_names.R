#' code to prepare `gender_names` dataset goes here
#'
#' US and UK names
#' @docType data
#'
#' @usage data(gender_names)
#' @noRd

gender_names <- read.csv("../../data/genderNames.csv") %>% 
  mutate_if(is.factor, as.character)
Encoding(gender_names$Name) <- "latin1"
gender_names$Name <- iconv(gender_names$Name, "latin1", "ASCII", sub = "")
gender_names$Name <- iconv(gender_names$Name, "latin1", "ASCII", sub = "byte")

usethis::use_data(gender_names, internal = TRUE, overwrite = TRUE)