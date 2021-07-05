context("Calculate baseline")

test_that("baseline is correct", {
  
  name <- c("anna", "john", "david", "ernest", "colin", "aileen")
  gender <- c("F", "M", "M", "M", "M", "F")
  df <- data.frame(FirstName = name, 
                   gender = gender,
                   stringsAsFactors = FALSE)

  
  baselin <- baseline(df, gendercol = gender)
  expect_is(baselin, "numeric")
  #expect_match(is.na(baselin), FALSE)
  
  # expect_setequal(assigngender,
  #                 dfout)
})