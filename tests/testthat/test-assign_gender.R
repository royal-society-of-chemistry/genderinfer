context("Assign gender to vector")

test_that("gender is correct", {
  
  name <- c("anna", "john", "david", "ernest", "colin", "aileen")
  df <- data.frame(FirstName = name, stringsAsFactors = FALSE)
  gender <- c("F", "M", "M", "M", "M", "F")


  assigngender <- assign_gender(data_df = df, first_name_col = df$FirstName)
  expect_is(assigngender, "data.frame")
  expect_is(assigngender$gender, "character")
  expect_equal(assigngender$gender, gender)
  
})

