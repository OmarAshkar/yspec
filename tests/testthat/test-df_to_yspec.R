test_that("df_to_yspec creates a valid YAML file with top-level variables", {
  # Create a dummy data frame
  adam_df <- data.frame(
    varname = c("AGE", "SEX", "WEIGHT"),
    vardesc = c("Age of the subject", "Sex of the subject", "Weight of the subject"),
    varunit = c("years", NA, "kg"),
    vartype = c("numeric", "character", "numeric"),
    stringsAsFactors = FALSE
  )
  
  # Define output path
  output_path <- tempfile(fileext = ".yaml")
  
  # Run the function
  df_to_yspec(
    x = adam_df,
    desc = "Another example PK type data set",
    varname = "varname",
    vardesc = "vardesc",
    varunit = "varunit",
    vartype = "vartype",
    output = output_path
  )
  
  # Validate the YAML file
  yaml_content <- ys_load(output_path)
  
  # Validate SETUP__ section
  expect_equal(get_meta(yaml_content)[[1]], "Another example PK type data set")
  
  # Validate top-level variables
  expect_equal(yaml_content$AGE$long, "Age of the subject")
  expect_equal(yaml_content$AGE$unit, "years")
  expect_equal(yaml_content$AGE$type, "numeric")
  
  expect_equal(yaml_content$SEX$long, "Sex of the subject")
  expect_false("unit" %in% names(yaml_content$SEX)) # Ensure 'unit' is excluded for N/A
  
  expect_equal(yaml_content$WEIGHT$long, "Weight of the subject")
  expect_equal(yaml_content$WEIGHT$unit, "kg")
  expect_equal(yaml_content$WEIGHT$type, "numeric")
})
