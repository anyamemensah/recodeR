# Test recode_cols.R

# Setup
set.seed(1234)

observed_df <- 
  data.frame(
    fruit = sample(x = c("a","c","d", NA, "AA"), size = 10, replace = TRUE),
    number = sample(x = 1:5, size = 10, replace = TRUE)
  )

observed_dict <- 
  tibble::tibble(
    variable = c(rep("number", times = 4), rep("fruit", times = 3)),
    old_values = c(1,2,3,4, "a", "d", "c"),
    new_values = c("One", "Two", "Three", "Four", "Apple", "Dates", 
                   "Cantaloupe"),
    default = c(rep("Out of Range", times = 4), rep("No Entry", times = 3)),
    string = 1)

expected_df <- data.frame(
  fruit = c("No Entry","Cantaloupe","No Entry", "No Entry",
            "Apple", "No Entry", "No Entry", "Cantaloupe",
            "Cantaloupe", "No Entry"),
  number = c("Four", "Four", "Out of Range", "Four", "Three",
             "Four", "Out of Range", "Two", "Out of Range", "Two")
)


test_that("'recode_cols' function returns expected data.frame with recoded values", {
  
  observed_recoding_dict <- 
    create_recode_dict(df = observed_dict, 
                       dict_type = "all",
                       var_col = "variable", 
                       from_col = "old_values", 
                       to_col = "new_values", 
                       default_col = "default", 
                       string_col = "string")
  
  observed_recoded_df <- 
    recode_cols(df = observed_df,
                list_of_dicts = observed_recoding_dict,
                var_col = "variable",
                formula_pairs_col = "formula_pairs",
                default_col = "default",
                string_col = "string")
  
  expect_equal(observed_recoded_df, expected_df)
  
})
