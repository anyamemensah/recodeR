# Test expand_delim_dict.R

# Setup
dict_expected <- tibble::tibble(
  variable = c(rep("number", times = 3), rep("fruit", times = 3), 
               rep("letter", times = 3), rep("weight", times = 4)),
  old_values = c(1,2,3, "r", "b", "g", "a", "b", "c", 150, 160, 170, 180),
  new_values = c("one", "two", "three", "red", "blue", "green", "apple", 
                 "blueberry", "cantaloupe", 150, 160, 170, 180),
  default = c(rep(".x", times = 3), 
              rep("no color", times = 3), 
              rep(NA, times = 3), 
              rep(-999, times = 4)),
  string = c(rep(1, times = 9), rep(0, times = 4)))


test_that("'df' argument is not a data.frame or tibble.", {
  
  expect_error(expand_delim_dict(
    df = NULL,
    var_col = "variable",
    from_col = "old_values",
    to_col = "new_values",
    default_col = "default",
    string_col = "string", 
    remove_na = FALSE, 
    remove_empty = FALSE,
    from_delim = ";",
    to_delim = ";",
    check_df = FALSE
  ), "The 'df' argument is not a data frame.")
  
  
  expect_error(expand_delim_dict(
    df = 1:3,
    var_col = "variable",
    from_col = "old_values",
    to_col = "new_values",
    default_col = "default",
    string_col = "string", 
    remove_na = FALSE, 
    remove_empty = FALSE,
    from_delim = ";",
    to_delim = ";",
    check_df = FALSE
  ), "The 'df' argument is not a data frame.")
})


test_that("'df' argument is an empty data.frame or tibble.", {
  
  expect_error(expand_delim_dict(
    df = data.frame(),
    var_col = "variable",
    from_col = "old_values",
    to_col = "new_values",
    default_col = "default",
    string_col = "string", 
    remove_na = FALSE, 
    remove_empty = FALSE,
    from_delim = ";",
    to_delim = ";",
    check_df = FALSE
  ), "The 'df' argument is empty.")
  
  expect_error(
    expand_delim_dict(
      df = tibble::tibble(),
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string", 
      remove_na = FALSE, 
      remove_empty = FALSE,
      from_delim = ";",
      to_delim = ";",
      check_df = FALSE
    ), "The 'df' argument is empty.")
})


test_that("'var_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = c("column_name", "variable"),
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "Invalid 'var_col' argument. 'var_col' must be a character vector of length one."
            )
            
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "column_name",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "The 'var_col' argument is not a column in 'df'."
            )
          })


test_that("'from_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = c("val", "old_values"),
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "Invalid 'from_col' argument. 'from_col' must be a character vector of length one."
            )
            
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "vals",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "The 'from_col' argument is not a column in 'df'."
            )
          })


test_that("'to_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = c("labels", "new_values"),
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "Invalid 'to_col' argument. 'to_col' must be a character vector of length one."
            )
            
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_labels",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "The 'to_col' argument is not a column in 'df'."
            )
          })


test_that("'default_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = c("default", "default_col"),
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "Invalid 'default_col' argument. 'default_col' must be a character vector of length one."
            )
            
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default_vals",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "The 'default_col' argument is not a column in 'df'."
            )
          })


test_that("'string_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = c("string", "string_col"),
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "Invalid 'string_col' argument. 'string_col' must be a character vector of length one."
            )
            
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string_col",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = ";",
                check_df = FALSE
              ),
              "The 'string_col' argument is not a column in 'df'."
            )
          })


test_that("'from_delim' argument is not a character vector of length one",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = c(",",";"),
                to_delim = ";",
                check_df = FALSE
              ),
              "Invalid 'from_delim' argument. 'from_delim' must be a character vector of length one."
            )
          })


test_that("'to_delim' argument is not a character vector of length one",
          {
            expect_error(
              expand_delim_dict(
                df = dict_expected,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE,
                from_delim = ";",
                to_delim = c(",",";"),
                check_df = FALSE
              ),
              "Invalid 'to_delim' argument. 'to_delim' must be a character vector of length one."
            )
          })

  
test_that("'expand_delim_dict' returns expanded data dictionary", {
  
  dict_observed <- tibble::tibble(
    variable = c("number", "fruit", "letter", "weight"),
    old_values = c("1;2;3","r;b;g", "a;b;c","150;160;170;180"),
    new_values = c("one;two;three","red;blue;green","apple;blueberry;cantaloupe",
                   "150;160;170;180"),
    default = c(".x","no color",NA,-999),
    string = c(1,1,1,0)
  )
  expanded_dict_observed <- 
    expand_delim_dict(
      df = dict_observed,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE,
      from_delim = ";",
      to_delim = ";",
      check_df = FALSE
    )
  
  expect_equal(expanded_dict_observed, dict_expected)
  
})
