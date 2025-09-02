# Test check_dict.R

# Setup
dict_pass <- data.frame(
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
  expect_error(
    check_dict(
      df = NULL,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    "The 'df' argument is not a data.frame."
  )
  
  expect_error(
    check_dict(
      df = NA,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    "The 'df' argument is not a data.frame."
  )
  
  expect_error(
    check_dict(
      df = 1:3,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    "The 'df' argument is not a data.frame."
  )
})


test_that("'df' argument is an empty data.frame or tibble.", {
  expect_error(
    check_dict(
      df = tibble::tibble(),
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    "The 'df' argument is empty."
  )
  
  expect_error(
    check_dict(
      df = data.frame(),
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    "The 'df' argument is empty."
  )
})


test_that("'var_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = c("column_name", "variable"),
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "Invalid 'var_col' argument. 'var_col' must be a character vector of length one."
            )
            
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "column_name",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "The 'var_col' argument is not a column in 'df'."
            )
          })


test_that("'from_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = c("val", "old_values"),
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "Invalid 'from_col' argument. 'from_col' must be a character vector of length one."
            )
            
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "vals",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "The 'from_col' argument is not a column in 'df'."
            )
          })


test_that("'to_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "old_values",
                to_col = c("labels", "new_values"),
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "Invalid 'to_col' argument. 'to_col' must be a character vector of length one."
            )
            
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_labels",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "The 'to_col' argument is not a column in 'df'."
            )
          })


test_that("'default_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = c("default", "default_col"),
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "Invalid 'default_col' argument. 'default_col' must be a character vector of length one."
            )
            
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default_vals",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "The 'default_col' argument is not a column in 'df'."
            )
          })


test_that("'string_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = c("string", "string_col"),
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "Invalid 'string_col' argument. 'string_col' must be a character vector of length one."
            )
            
            expect_error(
              check_dict(
                df = dict_pass,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string_col",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              "The 'string_col' argument is not a column in 'df'."
            )
          })


test_that("'remove_na' argument is not a logical vector of length one", {
  expect_error(
    check_dict(
      df = dict_pass,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = c(FALSE, TRUE),
      remove_empty = FALSE
    ),
    "Invalid 'remove_na' argument. 'remove_na' must be a logical vector of length one."
  )
  
  expect_error(
    check_dict(
      df = dict_pass,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = c(0, 1),
      remove_empty = FALSE
    ),
    "Invalid 'remove_na' argument. 'remove_na' must be a logical vector of length one."
  )
})


test_that("'remove_empty' argument is not a logical vector of length one", {
  expect_error(
    check_dict(
      df = dict_pass,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = c(FALSE, TRUE)
    ),
    "Invalid 'remove_empty' argument. 'remove_empty' must be a logical vector of length one."
  )
  
  expect_error(
    check_dict(
      df = dict_pass,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = c(0, 1)
    ),
    "Invalid 'remove_empty' argument. 'remove_empty' must be a logical vector of length one."
  )
})


test_that("'One variable in the dictionary only has a single row of data in the dictionary",
          {
            dict_error <- data.frame(
              variable = "odd",
              old_values = TRUE,
              new_values = 1,
              default = NA,
              string = 1
            )
            
            expect_error(
              check_dict(
                df = dict_error,
                var_col = "variable",
                from_col = "old_values",
                to_col = "new_values",
                default_col = "default",
                string_col = "string",
                remove_na = FALSE,
                remove_empty = FALSE
              ),
              paste0(
                "The following variables only have a single row of data in 'dict_error':\n",
                paste("odd", collapse = "\n")
              )
            )
            
          })


test_that("'One variable has more than one string value associated with it", {
  dict_error <- data.frame(
    variable = "odd",
    old_values = TRUE,
    new_values = c(1, 2),
    default = NA,
    string = c(0, 1)
  )
  
  expect_error(
    check_dict(
      df = dict_error,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    paste0(
      "The following variables have more than one specified string flag in 'dict_error':\n",
      paste("odd", collapse = "\n")
    )
  )
  
})


test_that("'One variable has more than one string value associated with it", {
  dict_error <- data.frame(
    variable = "odd",
    old_values = TRUE,
    new_values = c(1, 2),
    default = NA,
    string = 0
  )
  
  expect_error(
    check_dict(
      df = dict_error,
      var_col = "variable",
      from_col = "old_values",
      to_col = "new_values",
      default_col = "default",
      string_col = "string",
      remove_na = FALSE,
      remove_empty = FALSE
    ),
    paste0(
      "The variable value and value label columns in 'dict_error' do not contain an equal number of variable values and value labels",
      " for the following variables:\n",
      paste("odd", collapse = "\n")
    )
  )
  
})


test_that("'check_dict' returns original 'df' with all checks passed.", {
  observed <- check_dict(
    df = dict_pass,
    var_col = "variable",
    from_col = "old_values",
    to_col = "new_values",
    default_col = "default",
    string_col = "string",
    remove_na = FALSE,
    remove_empty = FALSE
  )
  expected <- dict_pass
  
  expect_equal(observed, expected)
  
})
