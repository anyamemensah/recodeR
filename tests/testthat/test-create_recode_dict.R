# Test create_recode_dict.R

# Setup
dict <- tibble::tibble(
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
  
  expect_error(create_recode_dict(
    df = NULL,
    dict_type = "all",
    var_col = "variable",
    values_col = "old_values",
    labels_col = "new_values",
    default_col = "default",
    string_col = "string"
  ), "The 'df' argument is not a data.frame")
  
  
  expect_error(create_recode_dict(
    df = 1:10,
    dict_type = "all",
    var_col = "variable",
    values_col = "old_values",
    labels_col = "new_values",
    default_col = "default",
    string_col = "string"
  ), "The 'df' argument is not a data.frame")
})


test_that("'df' argument is an empty data.frame or tibble.", {
  
  expect_error(create_recode_dict(
    df = tibble::tibble(),
    dict_type = "all",
    var_col = "variable",
    values_col = "old_values",
    labels_col = "new_values",
    default_col = "default",
    string_col = "string"
  ), "The 'df' argument is empty.")
  
  expect_error(
    create_recode_dict(
      df = data.frame(),
      dict_type = "all",
      var_col = "variable",
      values_col = "old_values",
      labels_col = "new_values",
      default_col = "default",
      string_col = "string"
    ), "The 'df' argument is empty.")
})


test_that("'var_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = c("var","var_names"),
                values_col = "old_values",
                labels_col = "new_values",
                default_col = "default",
                string_col = "string"
              ),
              "Invalid 'var_col' argument. 'var_col' must be a character vector of length one."
            )
            
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "vars",
                values_col = "old_values",
                labels_col = "new_values",
                default_col = "default",
                string_col = "string"
              ),
              "The 'var_col' argument is not a column in 'df'."
            )
          })


test_that("'values_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = c("val", "old_values"),
                labels_col = "new_values",
                default_col = "default",
                string_col = "string"
              ),
              "Invalid 'values_col' argument. 'values_col' must be a character vector of length one."
            )
            
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "vals",
                labels_col = "new_values",
                default_col = "default",
                string_col = "string"
              ),
              "The 'values_col' argument is not a column in 'df'."
            )
          })


test_that("'labels_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "old_values",
                labels_col = c("labels", "new_values"),
                default_col = "default",
                string_col = "string"
              ),
              "Invalid 'labels_col' argument. 'labels_col' must be a character vector of length one."
            )
            
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "old_values",
                labels_col = "labs",
                default_col = "default",
                string_col = "string"
              ),
              "The 'labels_col' argument is not a column in 'df'."
            )
          })


test_that("'default_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "old_values",
                labels_col = "new_values",
                default_col = c("default", "def_col"),
                string_col = "string"
              ),
              "Invalid 'default_col' argument. 'default_col' must be a character vector of length one."
            )
            
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "old_values",
                labels_col = "new_values",
                default_col = "def_col",
                string_col = "string"
              ),
              "The 'default_col' argument is not a column in 'df'."
            )
          })


test_that("'string_col' argument is not a character vector of length one, nor does it exist in 'df'",
          {
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "old_values",
                labels_col = "new_values",
                default_col = "default",
                string_col = c("string", "string_col")
              ),
              "Invalid 'string_col' argument. 'string_col' must be a character vector of length one."
            )
            
            expect_error(
              create_recode_dict(
                df = dict,
                dict_type = "all",
                var_col = "variable",
                values_col = "old_values",
                labels_col = "new_values",
                default_col = "default",
                string_col = "string_col"
              ),
              "The 'string_col' argument is not a column in 'df'."
            )
          })

test_that("'create_recode_dict' returns a list of data.frames: string df is populated; numeric is NULL", {
  
  dict_observed <- 
    tibble::tibble(
      col_name = c(rep("color", times = 3), rep("country_abbrev", times = 3)),
      old_vals = c("r", "b", "g", "GH", "TN", "CI"),
      new_vals = c("red", "blue", "green", "Ghana", "Tunisia", "Ivory Coast"),
      default = NA,
      string = 1
    )
  
  observed_recoding_dict <- 
    create_recode_dict(
      df = dict_observed,
      dict_type = "all",
      var_col = "col_name",
      values_col = "old_vals",
      labels_col = "new_vals",
      default_col = "default",
      string_col = "string"
    )
  
  expected_recoding_dict <- 
    list(
      tibble::tibble(
        col_name = c("color", "country_abbrev"),
        formula_pairs = list(
          purrr::map2(.x = c("r", "b", "g"),
                      .y = c("red", "blue", "green"),
                      .f = ~ rlang::new_formula(.x, .y)),
          purrr::map2(.x = c("GH", "TN", "CI"),
                      .y = c("Ghana", "Tunisia", "Ivory Coast"),
                      .f = ~ rlang::new_formula(.x, .y))
        )) |>
        dplyr::mutate(
          default = NA_character_,
          string = 1
        ),
      NULL) |> stats::setNames(c("string", "numeric"))
  
  expect_equal(observed_recoding_dict, expected_recoding_dict, ignore_attr = TRUE)
  
})
