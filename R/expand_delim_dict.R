#' @title Expand a delimited data dictionary
#'
#' @description Expands a delimited data dictionary in which each row represents a unique 
#' variable name along with its associated from values (original values), to values (new 
#' values), a default value to apply when a from value has no matching to value, and a logical 
#' indicator specifying whether the to values are strings (1) or not (0). The function 
#' restructures the data so that each unique from/to value pair for each variable is represented 
#' as a separate row in the output.
#'
#' @param df A data frame that lists the unique values and corresponding labels for variables 
#' in a dataset. It should also include two additional columns: `default`, which specifies a 
#' fallback value for any unmatched original values, and `string`, a logical indicator denoting 
#' whether the new mapped values are strings (1) or not (0).
#' @param var_col A character string of the name of the column in `df` that contains unique
#' variable names in your dataset.
#' @param from_col A character string specifying the name of the column in `df` that contains 
#' the original values (`from_values`) for each variable in your dataset.
#' @param to_col A character string specifying the name of the column in `df` that contains 
#' the new values (`to_values`) for each variable in your dataset.
#' @param default_col A character string specifying the name of the column in `df` that contains
#' a fallback value for any unmatched original values.
#' @param string_col A character string specifying the name of the column in `df` that indicates 
#' whether the new values to be mapped onto the variable are string.
#' @param from_delim A character or set of characters used to separate values in the
#' `from_col`. Default is a comma `,`.
#' @param to_delim A character or set of characters used to separate values in the
#' `to_col`. Default is a semi-colon `;`.
#' @param remove_na A logical value indicating whether to remove `NA` values from new and old
#' values. Default is `FALSE`.
#' @param remove_empty A logical value indicating whether to remove empty values from new and old
#' values. Default is `FALSE`.
#' @param check_df A logical value indicating whether to use the `check_dict` function to 
#' verify that the expanded data dictionary meets specific criteria necessary for use with 
#' the `create_dict` function. Default is `TRUE`.
#'
#' @return A tibble
#'
#' @export
expand_delim_dict <- function(df,
                              var_col = "var",
                              from_col = "values",
                              to_col = "labels",
                              default_col = "default",
                              string_col = "string",
                              from_delim = ",",
                              to_delim = ";",
                              remove_na = FALSE,
                              remove_empty = FALSE,
                              check_df = TRUE)  {

  # Check 'df' is a data frame with at least one row/column
  df_name <- deparse(substitute(df))

  if (!is.data.frame(df)) {
    stop("The 'df' argument is not a data frame.")
  }
  
  if (prod(dim(df)) == 0) {
    stop("The 'df' argument is empty.")
  }

  # Check 'var_col' is a character vector of length one and exists in 'df'
  if (!is.character(var_col) || length(var_col) != 1) {
    stop("Invalid 'var_col' argument. 'var_col' must be a character vector of length one.")
  }
  
  if (! var_col %in% colnames(df)) {
    stop("The 'var_col' argument is not a column in 'df'.")
  }

  # Check 'from_col' is a character vector of length one and exists in 'df'
  if (!is.character(from_col) || length(from_col) != 1) {
    stop("Invalid 'from_col' argument. 'from_col' must be a character vector of length one.")
  }
  
  if (! from_col %in% colnames(df)) {
    stop("The 'from_col' argument is not a column in 'df'.")
  }

  # Check 'to_col' is a character vector of length one and exists in 'df'
  if (!is.character(to_col) || length(to_col) != 1) {
    stop("Invalid 'to_col' argument. 'to_col' must be a character vector of length one.")
  }
  
  if (! to_col %in% colnames(df)) {
    stop("The 'to_col' argument is not a column in 'df'.")
  }

  # Check 'default_col' is a character vector of length one and exists in 'df'
  if (!is.character(default_col) || length(default_col) != 1) {
    stop("Invalid 'default_col' argument. 'default_col' must be a character vector of length one.")
  }
  
  if (! default_col %in% colnames(df)) {
    stop("The 'default_col' argument is not a column in 'df'.")
  }

  # Check 'string_col' is a character vector of length one, exists in 'df', and all values are 0/1
  if (!is.character(string_col) || length(string_col) != 1) {
    stop("Invalid 'string_col' argument. 'string_col' must be a character vector of length one.")
  }
  
  if (! string_col %in% colnames(df)) {
    stop("The 'string_col' argument is not a column in 'df'.")
  }

  if (!all(is.element(df[["string"]], c(0,1)))) {
    stop("The 'string_col' in 'df' contains values other than 0 or 1.")
  }
  
  # Check 'from_delim' is a character vector of length one
  if (!is.character(from_delim) || length(from_delim) != 1) {
    stop("Invalid 'from_delim' argument. 'from_delim' must be a character vector of length one.")
  }
  
  # Check 'to_delim' is a character vector of length one
  if (!is.character(to_delim) || length(to_delim) != 1) {
    stop("Invalid 'to_delim' argument. 'to_delim' must be a character vector of length one.")
  }
  
  # Check that values in 'df' under 'var_col' appear exactly once
  if (! length(df[[var_col]]) == length(unique(df[[var_col]]))) {
    dupe_cols <- unique(df[[var_col]][duplicated(df[[var_col]]) | duplicated(df[[var_col]], fromLast = TRUE)])
    
    stop(paste0("The following variables appear more than once in '",
                df_name,"' :\n", paste(dupe_cols)))
  }


  # Create expanded data dictionary flat file
  new_df <- vector("list", length = length(df[[var_col]])) |> stats::setNames(df[[var_col]])

  for (current_col in unique(df[[var_col]])) {
    current_df <- df |> dplyr::filter(.data[[var_col]] == current_col)

    extracted_values <-
      purrr::map2(
        .x = c(from_col, to_col),
        .y = c(from_delim, to_delim),
        .f = ~ .extract_unique(.extract_values(x = current_df[[.x]], delim = .y),
                               remove_na = remove_na,
                               remove_empty = remove_empty)
      ) |>
      stats::setNames(c(from_col, to_col))

    new_df[[current_col]] <-
      extracted_values |>
      dplyr::bind_cols() |>
      dplyr::mutate(
        !!var_col := current_col,
        !!default_col := current_df[[default_col]],
        !!string_col := current_df[[string_col]]
      ) |>
      dplyr::select(dplyr::all_of(
        c(var_col, from_col, to_col, default_col, string_col)
      ))
  }

  the_expanded_df <- dplyr::bind_rows(new_df)

  # Check 'final_df' against specific criteria
  if (check_df) {
    check_dict(df = the_expanded_df,
               var_col = var_col,
               from_col = from_col,
               to_col = to_col,
               default_col = default_col,
               string_col = string_col,
               remove_na = remove_na,
               remove_empty = remove_empty)
  }

  the_expanded_df
}
