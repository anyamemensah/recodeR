#' @title Expand a Data Dictionary
#'
#' @description Expands a delimited data dictionary where each row contains unique
#' variable/column names, (original) values/labels, (new) values/labels, default
#' values to apply when , and an indicator column identifying whether the new values
#' to be mapped onto the variable are string. The function transforms the data into
#' a structured format where each unique value/label pair for each unique column is
#' associated with a row in the data dictionary.
#'
#' @param df A data.frame or tibble containing unique variable values and value labels
#' for variables in a dataset. Two optional columns can also be included: `default`, a
#' default value for old values without a new mapped value/label and `string`, a logical
#' column indicating whether the new values to be mapped onto the variable are string (1)
#' or not (0).
#' @param var_col Character string of the name of the column in `df` that contains unique
#' variable/column names in your dataset.
#' @param values_col A character string specifying the name of the column in `df` that holds
#' the unique values for each variable/column in your dataset.
#' @param labels_col A character string specifying the name of the column in `df` that holds
#' the unique values/labels to replace old values for each variable/column in your dataset.
#' @param default_col A character string specifying the name of the column in `df` that holds
#' default values to use when a matching (old) value is not found.
#' @param string_col A character string specifying the name of the column in `df` that
#' indicates whether the new values to be mapped onto the variable are string.
#' @param value_delim A character or set of characters used to separate values in the
#' `values_col`. Default is a comma `,`.
#' @param label_delim A character or set of characters used to separate values in the
#' `labels_col`. Default is a semi-colon `;`.
#' @param remove_na A logical value indicating whether to remove NA values from values/labels.
#' Default is `FALSE`.
#' @param remove_empty A logical value indicating whether to remove empty values from values/
#' labels. Default is `FALSE`.
#' @param check_df A logical value indicating whether to use the `check_dict` function to verify
#' that the expanded data dictionary meets specific criteria necessary for use with the `create_dict`
#' function. Default is `TRUE`.
#'
#' @return A tibble
#'
#' @export
expand_delim_dict <- function(df,
                              var_col = "var",
                              values_col = "values",
                              labels_col = "labels",
                              default_col = "default",
                              string_col = "string",
                              value_delim = ",",
                              label_delim = ";",
                              remove_na = FALSE,
                              remove_empty = FALSE,
                              check_df = TRUE)  {

  # Check 'df' is a data frame with at least one row/column
  df_name <- deparse(substitute(df))

  if (!is.data.frame(df)) {
    stop("The 'df' argument is not a data.frame.")
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

  # Check 'values_col' is a character vector of length one and exists in 'df'
  if (!is.character(values_col) || length(values_col) != 1) {
    stop("Invalid 'values_col' argument. 'values_col' must be a character vector of length one.")
  }
  
  if (! values_col %in% colnames(df)) {
    stop("The 'values_col' argument is not a column in 'df'.")
  }

  # Check 'labels_col' is a character vector of length one and exists in 'df'
  if (!is.character(labels_col) || length(labels_col) != 1) {
    stop("Invalid 'labels_col' argument. 'labels_col' must be a character vector of length one.")
  }
  
  if (! labels_col %in% colnames(df)) {
    stop("The 'labels_col' argument is not a column in 'df'.")
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
  
  # Check 'value_delim' is a character vector of length one
  if (!is.character(value_delim) || length(value_delim) != 1) {
    stop("Invalid 'value_delim' argument. 'value_delim' must be a character vector of length one.")
  }
  
  # Check 'label_delim' is a character vector of length one
  if (!is.character(label_delim) || length(label_delim) != 1) {
    stop("Invalid 'label_delim' argument. 'label_delim' must be a character vector of length one.")
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
        .x = c(values_col, labels_col),
        .y = c(value_delim, label_delim),
        .f = ~ .extract_unique(.extract_values(x = current_df[[.x]], delim = .y),
                               remove_na = remove_na,
                               remove_empty = remove_empty)
      ) |>
      stats::setNames(c(values_col, labels_col))

    new_df[[current_col]] <-
      extracted_values |>
      dplyr::bind_cols() |>
      dplyr::mutate(
        !!var_col := current_col,
        !!default_col := current_df[[default_col]],
        !!string_col := current_df[[string_col]]
      ) |>
      dplyr::select(dplyr::all_of(
        c(var_col, values_col, labels_col, default_col, string_col)
      ))
  }

  the_expanded_df <- dplyr::bind_rows(new_df)

  # Check 'final_df' against specific criteria
  if (check_df) {
    check_dict(df = the_expanded_df,
               var_col = var_col,
               values_col = values_col,
               labels_col = labels_col,
               default_col = default_col,
               string_col = string_col,
               remove_na = remove_na,
               remove_empty = remove_empty)
  }

  the_expanded_df
}
