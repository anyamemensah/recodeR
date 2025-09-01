#' @title Create a recoding data dictionary
#'
#' @description Creates a 'recoding' dictionary for string and numeric variables.
#' Each dictionary includes unique column names from a dataset to recode, a series of
#' two-sided formulas to define a mapping from old values to new values, default values
#' to use when a matching (old) value is not found, and a column indicating whether the new
#' values to be applied to each unique column are string (1) or not (0).
#' @param df A data.frame or tibble containing unique variable values and value labels
#' for variables in a dataset. Two optional columns can also be included: `default`, a
#' default value for old values without a new mapped value/label and `string`, a logical
#' column indicating whether the new values to be mapped onto the variable are string (1)
#' or not (0).
#' @param dict_type Character string of a type of dictionary to create. Must be one
#' of `all`, `string`, or `numeric`. Default is `all`.
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
#'
#' @return A tibble
#'
#' @export
create_recode_dict <- function(df,
                               dict_type = "all",
                               var_col = "var",
                               values_col = "values",
                               labels_col = "labels",
                               default_col = "default",
                               string_col = "string") {

  # Check 'df' is a data frame with at least one row/column
  df_name <- deparse(substitute(df))
  
  if (!is.data.frame(df)) {
    stop(paste0("The 'df' argument is not a data.frame."))
  }
  
  if (prod(dim(df)) == 0) {
    stop(paste0("The 'df' argument is empty."))
  }

  # Check 'dict_type' is one of all, string, or numeric
  if (!is.character(dict_type) || length(dict_type) != 1) {
    stop("Invalid 'dict_type' argument. 'dict_type' must be a character vector of length one.")
  }

  if (! all(dict_type %in% c("all", "string", "numeric"))) {
    stop("Invalid 'dict_type' argument. 'dict_type' must be one of: 'all', 'string', or 'numeric'")
  }

  # Create 'return_type'
  return_type <-
    if (dict_type == "all") {
      c("string", "numeric")
    } else {
      dict_type
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
    stop(paste0("The 'labels_col' argument is not a column in 'df'."))
  }

  # Check 'default_col' is a character vector of length one and exists in 'df'
  if (!is.character(default_col) || length(default_col) != 1) {
    stop("Invalid 'default_col' argument. 'default_col' must be a character vector of length one.")
  }
  
  if (! default_col %in% colnames(df)) {
    stop(paste0("The 'default_col' argument is not a column in 'df'."))
  }

  # Check 'string_col' is a character vector of length one, exists in 'df', and all values are 0/1
  if (!is.character(string_col) || length(string_col) != 1) {
    stop("Invalid 'string_col' argument. 'string_col' must be a character vector of length one.")
  }
  
  if (! string_col %in% colnames(df)) {
    stop(paste0("The 'string_col' argument is not a column in 'df'."))
  }

  if (!all(is.element(df[["string"]], c(0,1)))) {
    stop(paste0("The 'string_col' in 'df' contains values other than 0 or 1."))
  }

  # Create the standardized dictionary
  df_dicts <-
    purrr::map(.x = return_type, .f = ~ {

      is_string <- switch(.x, string = TRUE, numeric = FALSE)

      .generate_dict(
        df = df,
        var_col = var_col,
        values_col = values_col,
        labels_col = labels_col,
        is_string = is_string,
        string_col = string_col,
        default_col = default_col
      )
    }) |>
    stats::setNames(return_type)

  df_dicts
}

#' @keywords internal
.generate_dict <- function(df,
                           is_string,
                           var_col,
                           values_col,
                           labels_col,
                           default_col,
                           string_col) {
  if (!sum(df[[string_col]] == is_string)) {
    return(NULL)
  } else {
    df |>
      dplyr::filter(!!rlang::sym(string_col) == is_string) |>
      dplyr::mutate(dplyr::across(
        .cols = dplyr::all_of(c(values_col, labels_col, default_col)),
        .fns = if (is_string)
          as.character
        else
          as.numeric
      )) |>
      dplyr::group_by(!!rlang::sym(var_col)) |>
      dplyr::summarize(
        formula_pairs = list(
          .tbl_key(
            values = !!rlang::sym(values_col),
            labels = !!rlang::sym(labels_col),
            string = unique(!!rlang::sym(string_col))
          )
        ),
        default = unique(!!rlang::sym(default_col)),
        string = unique(!!rlang::sym(string_col)),
        .groups = "drop"
      )
  }
}

