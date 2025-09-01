#' @title Check Data Dictionary Structure
#'
#' @description Checks whether (old) values and (new) values/labels for each unique
#' columns/variable in a data dictionary (flat file) are the same length and unique.
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
#' @param remove_na A logical value indicating whether to remove NA values from values/labels.
#' Default is `FALSE`.
#' @param remove_empty A logical value indicating whether to remove empty values from values/
#' labels. Default is `FALSE`.
#'
#' @return A tibble
#'
#' @export
check_dict <- function(df,
                       var_col = "var",
                       values_col = "values",
                       labels_col = "labels",
                       default_col = "default",
                       string_col = "string",
                       remove_na = FALSE,
                       remove_empty = FALSE) {

  # Check 'df' is a data frame with at least one row/column
  df_name <- deparse(substitute(df))

  if (!is.data.frame(df)) {
    stop(paste0("The 'df' argument is not a data.frame."))
  }

  if (prod(dim(df)) == 0) {
    stop(paste0("The 'df' argument is empty."))
  }
  
  message(paste0("'", df_name, "' is a valid column in '", df_name, "'. \u2714"))
  
  # Check 'var_col' is a character vector of length one and exists in 'df'
  if (!is.character(var_col) || length(var_col) != 1) {
    stop("Invalid 'var_col' argument. 'var_col' must be a character vector of length one.")
  }

  if (! var_col %in% colnames(df)) {
    stop("The 'var_col' argument is not a column in 'df'.")
  }

  message(paste0("'", var_col, "' is a valid column in '", df_name, "'. \u2714"))

  # Check 'values_col' is a character vector of length one and exists in 'df'
  if (!is.character(values_col) || length(values_col) != 1) {
    stop("Invalid 'values_col' argument. 'values_col' must be a character vector of length one.")
  }

  if (! values_col %in% colnames(df)) {
    stop("The 'values_col' argument is not a column in 'df'.")
  }
  
  message(paste0("'", values_col, "' is a valid column in '", df_name, "'. \u2714"))

  # Check 'labels_col' is a character vector of length one and exists in 'df'
  if (!is.character(labels_col) || length(labels_col) != 1) {
    stop("Invalid 'labels_col' argument. 'labels_col' must be a character vector of length one.")
  }

  if (! labels_col %in% colnames(df)) {
    stop(paste0("The 'labels_col' argument is not a column in 'df'."))
  }

  message(paste0("'", labels_col, "' is a valid column in '", df_name, "'. \u2714"))

  # Check 'default_col' is a character vector of length one and exists in 'df'
  if (!is.character(default_col) || length(default_col) != 1) {
    stop("Invalid 'default_col' argument. 'default_col' must be a character vector of length one.")
  }

  if (! default_col %in% colnames(df)) {
    stop(paste0("The 'default_col' argument is not a column in 'df'."))
  }

  message(paste0("'", default_col, "' is a valid column in '", df_name, "'. \u2714"))

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

  message(paste0(string_col, " is a valid column in '", df_name, "'. \u2714"))

  # Check 'remove_na'
  if (! is.logical(remove_na) || length(remove_na) != 1) {
    stop("Invalid 'remove_na' argument. 'remove_na' must be a logical vector of length one.")
  }

  # Check 'remove_empty'
  if (! is.logical(remove_empty) || length(remove_empty) != 1) {
    stop("Invalid 'remove_empty' argument. 'remove_empty' must be a logical vector of length one.")
  }

  # Check that there is more than one row associated with each unique column listed in 'var_col'
  count_per_col <-
    df |>
    dplyr::select(!!rlang::sym(var_col)) |>
    dplyr::group_by(!!rlang::sym(var_col)) |>
    dplyr::count(name = "count") |>
    dplyr::filter(count < 2) |>
    dplyr::pull(!!rlang::sym(var_col))

  if (length(count_per_col) > 0) {
    stop(paste0("The following variables only have a single row of data in '", df_name, "':\n",
                paste(count_per_col, collapse = "\n")))
  }

  message(paste0("Each unique variable in '", df_name, "' is associated with more than one row of data. \u2714"))

  # Check that there is only one default value per variable
  count_default_per_col <-
    df |>
    dplyr::select(dplyr::all_of(c(var_col, default_col))) |>
    dplyr::group_by(!!rlang::sym(var_col)) |>
    dplyr::distinct() |>
    dplyr::count(name = "count") |>
    dplyr::filter(count > 1) |>
    dplyr::pull(!!rlang::sym(var_col))

  if (length(count_default_per_col) > 0) {
    stop(paste0("The following variables have more than one specified default value in '", df_name, "':\n",
                paste(count_default_per_col, collapse = "\n")))
  }

  message(paste0("Each unique variable in '", df_name, "' is associated with a single default value. \u2714"))

  # Check that there is only one string value per variable
  count_string_per_col <-
    df |>
    dplyr::select(dplyr::all_of(c(var_col, string_col))) |>
    dplyr::group_by(!!rlang::sym(var_col)) |>
    dplyr::distinct() |>
    dplyr::count(name = "count") |>
    dplyr::filter(count > 1) |>
    dplyr::pull(!!rlang::sym(var_col))

  if (length(count_string_per_col) > 0) {
    stop(paste0("The following variables have more than one specified string flag in '", df_name, "':\n",
                paste(count_string_per_col, collapse = "\n")))
  }

  message(paste0("Each unique variable in '", df_name, "' is associated with a single string flag \u2714"))

  # Find unique number of value/label pairs
  check_dict_cols <-
    df |>
    dplyr::select(dplyr::all_of(c(var_col,values_col,labels_col))) |>
    dplyr::group_by(.data[[var_col]]) |>
    dplyr::summarize(across(
      .cols = dplyr::all_of(c(values_col,labels_col)),
      .fns =  list(unique_obs = ~ length(.extract_unique(x = ., remove_na = remove_na, remove_empty = remove_empty))))) |>
    dplyr::ungroup() |>
    dplyr::mutate(is_not_equal_len = .data[[paste0(values_col,"_unique_obs")]] !=
                    .data[[paste0(labels_col,"_unique_obs")]]) |>
    dplyr::filter(is_not_equal_len == TRUE) |>
    dplyr::select(dplyr::all_of(c(var_col))) |>
    dplyr::pull()

  # Return name of variables where value and labels pairs are not the same length,
  if (length(check_dict_cols) > 0) {
    stop(paste0(
      "The variable value and value label columns in '", df_name,
      "' do not contain an equal number of variable values and value labels",
      " for the following variables:\n",
      paste(check_dict_cols, collapse = "\n"))
    )
  }

  # Otherwise everything looks good.
  message(paste0("The value and label pairs for each unique variable in '",
                 df_name, "' have the same length. \u2714"))

  df
}
