#' @title Check data dictionary structure
#'
#' @description `check_dict()` verfies that the from and to values or labels for each 
#' unique variable in a data dictionary (flat file) are the same length and that all 
#' entries are unique.
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
#' @param remove_na A logical value indicating whether to remove `NA` values from new and old
#' values. Default is `FALSE`.
#' @param remove_empty A logical value indicating whether to remove empty values from new and old
#' values. Default is `FALSE`.
#'
#' @return A tibble
#'
#' @export
check_dict <- function(df,
                       var_col = "var",
                       from_col = "values",
                       to_col = "labels",
                       default_col = "default",
                       string_col = "string",
                       remove_na = FALSE,
                       remove_empty = FALSE) {

  # Check 'df' is a data frame with at least one row/column
  df_name <- deparse(substitute(df))

  if (!is.data.frame(df)) {
    stop("The 'df' argument is not a data.frame.")
  }

  if (prod(dim(df)) == 0) {
    stop("The 'df' argument is empty.")
  }
  
  message(paste0("'", df_name, "' is a valid data.frame.'. \u2714"))
  
  # Check 'var_col' is a character vector of length one and exists in 'df'
  if (!is.character(var_col) || length(var_col) != 1) {
    stop("Invalid 'var_col' argument. 'var_col' must be a character vector of length one.")
  }

  if (! var_col %in% colnames(df)) {
    stop("The 'var_col' argument is not a column in 'df'.")
  }

  message(paste0("'", var_col, "' is a valid column in '", df_name, "'. \u2714"))

  # Check 'from_col' is a character vector of length one and exists in 'df'
  if (!is.character(from_col) || length(from_col) != 1) {
    stop("Invalid 'from_col' argument. 'from_col' must be a character vector of length one.")
  }

  if (! from_col %in% colnames(df)) {
    stop("The 'from_col' argument is not a column in 'df'.")
  }
  
  message(paste0("'", from_col, "' is a valid column in '", df_name, "'. \u2714"))

  # Check 'to_col' is a character vector of length one and exists in 'df'
  if (!is.character(to_col) || length(to_col) != 1) {
    stop("Invalid 'to_col' argument. 'to_col' must be a character vector of length one.")
  }

  if (! to_col %in% colnames(df)) {
    stop(paste0("The 'to_col' argument is not a column in 'df'."))
  }

  message(paste0("'", to_col, "' is a valid column in '", df_name, "'. \u2714"))

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
    stop("The 'string_col' argument is not a column in 'df'.")
  }

  if (!all(is.element(df[["string"]], c(0,1)))) {
    stop("The 'string_col' in 'df' contains values other than 0 or 1.")
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
    dplyr::select(dplyr::all_of(c(var_col,from_col,to_col))) |>
    dplyr::group_by(.data[[var_col]]) |>
    dplyr::summarize(across(
      .cols = dplyr::all_of(c(from_col,to_col)),
      .fns =  list(unique_obs = ~ length(.extract_unique(x = ., remove_na = remove_na, remove_empty = remove_empty))))) |>
    dplyr::ungroup() |>
    dplyr::mutate(is_not_equal_len = .data[[paste0(from_col,"_unique_obs")]] !=
                    .data[[paste0(to_col,"_unique_obs")]]) |>
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
