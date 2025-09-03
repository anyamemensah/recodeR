#' @title Recode variable values
#'
#' @description `recode_cols()` recodes the values of variables in a data frame using a 'recoding' 
#' dictionary, which comprises of a series of two-sided formulas to define a mapping from old values 
#' to new ones
#' 
#' @param df A data frame.
#' @param list_of_dicts A list of 'recoding' dictionaries.
#' @param var_col A character string of the name of the column in `list_of_dicts` that contains 
#' unique variable names in your dataset.
#' @param formula_pairs_col A character string of the name of the column in `list_of_dicts` that
#' contains a sequence of two-sided formulas for recoding variable values.
#' @param default_col A character string of the name of the column in `list_of_dicts` that contains
#' a fallback value for any unmatched original values.
#' @param string_col A character string of the name of the column in `list_of_dicts` that
#' indicates whether the new values to be mapped onto the variable are string.
#'
#' @return A tibble
#'
#' @export
recode_cols <- function(df,
                        list_of_dicts,
                        var_col = "var",
                        formula_pairs_col = "formula_pairs",
                        default_col = "default",
                        string_col = "string") {

  for (dict in list_of_dicts) {
    if (!is.null(dict)) {
      df <- df |>
        dplyr::mutate(dplyr::across(
          .cols = dplyr::all_of(unique(dict[[var_col]])),
          .fn = ~ .find_recoding_entry(
            x = df[[dplyr::cur_column()]],
            current_col = dplyr::cur_column(),
            dict = dict,
            var_col = var_col,
            formula_pairs_col = formula_pairs_col,
            default_col = default_col,
            string_col = string_col
          )
        )
      )
    }
  }

  df
}


#' @keywords internal
.return_recoded_vctr <- function(x,
                                 formula_pairs,
                                 default = NULL,
                                 string = NULL) {
  
  # Check 'x' is a vector
  if (!is.vector(x)) {
    stop("'x' must be a vector with a length of at least one.")
  }
  
  # Check all elements of 'formula_pairs' inherit from 'formula'
  all_formula <- vapply(formula_pairs, rlang::is_formula, logical(1))
  
  if (!all(all_formula)) {
    stop("Invalid 'formula_pairs' argument. 'formula_pairs' must inherit from 'formula'.")
  }
  
  # Default string to TRUE if NULL
  if (is.null(string)) {
    string <- TRUE
  } 
  
  # Check for 'x' as 'string'
  if (string && !is.character(x)) {
    x <- as.character(x)
  } 
  
  # Check 'default'
  if (!is.null(default)) {
    default_result <- unique(default)
    # If 'default' values are found
    if (length(default_result) > 0) {
      if (length(default_result) > 1) {
        # Warn if more than one 'default' value found
        warning("Multiple unique default values detected. Using the first entry.")
      }
      # Set 'default' value
      default_val <- default_result[1]
    } else {
      # If no 'default' value is found then NA is used
      default_val <- NA
    }
  } else {
    # Use 'default' if provided or NA if 'default' is NULL
    default_val <- default %||% NA
  }
  
  dplyr::case_match(.x = x, !!!formula_pairs, .default = {
    if (is.na(default_val)) {
      NA
    } else if (default_val != ".x") {
      default_val
    } else {
      x
    }
  })

}


#' @keywords internal
.find_recoding_entry <- function(x,
                                 current_col,
                                 dict,
                                 var_col = "var",
                                 formula_pairs_col = "formula_pairs",
                                 default_col = "default",
                                 string_col = "string") {
  

  current_entry <- 
    dict |> dplyr::filter(!!rlang::sym(var_col) == current_col)
  
  .return_recoded_vctr(x = x, 
                       formula_pairs = unlist(current_entry[[formula_pairs_col]]),
                       default = current_entry[[default_col]], 
                       string = current_entry[[string_col]]
                       )
  
}

  