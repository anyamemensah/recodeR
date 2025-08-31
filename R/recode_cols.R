#' @title Recode columns/variables in a dataset
#'
#' @description Recodes values in an atomic vector using a 'recoding' dictionary, which
#' comprises of a series of two-sided formulas to define a mapping from old values to
#' new values.
#' @param df A data.frame or tibble.
#' @param list_of_dicts A list of 'recoding' dictionaries.
#' @param var_col A character string of the name of the column in `list_of_dicts` that contains
#' unique variable/column names in your dataset.
#' @param formula_pairs_col A character string of the name of the column in `list_of_dicts` that
#' contains a sequence of two-sided formulas for recoding variable values.
#' @param default_col A character string of the name of the column in `list_of_dicts` that holds
#' default values to use when a matching (old) value is not found.
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
          .fn = ~ .return_recoded_vctr(
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
                                 current_col,
                                 dict,
                                 var_col = "var",
                                 formula_pairs_col = "formula_pairs",
                                 default = NULL,
                                 default_col = NULL,
                                 string = NULL,
                                 string_col = NULL) {

  two_sided_formulas <-
    .find_two_sided(
      dict = dict,
      current_col = current_col,
      var_col = var_col,
      formula_pairs_col = formula_pairs_col
    )

  if (!is.null(default_col) && is.null(default)) {
    default_result <- dict[dict[[var_col]] == current_col, ][[default_col]]
    default_result <- unique(default_result)

    # If default values are found
    if (length(default_result) > 0) {
      if (length(default_result) > 1) {
        # Warn if more than one default value found
        warning(
          paste0(
            "More than one unique default value found for ",
            current_col,
            ". First entry is being used."
          )
        )
      }
      # Set default value
      default_val <- default_result[1]
    } else {
      # If no default value is found then NA is used
      default_val <- NA
    }
  } else {
    # Use default if provided or NA if default is NULL
    default_val <- default %||% NA
  }

  string <-
    if (is.null(string) && is.null(string_col)) {
      TRUE
    } else if (!is.null(string)) {
      string
    } else {
      dict[dict[[var_col]] == current_col, ][[string_col]]
    }

  current_vec <- {
    if (string) {
      as.character(x)
    } else {
      x
    }
  }

  dplyr::case_match(.x = current_vec, !!!two_sided_formulas, .default = {
    if (is.na(default_val)) {
      NA
    } else if (default_val != ".x") {
      default_val
    } else {
      current_vec
    }
  })
}

