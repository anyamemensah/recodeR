
.tbl_key <- function(values, labels, string = TRUE) {

  if (string) {
    values <- as.character(values)
    labels <- as.character(labels)
  } else {
    values <- as.numeric(values)
    labels <- as.numeric(labels)
  }

  purrr::map2(.x = values,
              .y = labels,
              .f = ~ rlang::new_formula(.x, .y))
}


.find_two_sided <- function(dict,
                            current_col,
                            var_col = "var",
                            formula_pairs_col = "formula_pairs") {

  sub_dict <- dict[dict[[var_col]] == current_col, formula_pairs_col]

  unlist(sub_dict)
}


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


.extract_unique <-function(x, remove_na = FALSE, remove_empty = FALSE) {

  x_unique <- unique(x)

  if (remove_na) {
    x_unique <- x_unique[!is.na(x_unique)]
  }

  if (remove_empty) {
    x_unique <- stringr::str_squish(x_unique)
    x_unique <- x_unique[nzchar(x_unique)]
  }

  x_unique
}


.extract_values <- function(x, delim = ",") {
  stopifnot("\n\"x\" must be of type vector or factor." =
              any(class(x) %in% c("character", "numeric", "integer", "factor")))

  trimws(paste(unlist(strsplit(as.character(x), split = delim))))
}

