#' @title Sample Delimited Data Dictionary
#'
#' @description This tibble represents a sample delimited data dictionary.
#' Both old values and new labels are separated by semi-colons ;.
#'
#' @format A data.frame with 10 rows and 6 columns:
#' \describe{
#' \item{column_name}{names of variables to recode in the `grad_app` dataset.}
#' \item{column_description}{description of the columns to recode in the `grad_app`
#' dataset.}
#' \item{old_values}{old values to recode for each column separated by a semi colon.}
#' \item{new_labels}{new values/labels to apply to each column separated by a semi colon.}
#' \item{default}{default value to use when a matching old value is not found.}
#' \item{string}{whether the new values to be mapped onto the variable are string.}
#' }
"grad_app_dict_delim"
