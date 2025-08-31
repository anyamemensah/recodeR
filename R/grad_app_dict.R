#' @title Sample Data Dictionary Flat File
#'
#' @description This tibble represents a sample data dictionary. Each row is associated
#' with a unique value/label pair for a column in the `grad_app` dataset.
#'
#' @format A data.frame with 116 rows and 6 columns:
#' \describe{
#' \item{column_name}{names of variables to recode in the `grad_app` dataset.}
#' \item{column_description}{description of the columns to recode in the `grad_app`
#' dataset.}
#' \item{old_values}{old values to recode for each column}
#' \item{new_labels}{new values/labels to apply to each column}
#' \item{default}{default value to use when a matching old value is not found.}
#' \item{string}{whether the new values to be mapped onto the variable are string.}
#' }
"grad_app_dict"
