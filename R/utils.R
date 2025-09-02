
.tbl_key <- function(values_from, values_to, string = TRUE) {

  if (string) {
    values_from <- as.character(values_from)
    values_to <- as.character(values_to)
  } else {
    values_from <- as.numeric(values_from)
    values_to <- as.numeric(values_to)
  }

  purrr::map2(.x = values_from,
              .y = values_to,
              .f = ~ rlang::new_formula(.x, .y))
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
  stopifnot("'x' must be of type vector or factor." = is.vector(x))
  
  trimws(paste(unlist(strsplit(as.character(x), split = delim))))
}
