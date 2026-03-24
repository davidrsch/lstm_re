box::use(
  tibble[tibble],
)

#' @export
file_formats <- tibble(
  type = c("text", "text", "excel", "excel"),
  extensions = c("csv", "tsv", "xlsx", "xls")
)

#' @export
status_mapping <- tibble(
  type = c("error", "info", "success", "warning"),
  color = c("red", "blue", "green", "yellow"),
  icon = c("Error", "Info", "CheckMark", "Warning")
)

#' @export
transformations <- list(
  list(key = "original", text = "Original"),
  list(key = "first", text = "First transformation"),
  list(key = "second", text = "Second transformation")
)

#' @export
scales <- list(
  list(key = "exact", text = "Exact"),
  list(key = "zero_one", text = "From 0 to 1"),
  list(key = "minus_plus", text = "From -1 to 1")
)
