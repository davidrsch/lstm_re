box::use(
  tibble[tribble],
)

#' @export
file_formats <- tribble(
  ~type,
  ~extensions,
  "text",
  "csv",
  "text",
  "tsv",
  "excel",
  "xlsx",
  "excel",
  "xls"
)
