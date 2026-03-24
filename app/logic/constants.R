box::use(
  tibble[tribble],
)

#' @export
file_formats <- tribble(
  ~type       ,
  ~extensions ,
  "text"      ,
  "csv"       ,
  "text"      ,
  "tsv"       ,
  "excel"     ,
  "xlsx"      ,
  "excel"     ,
  "xls"
)

#' @export
status_mapping <- tribble(
  ~type     , ~color   , ~icon       ,
  "error"   , "red"    , "Error"     ,
  "info"    , "blue"   , "Info"      ,
  "success" , "green"  , "CheckMark" ,
  "warning" , "yellow" , "Warning"
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
