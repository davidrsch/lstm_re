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
  list(key = "Original", text = "Original"),
  list(key = "First transformation", text = "First transformation"),
  list(key = "Second transformation", text = "Second transformation")
)

#' @export
scales <- list(
  list(key = "Exact", text = "Exact"),
  list(key = "From 0 to 1", text = "From 0 to 1"),
  list(key = "From -1 to 1", text = "From -1 to 1")
)
