#' @export
max_min_width_input <- function(x) {
  list(
    root = list(
      "max-width" = paste0(x, "%"),
      "min-width" = paste0(x, "%")
    )
  )
}
