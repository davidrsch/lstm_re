box::use(
  abind[abind],
  runner[runner],
)

#' @export
create_3d_vector <- function(data, steps, datasample) {
  if (is.data.frame(data) || is.matrix(data)) {
    for (column in seq_len(dim(data)[2])) {
      rollingwin <- runner(
        data[datasample[1]:datasample[2], column],
        k = steps,
        na_pad = TRUE
      )
      rollingwin <- rollingwin[steps:length(datasample[1]:datasample[2])]
      rollingwin <- t(matrix(
        unlist(rollingwin),
        ncol = length(datasample[1]:datasample[2]) - steps + 1,
        nrow = steps
      ))
      if (column == 1) {
        threedrw <- abind(rollingwin, along = 3)
      } else {
        threedrw <- abind(threedrw, rollingwin, along = 3)
      }
    }
    dimnames(threedrw) <- list(NULL, NULL, colnames(data))
  }
  threedrw
}

#' @export
which_equal_vec <- function(vec, equalto) {
  for (i in seq_along(equalto)) {
    if (i == 1) {
      x <- which(vec == equalto[i])
    } else {
      x <- c(x, which(vec == equalto[i]))
    }
  }
  x <- sort(x)
  x
}
