box::use(
  abind[abind],
  purrr[map, reduce],
  runner[runner],
)

# Converts a 2-D data frame or matrix into a 3-D rolling-window tensor of
# shape (samples, steps, features) suitable for LSTM input.
# - datasample: two-element vector of [start_row, end_row] indices
#' @export
create_3d_vector <- function(data, steps, datasample) {
  if (is.data.frame(data) || is.matrix(data)) {
    n_obs <- length(datasample[1]:datasample[2])
    threedrw <- seq_len(dim(data)[2]) |>
      map(function(column) {
        rollingwin <- runner(
          data[datasample[1]:datasample[2], column],
          k = steps,
          na_pad = TRUE
        )
        rollingwin <- rollingwin[steps:n_obs]
        t(matrix(unlist(rollingwin), ncol = n_obs - steps + 1, nrow = steps))
      }) |>
      reduce(function(acc, x) abind(acc, x, along = 3))
    dimnames(threedrw) <- list(NULL, NULL, colnames(data))
  } else {
    stop("'data' must be a data frame or matrix.")
  }
  threedrw
}

# Returns the sorted positions within vec that match any element in equalto.
#' @export
which_equal_vec <- function(vec, equalto) {
  sort(which(vec %in% equalto))
}
