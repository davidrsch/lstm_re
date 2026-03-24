box::use(
  abind[abind, adrop],
  dplyr[arrange],
  Metrics[mae, mse, rmse],
  purrr[map, map_dbl, map_dfc, reduce],
  scales[rescale],
  stats[diffinv, predict],
)

# Collapses a 3-D array (samples × steps × features) into a 2-D matrix by
# concatenating sample slices along the row dimension.
#' @export
array_3d_to_2d <- function(vec3d) {
  seq_len(dim(vec3d)[1]) |>
    map(function(i) adrop(vec3d[i, , , drop = FALSE], drop = 1)) |>
    reduce(function(acc, x) abind(acc, x, along = 1))
}

# Inverts applied differencing on a 3-D prediction array using the last known
# values from the input tensor. Returns only the forward-predicted steps.
#' @export
diff_inverse_3d <- function(data3d, difference, lastknow3d) {
  data2d <- array_3d_to_2d(data3d)
  lastknow2d <- array_3d_to_2d(lastknow3d)
  datastepspersample <- dim(data2d)[1] / dim(data3d)[1]
  lastknowsteppersample <- dim(lastknow2d)[1] / dim(lastknow3d)[1]

  invertedarray <- seq_len(dim(data3d)[1]) |>
    map(function(sample) {
      datastartsample <- ((datastepspersample * sample) - datastepspersample) +
        1
      dataendsample <- datastepspersample * sample
      data <- data2d[datastartsample:dataendsample, , drop = FALSE]
      lastknowendsample <- lastknowsteppersample * sample
      lastknow <- lastknow2d[
        (lastknowendsample - difference + 1):lastknowendsample,
        ,
        drop = FALSE
      ]
      lastknow <- as.matrix(lastknow)
      invert2d <- diffinv(data, differences = difference, xi = lastknow)
      invert3d <- invert2d
      dim(invert3d) <- c(1, dim(invert2d)[1], dim(invert2d)[2])
      invert3d
    }) |>
    reduce(function(acc, x) abind(acc, x, along = 1))

  stepstoselect <- (dim(invertedarray)[2] - dim(data3d)[2] + 1):dim(
    invertedarray
  )[2]
  invertedarray[, stepstoselect, , drop = FALSE]
}

# Runs LSTM predictions for every sample, then reverses any scaling and
# transformation (differencing/log) applied during pre-processing to recover
# predictions in original units.
#' @export
predict_with_keras <- function(
  model,
  inputs,
  outputs,
  lastvaluesout,
  scale,
  transformation,
  transf_ts
) {
  seq_len(dim(inputs)[1]) |>
    map(function(samples) {
      predictions <- predict(model, inputs[samples, , , drop = FALSE])
      if (scale == "zero_one") {
        predictions <- rescale(
          predictions,
          to = c(min(transf_ts[, -1]), max(transf_ts[, -1])),
          from = c(0, 1)
        )
      } else if (scale == "minus_plus") {
        predictions <- rescale(
          predictions,
          to = c(min(transf_ts[, -1]), max(transf_ts[, -1])),
          from = c(-1, 1)
        )
      }
      if (transformation == "first") {
        predictions <- diff_inverse_3d(
          data3d = predictions,
          difference = transf_ts[[dim(transf_ts)[2]]][1],
          lastknow3d = lastvaluesout[samples, , , drop = FALSE]
        )
      } else if (transformation == "second") {
        predictions <- diff_inverse_3d(
          data3d = predictions,
          difference = transf_ts[[dim(transf_ts)[2]]][1],
          lastknow3d = lastvaluesout[samples, , , drop = FALSE]
        )
        predictions <- exp(predictions)
      }
      predictions
    }) |>
    reduce(function(acc, x) abind(acc, x, along = 1))
}

# Returns a length-3 numeric vector of MSE, RMSE, and MAE for the given
# actual/predicted pair.
#' @export
get_metrics <- function(actual, predicted) {
  obtained_mse <- mse(actual, predicted)
  obtained_rmse <- rmse(actual, predicted)
  obtained_mae <- mae(actual, predicted)
  x <- c(obtained_mse, obtained_rmse, obtained_mae)
  x
}

# Aggregates 3-D prediction arrays into a data frame of per-date min, mean,
# and max prediction values across all samples, one set of columns per output
# variable.
#' @export
create_plot_pred_df <- function(threddata, xdata, colnames) {
  date_slice <- as.data.frame(as.matrix(threddata[,, 1]))

  map_dfc(seq_along(colnames), function(col) {
    predictions_list <- map(seq_along(xdata), function(date) {
      rowcolar <- which(date_slice == xdata[date], arr.ind = TRUE) |>
        as.data.frame() |>
        arrange(row)
      predict <- map_dbl(seq_len(nrow(rowcolar)), function(k) {
        as.matrix(threddata[rowcolar$row[k], rowcolar$col[k], 1 + col])[[1]]
      })
      length_diff <- dim(threddata)[2] - length(predict)
      if (length_diff > 0) {
        predict <- c(predict, rep(NaN, length_diff))
      }
      predict
    })
    predictions <- do.call(rbind, predictions_list)

    minname <- paste0(colnames[col], "MIN")
    meanname <- paste0(colnames[col], "MEAN")
    maxname <- paste0(colnames[col], "MAX")
    datapred <- data.frame(
      min = apply(predictions, 1, min, na.rm = TRUE),
      mean = apply(predictions, 1, mean, na.rm = TRUE),
      max = apply(predictions, 1, max, na.rm = TRUE)
    )
    names(datapred) <- c(minname, meanname, maxname)
    datapred
  })
}
