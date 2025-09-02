box::use(
  abind[abind, adrop],
  dplyr[arrange],
  Metrics[mae, mse, rmse],
  scales[rescale],
  stats[diffinv, predict],
)

#' @export
from3dto2d <- function(vec3d) {
  for (i in seq_len(dim(vec3d)[1])) {
    x1 <- adrop(vec3d[i, , , drop = FALSE], drop = 1)
    if (i == 1) {
      x <- x1
    } else {
      x <- abind(x, x1, along = 1)
    }
  }
  x
}

#' @export
diffinv3d <- function(data3d, difference, lastknow3d) {
  data2d <- from3dto2d(data3d)
  lastknow2d <- from3dto2d(lastknow3d)
  datastepspersample <- dim(data2d)[1] / dim(data3d)[1]
  lastknowsteppersample <- dim(lastknow2d)[1] / dim(lastknow3d)[1]
  for (sample in seq_len(dim(data3d)[1])) {
    datastartsample <- ((datastepspersample * sample) - datastepspersample) + 1
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
    if (sample == 1) {
      invertedarray <- invert3d
    } else {
      invertedarray <- abind(invertedarray, invert3d, along = 1)
    }
  }
  stepstoselect <- (dim(invertedarray)[2] - dim(data3d)[2] + 1):dim(
    invertedarray
  )[2]
  invertedarray <- invertedarray[, stepstoselect, , drop = FALSE]
  invertedarray
}

#' @export
predictwkeras <- function(
  model,
  inputs,
  outputs,
  lastvaluesout,
  trueoutputs,
  scale,
  transformation,
  transf_ts
) {
  modeltouse <- model
  for (samples in seq_len(dim(inputs)[1])) {
    inputss <- inputs[samples, , , drop = FALSE]
    predictions <- predict(modeltouse, inputss)
    if (scale == "Exact") {
      predictions <- predictions
    } else {
      if (scale == "From 0 to 1") {
        predictions <- rescale(
          predictions,
          to = c(min(transf_ts[, -1]), max(transf_ts[, -1])),
          from = c(0, 1)
        )
      } else {
        predictions <- rescale(
          predictions,
          to = c(min(transf_ts[, -1]), max(transf_ts[, -1])),
          from = c(-1, 1)
        )
      }
    }
    if (transformation == "Original") {
      predictions <- predictions
    } else {
      if (transformation == "First transformation") {
        predictions <- diffinv3d(
          data3d = predictions,
          difference = transf_ts[[dim(transf_ts)[2]]][1],
          lastknow3d = lastvaluesout[samples, , , drop = FALSE]
        )
      } else {
        predictions <- diffinv3d(
          data3d = predictions,
          difference = transf_ts[[dim(transf_ts)[2]]][1],
          lastknow3d = lastvaluesout[samples, , , drop = FALSE]
        )
        predictions <- exp(predictions)
      }
    }
    if (samples == 1) {
      predarray <- predictions
    } else {
      predarray <- abind(predarray, predictions, along = 1)
    }
  }
  predarray
}

#' @export
gettingmetrics <- function(actual, predicted) {
  obtained_mse <- mse(actual, predicted)
  obtained_rmse <- rmse(actual, predicted)
  obtained_mae <- mae(actual, predicted)
  x <- c(obtained_mse, obtained_rmse, obtained_mae)
  x
}

#' @export
creatingplotpreddf <- function(threddata, xdata, colnames) {
  for (col in seq_along(colnames)) {
    for (date in seq_along(xdata)) {
      data3d <- as.data.frame(as.matrix(threddata[, , 1]))
      rowcolar <- which(data3d == xdata[date], arr.ind = TRUE)
      rowcolar <- as.data.frame(rowcolar)
      rowcolar <- arrange(rowcolar, row)
      for (combofrowcol in seq_len(dim(rowcolar)[1])) {
        rowcol <- as.vector(as.matrix(rowcolar[combofrowcol, ]))
        pred <- as.matrix(threddata[rowcol[1], rowcol[2], 1 + col])[[1]]
        if (combofrowcol == 1) {
          predict <- pred
        } else {
          predict <- c(predict, pred)
        }
      }
      if (length(predict) < dim(threddata)[2]) {
        predict <- c(predict, rep(NaN, dim(threddata)[2] - length(predict)))
      }
      if (date == 1) {
        predictions <- data.frame(t(predict))
      } else {
        predictions <- rbind(predictions, predict)
      }
    }
    min <- apply(
      predictions[, seq_len(dim(predictions)[2]), drop = FALSE],
      1,
      function(predictions) {
        min(predictions, na.rm = TRUE)
      }
    )
    mean <- apply(
      predictions[, seq_len(dim(predictions)[2]), drop = FALSE],
      1,
      function(predictions) {
        mean(predictions, na.rm = TRUE)
      }
    )
    max <- apply(
      predictions[, seq_len(dim(predictions)[2]), drop = FALSE],
      1,
      function(predictions) {
        max(predictions, na.rm = TRUE)
      }
    )
    datapred <- data.frame(min, mean, max)
    minname <- paste0(colnames[col], "MIN")
    meanname <- paste0(colnames[col], "MEAN")
    maxname <- paste0(colnames[col], "MAX")
    names(datapred) <- c(minname, meanname, maxname)
    if (col == 1) {
      datapredfin <- datapred
    } else {
      datapredfin <- cbind(datapredfin, datapred)
    }
  }
  datapredfin
}
