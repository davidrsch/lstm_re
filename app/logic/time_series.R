box::use(
  purrr[detect_index, map, map_int],
  scales[rescale],
  tseries[adf.test, pp.test],
)

# Slices the combined date-variables data frame to the window defined by the
# selected training start date (sttrain[ntrain]) and the test end date (endtt).
#' @export
create_ts <- function(date, variables, sttrain, ntrain, endtt) {
  df <- cbind(date, variables)
  start_date_char <- as.character(sttrain[ntrain, ][[1]])
  start <- which(as.character(df[[1]]) == start_date_char)
  end <- which(df[[1]] == endtt)

  if (length(start) == 0) {
    warning(paste0(
      "Start date '",
      start_date_char,
      "' not found in data. Returning NULL."
    ))
    return(NULL)
  }
  if (length(end) == 0) {
    warning(paste0("End date '", endtt, "' not found in data. Returning NULL."))
    return(NULL)
  }
  df <- df[start:end, ]
}

# Applies the minimum order of first differencing needed to make each series
# stationary (ADF and PP tests, p <= 0.05). Returns the differenced data frame
# with a trailing difv column recording the applied difference order.
#' @export
first_diff <- function(ts) {
  df <- ts[, -1, drop = FALSE]

  find_diff_order <- function(x) {
    order <- detect_index(seq_len(length(x)), function(i) {
      valuedif <- diff(x, differences = i)
      adf <- adf.test(valuedif, alternative = "stationary")
      pp <- pp.test(valuedif, alternative = "stationary")
      adf$p.value <= 0.05 && pp$p.value <= 0.05
    })
    if (order == 0L) length(x) else order
  }

  diffvalue <- max(map_int(df, find_diff_order))
  diffdf <- data.frame(diff(as.matrix(df), differences = diffvalue))
  names(diffdf) <- names(ts)[-1]
  valstrm <- 1:diffvalue
  date <- ts[[1]][-(valstrm)]
  difv <- rep(diffvalue, length(date))
  diffdf <- cbind(date, diffdf, difv)
  diffdf
}

# Applies a log transform followed by first differencing. Useful for series
# with exponential trends.
#' @export
second_diff <- function(ts) {
  logts <- log(ts[, -1, drop = FALSE])
  date <- ts[, 1, drop = FALSE]
  logdf <- cbind(date, logts)
  logdf <- first_diff(logdf)
  logdf
}

# Dispatches to the correct transformation function based on the key trf[ntrf].
# Keys: "original" (no change), "first" (first_diff), "second" (second_diff).
#' @export
create_transformed_ts <- function(ts, trf, ntrf) {
  if (trf[ntrf] == "original") {
    x <- ts
  } else {
    if (trf[ntrf] == "first") {
      x <- first_diff(ts)
    } else {
      x <- second_diff(ts)
    }
  }
  x
}

# Rescales all numeric columns of x to the interval [to[1], to[2]) using a
# single global min/max derived from the entire data frame.
#' @export
rescale_df <- function(x, to) {
  global_from <- c(min(x), max(x))
  result <- as.data.frame(map(x, function(col) {
    rescale(col, to = to, from = global_from)
  }))
  colnames(result) <- names(x)
  result
}

# Removes the difv bookkeeping column from a time-series data frame if present.
strip_difv <- function(ts) {
  if (any(names(ts) == "difv")) {
    ts[, -grep("difv", names(ts)), drop = FALSE]
  } else {
    ts
  }
}

# Applies the chosen scale transformation by key: "exact" (no scaling),
# "zero_one" ([0, 1]), "minus_plus" ([-1, 1]). Also strips the difv
# bookkeeping column before scaling.
#' @export
create_scaled_ts <- function(ts, sc, nsc) {
  if (sc[nsc] == "exact") {
    x <- strip_difv(ts)
  } else {
    if (sc[nsc] == "zero_one") {
      y <- strip_difv(ts)
      dftrsc <- y[, -1, drop = FALSE]
      date <- y[1, drop = FALSE]
      df <- rescale_df(x = dftrsc, to = c(0, 1))
      x <- cbind(date, df)
    } else {
      y <- strip_difv(ts)
      dftrsc <- y[, -1, drop = FALSE]
      date <- y[1, drop = FALSE]
      df <- rescale_df(dftrsc, c(-1, 1))
      x <- cbind(date, df)
    }
  }
  x
}
