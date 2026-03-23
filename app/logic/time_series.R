box::use(
  scales[rescale],
  tseries[adf.test, pp.test],
)

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

#' @export
first_diff <- function(ts) {
  df <- ts[, -1, drop = FALSE]
  for (variables in seq_len(dim(df)[2])) {
    for (i in seq_len(dim(df)[1])) {
      valuedif <- diff(df[[variables]], differences = i)
      adf <- adf.test(valuedif, alternative = "stationary")
      pp <- pp.test(valuedif, alternative = "stationary")
      if (adf$p.value <= 0.05 && pp$p.value <= 0.05) {
        valuedif <- i
        break
      } else {}
    }
    if (variables == 1) {
      diffvalue <- valuedif
    } else {
      diffvalue <- c(diffvalue, valuedif)
    }
  }
  diffvalue <- max(diffvalue)
  diffdf <- data.frame(diff(as.matrix(df), differences = diffvalue))
  names(diffdf) <- names(ts)[-1]
  valstrm <- 1:diffvalue
  date <- ts[[1]][-(valstrm)]
  difv <- rep(diffvalue, length(date))
  diffdf <- cbind(date, diffdf, difv)
  diffdf
}

#' @export
second_diff <- function(ts) {
  logts <- log(ts[, -1, drop = FALSE])
  date <- ts[, 1, drop = FALSE]
  logdf <- cbind(date, logts)
  logdf <- first_diff(logdf)
  logdf
}

#' @export
create_transformed_ts <- function(ts, trf, ntrf) {
  if (trf[ntrf] == "Original") {
    x <- ts
  } else {
    if (trf[ntrf] == "First transformation") {
      x <- first_diff(ts)
    } else {
      x <- second_diff(ts)
    }
  }
  x
}

#' @export
rescale_df <- function(x, to) {
  for (variable in seq_len(dim(x)[2])) {
    df <- rescale(x[[variable]], to, from = c(min(x), max(x)))
    if (variable == 1) {
      datfra <- data.frame(df)
    } else {
      datfra <- cbind(datfra, df)
    }
  }
  colnames(datfra) <- names(x)
  datfra
}

#' @export
create_scaled_ts <- function(ts, sc, nsc) {
  if (sc[nsc] == "Exact") {
    if (any(names(ts) == "difv")) {
      x <- ts[, -grep("difv", names(ts)), drop = FALSE]
    } else {
      x <- ts
    }
  } else {
    if (sc[nsc] == "From 0 to 1") {
      if (any(names(ts) == "difv")) {
        y <- ts[, -grep("difv", names(ts)), drop = FALSE]
      } else {
        y <- ts
      }
      dftrsc <- y[, -1, drop = FALSE]
      date <- y[1, drop = FALSE]
      df <- rescale_df(x = dftrsc, to = c(0, 1))
      x <- cbind(date, df)
    } else {
      if (any(names(ts) == "difv")) {
        y <- ts[, -grep("difv", names(ts)), drop = FALSE]
      } else {
        y <- ts
      }
      dftrsc <- y[, -1, drop = FALSE]
      date <- y[1, drop = FALSE]
      df <- rescale_df(dftrsc, c(-1, 1))
      x <- cbind(date, df)
    }
  }
  x
}
