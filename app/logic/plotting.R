box::use(
  plotly[config, plot_ly],
  shiny[div, tagList],
)

#' @export
sets_vars_plots <- function(
  selected_trains,
  eda,
  x_data,
  select_test_start,
  select_test_end
) {
  # Ensure eda columns are numeric
  eda[] <- lapply(eda, function(x) as.numeric(as.character(x)))

  sv_plots <- list()
  for (i1 in seq_len(dim(selected_trains)[1])) {
    plot_data <- data.frame()
    for (i2 in seq_len(dim(eda)[2])) {
      stn <- which(x_data == selected_trains[i1, ])
      stt <- which(x_data == select_test_start)
      ett <- which(x_data == select_test_end)

      if (length(stn) > 0 && length(stt) > 0) {
        # The levels of x_data are used to order the x-axis
        x1 <- x_data[stn:stt]
        y1 <- eda[stn:stt, i2][[1]]
        train_df <- data.frame(
          x = factor(x1, levels = unique(x_data)),
          y = y1,
          variable = colnames(eda)[i2],
          type = "train"
        )
        plot_data <- rbind(plot_data, train_df)
      }

      if (length(stt) > 0 && length(ett) > 0) {
        x2 <- x_data[stt:ett]
        y2 <- eda[stt:ett, i2][[1]]
        test_df <- data.frame(
          x = factor(x2, levels = unique(x_data)),
          y = y2,
          variable = colnames(eda)[i2],
          type = "test"
        )
        plot_data <- rbind(plot_data, test_df)
      }
    }

    if (nrow(plot_data) > 0) {
      p <- plot_ly(
        plot_data,
        x = ~x,
        y = ~y,
        color = ~variable,
        linetype = ~type,
        linetypes = c("train" = "solid", "test" = "dash"),
        type = "scatter",
        mode = "lines"
      ) |>
        config(displayModeBar = FALSE)

      if (!(i1 %% 2) == 0) {
        element <- div(style = "float:left; width:48%;", p)
      } else {
        element <- div(style = "float:right; width:48%;", p)
      }
      sv_plots[[i1]] <- element
    }
  }

  tagList(sv_plots)
}
