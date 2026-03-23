box::use(
  GGally[ggpairs, wrap],
  ggplot2[element_text, theme],
  pastecs[stat.desc],
)

#' @export
plot_eda <- function(data) {
  p <- ggpairs(
    data,
    title = "Exploratory Data Analysis",
    lower = list(
      continuous = wrap(
        "points",
        colour = "blue"
      )
    ),
    diag = list(
      continuous = wrap(
        "densityDiag",
        color = "black",
        fill = "blue",
        alpha = 0.5
      )
    ),
    upper = list(
      continuous = wrap(
        "cor",
        size = 6
      )
    )
  ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 15)
    )
  p
}

#' @export
database_summary <- function(data) {
  stat.desc(data)
}
