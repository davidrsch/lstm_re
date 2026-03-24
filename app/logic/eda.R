box::use(
  GGally[ggpairs, wrap],
  ggplot2[element_text, theme],
  pastecs[stat.desc],
)

# Generates a pairwise correlation and distribution plot for the uploaded
# dataset using GGally. Adds a centred title and uniform text sizing.
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

# Returns a descriptive statistics table for each column of the dataset,
# including min, max, mean, and standard deviation.
#' @export
database_summary <- function(data) {
  stat.desc(data)
}
