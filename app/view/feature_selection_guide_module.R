box::use(
  shiny.fluent[Pivot, PivotItem, Stack],
  shiny[moduleServer, NS, tags],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  make_card(
    "Feature selection guide",
    Stack(
      Pivot(
        id = ns("Proposal_tabsetPanel"),
        PivotItem(
          headerText = "Time series",
          tags$iframe(
            src = "static/qmds/time_series.html",
            width = "100%",
            height = "720px",
            frameBorder = "0"
          )
        ),
        PivotItem(
          headerText = "Training vectors",
          tags$iframe(
            src = "static/qmds/training_vectors.html",
            width = "100%",
            height = "720px",
            frameBorder = "0"
          )
        ),
        PivotItem(
          headerText = "Models",
          tags$iframe(
            src = "static/qmds/models.html",
            width = "100%",
            height = "720px",
            frameBorder = "0"
          )
        ),
        PivotItem(
          headerText = "Training",
          tags$iframe(
            src = "static/qmds/training.html",
            width = "100%",
            height = "720px",
            frameBorder = "0"
          )
        )
      )
    ),
    style = "max-height: 808px; background-color: white;",
    is_contained = TRUE
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
