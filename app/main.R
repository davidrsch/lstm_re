box::use(
  shiny.fluent[fluentPage, Pivot, PivotItem],
  shiny[moduleServer, NS, reactiveValues],
  shinyjs[useShinyjs],
)

box::use(
  app / view / results,
  app / view / selecting_features,
  app / view / upload_data,
  app / view / wellcome,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    useShinyjs(),
    Pivot(
      id = "Main_tabsetpanel",
      PivotItem(headerText = "Welcome", wellcome$ui(ns("wellcome"))),
      PivotItem(headerText = "Upload Data", upload_data$ui(ns("upload_data"))),
      PivotItem(
        headerText = "Selecting Features",
        selecting_features$ui(ns("selecting_features"))
      ),
      PivotItem(
        headerText = "Results",
        results$ui(ns("results"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    shared_data <- reactiveValues(
      df = data.frame(),
      grid = data.frame(),
      EDA = data.frame(),
      showEDA = 0,
      x_data = NULL,
      previousx_data = NULL,
      starttrainlevels = NULL,
      selectedtrains = data.frame(),
      showGraphs = 0,
      upload_card_visible = TRUE,
      variables_card_visible = FALSE,
      data_amount_card_visible = FALSE,
      transf = c("Original", "First transformation", "Second transformation"),
      inputamnts = NULL,
      stdinputamnts = NULL,
      LSTMamnts = NULL,
      stdLSTMamnts = NULL,
      neuronsamnts = NULL,
      stdneuronsamnts = NULL,
      modelstable = NULL,
      run_experiment = NULL,
      selected_date_variable = "",
      test_start_date = NULL,
      test_end_date = NULL,
      setseed = FALSE,
      seed = NULL
    )

    upload_data$server("upload_data", shared_data)
    selecting_features$server("selecting_features", shared_data)
    results$server("results", shared_data)
  })
}
