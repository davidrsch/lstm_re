box::use(
  shiny.fluent[fluentPage, Pivot, PivotItem],
  shiny[moduleServer, NS, reactiveValues],
  shinyjs[useShinyjs],
)

box::use(
  app / view / results_display,
  app / view / selecting_features,
  app / view / upload_data,
  app / view / welcome,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluentPage(
    useShinyjs(),
    Pivot(
      id = "Main_tabsetpanel",
      PivotItem(headerText = "Welcome", welcome$ui(ns("welcome"))),
      PivotItem(headerText = "Upload Data", upload_data$ui(ns("upload_data"))),
      PivotItem(
        headerText = "Selecting Features",
        selecting_features$ui(ns("selecting_features"))
      ),
      PivotItem(
        headerText = "Results",
        results_display$ui(ns("results"))
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
      show_eda = 0,
      x_data = NULL,
      previous_x_data = NULL,
      start_train_levels = NULL,
      selected_trains = data.frame(),
      show_graphs = 0,
      upload_card_visible = TRUE,
      variables_card_visible = FALSE,
      data_amount_card_visible = FALSE,
      transf = c("Original", "First transformation", "Second transformation"),
      scales = c("Exact", "From 0 to 1", "From -1 to 1"),
      input_amounts = NULL,
      std_input_amounts = NULL,
      lstm_amounts = NULL,
      std_lstm_amounts = NULL,
      neuron_amounts = NULL,
      std_neuron_amounts = NULL,
      models_table = NULL,
      run_experiment = NULL,
      selected_date_variable = "",
      test_start_date = NULL,
      test_end_date = NULL,
      set_seed = FALSE,
      seed = NULL
    )

    upload_data$server("upload_data", shared_data)
    selecting_features$server("selecting_features", shared_data)
    results_display$server("results", shared_data)
  })
}
