box::use(
  DT[datatable, renderDataTable],
  shiny.fluent[DefaultButton.shinyInput, PrimaryButton.shinyInput, Stack],
  shiny.fluent[Modal, reactOutput, renderReact],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, reactiveValues],
  shiny[req],
  shinyalert[shinyalert],
  stats[runif],
)


box::use(
  app / logic / ui_helpers[find_models, select_models_to_build],
  app / view / feature_selection_guide_module,
  app / view / models_options_card_module,
  app / view / training_options_card_module,
  app / view / training_vectors_card_module,
  app / view / ts_transformations_card_module,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    div(
      class = "ms-Grid-row",
      style = "display: flex; flex-wrap: wrap;",
      div(
        class = "ms-Grid-col ms-sm4",
        ts_transformations_card_module$ui(ns("ts_transformations_card")),
        training_vectors_card_module$ui(ns("training_vectors_card")),
        models_options_card_module$ui(ns("models_options_card")),
        training_options_card_module$ui(ns("training_options_card")),
        Stack(
          horizontal = TRUE,
          tokens = list(childrenGap = "10%"),
          PrimaryButton.shinyInput(ns("startexperimentation"), "Start"),
          div(style = "flex-grow: 1;"),
          DefaultButton.shinyInput(ns("repeatexperimentation"), "Repeat")
        )
      ),
      div(
        class = "ms-Grid-col ms-sm8",
        feature_selection_guide_module$ui(ns("feature_selection_guide")),
      )
    ),
    reactOutput(ns("modal"))
  )
}

#' @export
server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    modal_visible <- reactiveVal(FALSE)

    startalert <- paste0(
      "Please make sure that you have selected a time series, a scale, a ",
      "temporal horizon, an input amount for training vectors, an amount ",
      "of LSTM layers per model, an amount of neurons per LSTM layer, and ",
      "an epoch amount."
    )

    visibility <- reactiveValues(
      ts_transformations = TRUE,
      training_vectors = FALSE,
      models_options = FALSE,
      training_options = FALSE
    )

    # The reactiveValues are now in app/main.R

    ts_transformations_card_module$server(
      "ts_transformations_card",
      shared_data,
      visibility
    )
    training_vectors_card_module$server(
      "training_vectors_card",
      shared_data,
      visibility
    )
    models_options_card_module$server(
      "models_options_card",
      shared_data,
      visibility
    )
    training_options_card_module$server(
      "training_options_card",
      shared_data,
      visibility
    )
    feature_selection_guide_module$server("feature_selection_guide")

    observeEvent(input$startexperimentation, {
      if (
        is.null(shared_data$transf) ||
          is.null(shared_data$scales) ||
          is.na(shared_data$temporalhorizon) ||
          is.null(shared_data$std_input_amounts) ||
          is.null(shared_data$std_lstm_amounts) ||
          is.null(shared_data$std_neuron_amounts) ||
          is.na(shared_data$epoch)
      ) {
        shinyalert(html = TRUE, text = startalert, type = "error")
      } else {
        if (shared_data$epoch < 1 || !is.integer(shared_data$epoch)) {
          shinyalert(
            "Error",
            "Wrong epoch format, most be an integer number bigger than 0",
            type = "error"
          )
        } else {
          if (shared_data$set_seed == TRUE && is.na(shared_data$seed)) {
            shinyalert(
              "Error",
              "If a seed is going to be used a seed most be specified",
              type = "error"
            )
          } else {
            modal_visible(TRUE)
            shared_data$models_table <- find_models(
              shared_data$std_lstm_amounts,
              shared_data$std_neuron_amounts
            )
          }
        }
      }
    })

    output$modal <- renderReact({
      if (modal_visible()) {
        Modal(
          isOpen = modal_visible(),
          isBlocking = FALSE,
          select_models_to_build(
            ns = ns,
            train = shared_data$selected_trains,
            ts = shared_data$transf,
            sc = shared_data$scales,
            vec = shared_data$std_input_amounts,
            lstm = shared_data$std_lstm_amounts,
            neu = shared_data$std_neuron_amounts
          )
        )
      }
    })

    output$modelestable <- renderDataTable({
      req(shared_data$models_table)
      datatable(
        shared_data$models_table,
        options = list(
          dom = "t",
          ordering = FALSE,
          pageLength = -1,
          columnDefs = list(
            list(
              className = "dt-center",
              targets = "_all"
            )
          )
        ),
        rownames = FALSE,
        selection = "multiple"
      )
    })

    observeEvent(input$acceptmodels, {
      modal_visible(FALSE)
      shared_data$run_experiment <- runif(1)
    })

    observeEvent(input$cancelmodels, {
      modal_visible(FALSE)
    })

    observeEvent(input$eliminatemodel, {
      req(shared_data$models_table, input$modelestable_rows_selected)
      shared_data$models_table <- shared_data$models_table[
        -as.numeric(input$modelestable_rows_selected),
      ]
    })

    shared_data
  })
}
