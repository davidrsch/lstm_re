box::use(
  DT[datatable, renderDataTable],
  shiny.fluent[DefaultButton.shinyInput, PrimaryButton.shinyInput, Stack],
  shiny.fluent[Modal, reactOutput, renderReact],
  shiny.fluent[updateDefaultButton.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, reactiveValues],
  shiny[req],
  shinyalert[shinyalert],
  shinyjs[hide, toggle],
  stats[runif],
)


box::use(
  app / logic / ui_helpers[findmodels, selectmodelstobuild],
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
      shared_data
    )
    training_vectors_card_module$server("training_vectors_card", shared_data)
    models_options_card_module$server("models_options_card", shared_data)
    training_options_card_module$server("training_options_card", shared_data)
    feature_selection_guide_module$server("feature_selection_guide")

    observeEvent(input$`ts_transformations_card-toggle_card`, {
      visibility$ts_transformations <- !visibility$ts_transformations
      toggle(id = "ts_transformations_card-card_content")
      updateDefaultButton.shinyInput(
        session,
        "ts_transformations_card-toggle_card",
        iconProps = list(
          iconName = ifelse(
            visibility$ts_transformations,
            "ChevronUp",
            "ChevronDown"
          )
        )
      )
      if (visibility$ts_transformations) {
        if (visibility$training_vectors) {
          visibility$training_vectors <- FALSE
          hide(id = "training_vectors_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "training_vectors_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$models_options) {
          visibility$models_options <- FALSE
          hide(id = "models_options_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "models_options_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$training_options) {
          visibility$training_options <- FALSE
          hide(id = "training_options_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "training_options_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })

    observeEvent(input$`training_vectors_card-toggle_card`, {
      visibility$training_vectors <- !visibility$training_vectors
      toggle(id = "training_vectors_card-card_content")
      updateDefaultButton.shinyInput(
        session,
        "training_vectors_card-toggle_card",
        iconProps = list(
          iconName = ifelse(
            visibility$training_vectors,
            "ChevronUp",
            "ChevronDown"
          )
        )
      )
      if (visibility$training_vectors) {
        if (visibility$ts_transformations) {
          visibility$ts_transformations <- FALSE
          hide(id = "ts_transformations_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "ts_transformations_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$models_options) {
          visibility$models_options <- FALSE
          hide(id = "models_options_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "models_options_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$training_options) {
          visibility$training_options <- FALSE
          hide(id = "training_options_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "training_options_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })

    observeEvent(input$`models_options_card-toggle_card`, {
      visibility$models_options <- !visibility$models_options
      toggle(id = "models_options_card-card_content")
      updateDefaultButton.shinyInput(
        session,
        "models_options_card-toggle_card",
        iconProps = list(
          iconName = ifelse(
            visibility$models_options,
            "ChevronUp",
            "ChevronDown"
          )
        )
      )
      if (visibility$models_options) {
        if (visibility$ts_transformations) {
          visibility$ts_transformations <- FALSE
          hide(id = "ts_transformations_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "ts_transformations_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$training_vectors) {
          visibility$training_vectors <- FALSE
          hide(id = "training_vectors_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "training_vectors_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$training_options) {
          visibility$training_options <- FALSE
          hide(id = "training_options_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "training_options_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })

    observeEvent(input$`training_options_card-toggle_card`, {
      visibility$training_options <- !visibility$training_options
      toggle(id = "training_options_card-card_content")
      updateDefaultButton.shinyInput(
        session,
        "training_options_card-toggle_card",
        iconProps = list(
          iconName = ifelse(
            visibility$training_options,
            "ChevronUp",
            "ChevronDown"
          )
        )
      )
      if (visibility$training_options) {
        if (visibility$ts_transformations) {
          visibility$ts_transformations <- FALSE
          hide(id = "ts_transformations_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "ts_transformations_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$training_vectors) {
          visibility$training_vectors <- FALSE
          hide(id = "training_vectors_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "training_vectors_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (visibility$models_options) {
          visibility$models_options <- FALSE
          hide(id = "models_options_card-card_content")
          updateDefaultButton.shinyInput(
            session,
            "models_options_card-toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })

    observeEvent(input$startexperimentation, {
      if (
        is.null(shared_data$transf) ||
          is.null(shared_data$scales) ||
          is.na(shared_data$temporalhorizon) ||
          is.null(shared_data$stdinputamnts) ||
          is.null(shared_data$stdLSTMamnts) ||
          is.null(shared_data$stdneuronsamnts) ||
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
          if (shared_data$setseed == TRUE && is.na(shared_data$seed)) {
            shinyalert(
              "Error",
              "If a seed is going to be used a seed most be specified",
              type = "error"
            )
          } else {
            modal_visible(TRUE)
            shared_data$modelstable <- findmodels(
              shared_data$stdLSTMamnts,
              shared_data$stdneuronsamnts
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
          selectmodelstobuild(
            ns = ns,
            train = shared_data$selectedtrains,
            ts = shared_data$transf,
            sc = shared_data$scales,
            vec = shared_data$stdinputamnts,
            lstm = shared_data$stdLSTMamnts,
            neu = shared_data$stdneuronsamnts
          )
        )
      }
    })

    output$modelestable <- renderDataTable({
      req(shared_data$modelstable)
      datatable(
        shared_data$modelstable,
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
      req(shared_data$modelstable, input$modelestable_rows_selected)
      shared_data$modelstable <- shared_data$modelstable[
        -as.numeric(input$modelestable_rows_selected),
      ]
    })

    shared_data
  })
}
