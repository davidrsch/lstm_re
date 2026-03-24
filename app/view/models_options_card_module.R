box::use(
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack],
  shiny.fluent[TextField.shinyInput, updateDefaultButton.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, reactiveVal, renderUI, tagList, uiOutput],
  shinyjs[hidden, hide, toggle],
)

box::use(
  app / logic / make_card[make_card],
  app / view / make_modal,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    make_modal$ui(ns("error_modal")),
    make_card(
      title = tagList(
        "Models options",
        DefaultButton.shinyInput(
          ns("toggle_card"),
          iconProps = list(iconName = "ChevronDown"),
          style = "float: right; width: 0.7em",
          styles = list(
            root = list(
              "min-width" = "32px"
            )
          )
        )
      ),
      hidden(
        div(
          id = ns("card_content"),
          Stack(
            tokens = list(childrenGap = 10),
            uiOutput(ns("addLSTMamount_ui")),
            DefaultButton.shinyInput(
              ns("acceptLSTMamountbutton"),
              text = "Add amount"
            ),
            uiOutput(ns("selectLSTMsoptions_ui")),
            uiOutput(ns("addneuronsamount_ui")),
            DefaultButton.shinyInput(
              ns("acceptneuronamountbutton"),
              text = "Add amount"
            ),
            uiOutput(ns("selectneuronsoptions_ui"))
          )
        )
      ),
      style = "background-color: white;",
      is_contained = TRUE
    )
  )
}

#' @export
server <- function(id, shared_data, visibility) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    error_visible <- reactiveVal(FALSE)
    error_message <- reactiveVal("")
    output$error_content <- renderUI(div(error_message()))
    make_modal$server(
      "error_modal",
      name = "error_modal",
      is_open = error_visible,
      title = "Error",
      content = uiOutput(ns("error_content")),
      status = "error"
    )

    observeEvent(input$toggle_card, {
      visibility$models_options <- !visibility$models_options
      toggle(id = ns("card_content"))
      updateDefaultButton.shinyInput(
        session,
        "toggle_card",
        iconProps = list(
          iconName = if (visibility$models_options) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )
    })

    observeEvent(
      visibility$ts_transformations,
      {
        if (visibility$ts_transformations && visibility$models_options) {
          visibility$models_options <- FALSE
          hide(id = ns("card_content"))
          updateDefaultButton.shinyInput(
            session,
            "toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      visibility$training_vectors,
      {
        if (visibility$training_vectors && visibility$models_options) {
          visibility$models_options <- FALSE
          hide(id = ns("card_content"))
          updateDefaultButton.shinyInput(
            session,
            "toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      visibility$training_options,
      {
        if (visibility$training_options && visibility$models_options) {
          visibility$models_options <- FALSE
          hide(id = ns("card_content"))
          updateDefaultButton.shinyInput(
            session,
            "toggle_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      },
      ignoreInit = TRUE
    )

    output$addLSTMamount_ui <- renderUI({
      TextField.shinyInput(
        session$ns("addLSTMamount"),
        label = "Add LSTM layer amount",
        type = "number",
        min = 1,
        value = shared_data$addLSTMamount
      )
    })

    observeEvent(input$addLSTMamount, {
      shared_data$addLSTMamount <- input$addLSTMamount
    })

    observeEvent(input$acceptLSTMamountbutton, {
      add_lstm_amount_val <- as.numeric(input$addLSTMamount)
      if (
        is.null(add_lstm_amount_val) ||
          is.na(add_lstm_amount_val) ||
          add_lstm_amount_val == "" ||
          add_lstm_amount_val < 1 ||
          (add_lstm_amount_val %% 1 != 0)
      ) {
        error_message(
          "Wrong amount of LSTM layers format, must be an integer number bigger than 0"
        )
        error_visible(TRUE)
      } else {
        if (is.null(shared_data$lstm_amounts)) {
          shared_data$lstm_amounts <- add_lstm_amount_val
          shared_data$std_lstm_amounts <- add_lstm_amount_val
        } else {
          if (!is.element(add_lstm_amount_val, shared_data$lstm_amounts)) {
            shared_data$lstm_amounts <- c(
              shared_data$lstm_amounts,
              add_lstm_amount_val
            )
            shared_data$lstm_amounts <- sort(shared_data$lstm_amounts)
            shared_data$std_lstm_amounts <- c(
              shared_data$std_lstm_amounts,
              add_lstm_amount_val
            )
          }
        }
      }
    })

    output$selectLSTMsoptions_ui <- renderUI({
      Dropdown.shinyInput(
        session$ns("selectLSTMsoptions"),
        label = "Select the amounts of LSTM",
        multiSelect = TRUE,
        value = shared_data$std_lstm_amounts,
        options = lapply(shared_data$lstm_amounts, function(x) {
          list(key = x, text = x)
        })
      )
    })

    observeEvent(input$selectLSTMsoptions, {
      shared_data$std_lstm_amounts <- input$selectLSTMsoptions
    })

    output$addneuronsamount_ui <- renderUI({
      TextField.shinyInput(
        session$ns("addneuronsamount"),
        label = "Add neuron amount",
        type = "number",
        min = 1,
        value = shared_data$addneuronsamount
      )
    })

    observeEvent(input$addneuronsamount, {
      shared_data$addneuronsamount <- input$addneuronsamount
    })

    observeEvent(input$acceptneuronamountbutton, {
      addneuronsamount_val <- as.numeric(input$addneuronsamount)
      if (
        is.null(addneuronsamount_val) ||
          is.na(addneuronsamount_val) ||
          addneuronsamount_val == "" ||
          addneuronsamount_val < 1 ||
          (addneuronsamount_val %% 1 != 0)
      ) {
        error_message(
          "Wrong amount of neurons format, must be an integer number bigger than 0"
        )
        error_visible(TRUE)
      } else {
        if (is.null(shared_data$neuron_amounts)) {
          shared_data$neuron_amounts <- addneuronsamount_val
          shared_data$std_neuron_amounts <- addneuronsamount_val
        } else {
          if (!is.element(addneuronsamount_val, shared_data$neuron_amounts)) {
            shared_data$neuron_amounts <- c(
              shared_data$neuron_amounts,
              addneuronsamount_val
            )
            shared_data$neuron_amounts <- sort(shared_data$neuron_amounts)
            shared_data$std_neuron_amounts <- c(
              shared_data$std_neuron_amounts,
              addneuronsamount_val
            )
          }
        }
      }
    })

    output$selectneuronsoptions_ui <- renderUI({
      Dropdown.shinyInput(
        session$ns("selectneuronsoptions"),
        label = "Select the amounts of neurons",
        multiSelect = TRUE,
        value = shared_data$std_neuron_amounts,
        options = lapply(shared_data$neuron_amounts, function(x) {
          list(key = x, text = x)
        })
      )
    })

    observeEvent(input$selectneuronsoptions, {
      shared_data$std_neuron_amounts <- input$selectneuronsoptions
    })
  })
}
