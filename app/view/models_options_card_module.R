box::use(
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack],
  shiny.fluent[TextField.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, renderUI, tagList, uiOutput],
  shinyalert[shinyalert],
  shinyjs[hidden],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
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
            "Add amount"
          ),
          uiOutput(ns("selectLSTMsoptions_ui")),
          uiOutput(ns("addneuronsamount_ui")),
          DefaultButton.shinyInput(
            ns("acceptneuronamountbutton"),
            "Add amount"
          ),
          uiOutput(ns("selectneuronsoptions_ui"))
        )
      )
    ),
    style = "background-color: white;",
    is_contained = TRUE
  )
}

#' @export
server <- function(id, sf) {
  moduleServer(id, function(input, output, session) {
    output$addLSTMamount_ui <- renderUI({
      TextField.shinyInput(
        session$ns("addLSTMamount"),
        label = "Add LSTM layer amount",
        type = "number",
        min = 1,
        value = sf$addLSTMamount
      )
    })

    observeEvent(input$addLSTMamount, {
      sf$addLSTMamount <- input$addLSTMamount
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
        shinyalert(
          "Error",
          "Wrong amount of LSTM layers format, most be an integer number bigger than 0",
          type = "error"
        )
      } else {
        if (is.null(sf$lstm_amounts)) {
          sf$lstm_amounts <- add_lstm_amount_val
          sf$std_lstm_amounts <- add_lstm_amount_val
        } else {
          if (!is.element(add_lstm_amount_val, sf$lstm_amounts)) {
            sf$lstm_amounts <- c(sf$lstm_amounts, add_lstm_amount_val)
            sf$lstm_amounts <- sort(sf$lstm_amounts)
            sf$std_lstm_amounts <- c(sf$std_lstm_amounts, add_lstm_amount_val)
          }
        }
      }
    })

    output$selectLSTMsoptions_ui <- renderUI({
      Dropdown.shinyInput(
        session$ns("selectLSTMsoptions"),
        label = "Select the amounts of LSTM",
        multiSelect = TRUE,
        value = sf$std_lstm_amounts,
        options = lapply(sf$lstm_amounts, function(x) list(key = x, text = x))
      )
    })

    observeEvent(input$selectLSTMsoptions, {
      sf$std_lstm_amounts <- input$selectLSTMsoptions
    })

    output$addneuronsamount_ui <- renderUI({
      TextField.shinyInput(
        session$ns("addneuronsamount"),
        label = "Add neuron amount",
        type = "number",
        min = 1,
        value = sf$addneuronsamount
      )
    })

    observeEvent(input$addneuronsamount, {
      sf$addneuronsamount <- input$addneuronsamount
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
        shinyalert(
          "Error",
          "Wrong amount of neurons format, most be an integer number bigger than 0",
          type = "error"
        )
      } else {
        if (is.null(sf$neuron_amounts)) {
          sf$neuron_amounts <- addneuronsamount_val
          sf$std_neuron_amounts <- addneuronsamount_val
        } else {
          if (!is.element(addneuronsamount_val, sf$neuron_amounts)) {
            sf$neuron_amounts <- c(sf$neuron_amounts, addneuronsamount_val)
            sf$neuron_amounts <- sort(sf$neuron_amounts)
            sf$std_neuron_amounts <- c(sf$std_neuron_amounts, addneuronsamount_val)
          }
        }
      }
    })

    output$selectneuronsoptions_ui <- renderUI({
      Dropdown.shinyInput(
        session$ns("selectneuronsoptions"),
        label = "Select the amounts of neurons",
        multiSelect = TRUE,
        value = sf$std_neuron_amounts,
        options = lapply(sf$neuron_amounts, function(x) list(key = x, text = x))
      )
    })

    observeEvent(input$selectneuronsoptions, {
      sf$std_neuron_amounts <- input$selectneuronsoptions
    })
  })
}
