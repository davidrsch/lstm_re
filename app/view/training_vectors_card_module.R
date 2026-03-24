box::use(
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack],
  shiny.fluent[TextField.shinyInput, updateDefaultButton.shinyInput],
  shiny[
    div,
    moduleServer,
    NS,
    observeEvent,
    reactiveVal,
    renderUI,
    tagList,
    uiOutput
  ],
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
        "Training vectors options",
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
            uiOutput(ns("temporalhorizon_ui")),
            uiOutput(ns("addINoption_ui")),
            DefaultButton.shinyInput(
              ns("acceptinputoptionbutton"),
              text = "Add input"
            ),
            uiOutput(ns("selectinputoptions_ui"))
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
      visibility$training_vectors <- !visibility$training_vectors
      toggle(id = ns("card_content"))
      updateDefaultButton.shinyInput(
        session,
        "toggle_card",
        iconProps = list(
          iconName = if (visibility$training_vectors) {
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
        if (visibility$ts_transformations && visibility$training_vectors) {
          visibility$training_vectors <- FALSE
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
      visibility$models_options,
      {
        if (visibility$models_options && visibility$training_vectors) {
          visibility$training_vectors <- FALSE
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
        if (visibility$training_options && visibility$training_vectors) {
          visibility$training_vectors <- FALSE
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

    output$temporalhorizon_ui <- renderUI({
      TextField.shinyInput(
        session$ns("temporalhorizon"),
        label = "Temporal horizon",
        type = "number",
        min = 1,
        value = shared_data$temporalhorizon
      )
    })

    observeEvent(input$temporalhorizon, {
      shared_data$temporalhorizon <- input$temporalhorizon
    })

    output$addINoption_ui <- renderUI({
      TextField.shinyInput(
        session$ns("addINoption"),
        label = "Add input amount",
        type = "number",
        min = 1,
        value = shared_data$addINoption
      )
    })

    observeEvent(input$addINoption, {
      shared_data$addINoption <- input$addINoption
    })

    observeEvent(input$temporalhorizon, {
      temporalhorizon_val <- as.numeric(input$temporalhorizon)
      if (
        !is.null(temporalhorizon_val) &&
          !is.na(temporalhorizon_val) &&
          temporalhorizon_val != "" &&
          (temporalhorizon_val < 1 || (temporalhorizon_val %% 1 != 0))
      ) {
        error_message(
          "Temporal horizon must be an integer number bigger than 0"
        )
        error_visible(TRUE)
      }
    })

    observeEvent(input$acceptinputoptionbutton, {
      add_in_option_val <- as.numeric(input$addINoption)
      if (
        is.null(add_in_option_val) ||
          is.na(add_in_option_val) ||
          add_in_option_val == "" ||
          add_in_option_val < 1 ||
          (add_in_option_val %% 1 != 0)
      ) {
        error_message(
          "Wrong input format. Input must be an integer number bigger than 0"
        )
        error_visible(TRUE)
      } else {
        if (is.null(shared_data$input_amounts)) {
          shared_data$input_amounts <- add_in_option_val
          shared_data$std_input_amounts <- add_in_option_val
        } else {
          if (!is.element(add_in_option_val, shared_data$input_amounts)) {
            shared_data$input_amounts <- c(
              shared_data$input_amounts,
              add_in_option_val
            )
            shared_data$std_input_amounts <- c(
              shared_data$std_input_amounts,
              add_in_option_val
            )
          }
        }
      }
    })

    output$selectinputoptions_ui <- renderUI({
      Dropdown.shinyInput(
        session$ns("selectinputoptions"),
        label = "Select the amounts of inputs",
        multiSelect = TRUE,
        value = shared_data$std_input_amounts,
        options = lapply(shared_data$input_amounts, function(x) {
          list(key = x, text = x)
        })
      )
    })

    observeEvent(input$selectinputoptions, {
      shared_data$std_input_amounts <- input$selectinputoptions
    })
  })
}
