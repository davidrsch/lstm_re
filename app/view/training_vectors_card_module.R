box::use(
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack],
  shiny.fluent[TextField.shinyInput, updateDefaultButton.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, renderUI, tagList, uiOutput],
  shinyalert[shinyalert],
  shinyjs[hidden, hide, toggle],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
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
            "Add input"
          ),
          uiOutput(ns("selectinputoptions_ui"))
        )
      )
    ),
    style = "background-color: white;",
    is_contained = TRUE
  )
}

#' @export
server <- function(id, sf, visibility) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
        value = sf$temporalhorizon
      )
    })

    observeEvent(input$temporalhorizon, {
      sf$temporalhorizon <- input$temporalhorizon
    })

    output$addINoption_ui <- renderUI({
      TextField.shinyInput(
        session$ns("addINoption"),
        label = "Add input amount",
        type = "number",
        min = 1,
        value = sf$addINoption
      )
    })

    observeEvent(input$addINoption, {
      sf$addINoption <- input$addINoption
    })

    observeEvent(input$temporalhorizon, {
      temporalhorizon_val <- as.numeric(input$temporalhorizon)
      if (
        !is.null(temporalhorizon_val) &&
          !is.na(temporalhorizon_val) &&
          temporalhorizon_val != "" &&
          (temporalhorizon_val < 1 || (temporalhorizon_val %% 1 != 0))
      ) {
        shinyalert(
          "Error",
          "Temporal horizon must be an integer number bigger than 0",
          type = "error"
        )
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
        shinyalert(
          "Error",
          "Wrong input format. Input must be an integer number bigger than 0",
          type = "error"
        )
      } else {
        if (is.null(sf$input_amounts)) {
          sf$input_amounts <- add_in_option_val
          sf$std_input_amounts <- add_in_option_val
        } else {
          if (!is.element(add_in_option_val, sf$input_amounts)) {
            sf$input_amounts <- c(sf$input_amounts, add_in_option_val)
            sf$std_input_amounts <- c(sf$std_input_amounts, add_in_option_val)
          }
        }
      }
    })

    output$selectinputoptions_ui <- renderUI({
      Dropdown.shinyInput(
        session$ns("selectinputoptions"),
        label = "Select the amounts of inputs",
        multiSelect = TRUE,
        value = sf$std_input_amounts,
        options = lapply(sf$input_amounts, function(x) list(key = x, text = x))
      )
    })

    observeEvent(input$selectinputoptions, {
      sf$std_input_amounts <- input$selectinputoptions
    })
  })
}
