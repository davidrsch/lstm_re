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
  shinyjs[hidden, runjs, toggle],
)

box::use(
  app / logic / make_card[make_card],
  app / logic / ui_helpers[collapse_on_sibling_open],
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
          ),
          `data-testid` = "toggle_tv_card"
        )
      ),
      hidden(
        div(
          id = ns("card_content"),
          Stack(
            tokens = list(childrenGap = 10),
            div(
              `data-testid` = "temporalhorizon",
              uiOutput(ns("temporalhorizon_ui"))
            ),
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
      toggle("card_content")
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
      if (visibility$training_vectors) {
        runjs(paste0(
          "[...document.querySelectorAll('[role=\"tab\"]')]",
          ".find(el => el.textContent.trim() === 'Training vectors')",
          "?.click();"
        ))
      }
    })

    collapse_on_sibling_open(
      "ts_transformations",
      "training_vectors",
      visibility,
      session
    )
    collapse_on_sibling_open(
      "models_options",
      "training_vectors",
      visibility,
      session
    )
    collapse_on_sibling_open(
      "training_options",
      "training_vectors",
      visibility,
      session
    )

    output$temporalhorizon_ui <- renderUI({
      TextField.shinyInput(
        session$ns("temporalhorizon"),
        label = "Temporal horizon",
        type = "number",
        min = 1,
        value = if (is.null(shared_data$temporalhorizon)) "" else as.character(shared_data$temporalhorizon)
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
        value = if (is.null(shared_data$addINoption)) "" else as.character(shared_data$addINoption)
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
      shiny::freezeReactiveValue(input, "selectinputoptions")
      raw_val <- input$addINoption
      add_in_option_val <- if (is.null(raw_val) || length(raw_val) == 0L) {
        NA_real_
      } else {
        suppressWarnings(as.numeric(raw_val[[1L]]))
      }
      if (
        is.na(add_in_option_val) ||
          is.infinite(add_in_option_val) ||
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
          shared_data$std_input_amounts <- as.character(add_in_option_val)
        } else {
          if (!is.element(add_in_option_val, shared_data$input_amounts)) {
            shared_data$input_amounts <- sort(c(
              shared_data$input_amounts,
              add_in_option_val
            ))
            shared_data$std_input_amounts <- as.character(c(
              unlist(shared_data$std_input_amounts),
              add_in_option_val
            ))
          }
        }
      }
    })

    output$selectinputoptions_ui <- renderUI({
      value <- as.list(as.character(unlist(shared_data$std_input_amounts)))
      options <- lapply(shared_data$input_amounts, function(x) {
        list(key = as.character(x), text = as.character(x))
      })
      # The key must be unique to the state to force a clean React re-mount
      dropdown_key <- paste0(
        "in_dropdown_",
        length(options),
        "_",
        paste(value, collapse = "-")
      )

      Dropdown.shinyInput(
        session$ns("selectinputoptions"),
        label = "Select the amounts of inputs",
        multiSelect = TRUE,
        value = value,
        options = options,
        key = dropdown_key,
        `data-testid` = "selectinputoptions"
      )
    })

    observeEvent(input$selectinputoptions, {
      val <- input$selectinputoptions
      # Only update if there's an actual selection. Programmatic additions 
      # from the 'Add' button take precedence. This prevents 'renderUI' 
      # reset loops where a new component flashes an empty value.
      if (!is.null(val) && length(val) > 0) {
        shared_data$std_input_amounts <- val
      }
    }, ignoreInit = TRUE)
  })
}
