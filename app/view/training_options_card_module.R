box::use(
  shiny.fluent[Checkbox.shinyInput, DefaultButton.shinyInput, Stack],
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
  shinyjs[disable, enable, hidden, toggle],
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
        "Training options",
        DefaultButton.shinyInput(
          ns("toggle_card"),
          iconProps = list(iconName = "ChevronDown"),
          style = "float: right; width: 0.7em",
          styles = list(
            root = list(
              "min-width" = "32px"
            )
          ),
          `data-testid` = "toggle_to_card"
        )
      ),
      hidden(
        div(
          id = ns("card_content"),
          Stack(
            tokens = list(childrenGap = 10),
            uiOutput(ns("selectepochamount_ui")),
            Checkbox.shinyInput(
              ns("setseed"),
              label = "Set seed",
              value = TRUE
            ),
            uiOutput(ns("seed_ui"))
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
      visibility$training_options <- !visibility$training_options
      toggle("card_content")
      updateDefaultButton.shinyInput(
        session,
        "toggle_card",
        iconProps = list(
          iconName = if (visibility$training_options) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )
    })

    collapse_on_sibling_open(
      "ts_transformations",
      "training_options",
      visibility,
      session
    )
    collapse_on_sibling_open(
      "training_vectors",
      "training_options",
      visibility,
      session
    )
    collapse_on_sibling_open(
      "models_options",
      "training_options",
      visibility,
      session
    )

    output$selectepochamount_ui <- renderUI({
      TextField.shinyInput(
        session$ns("selectepochamount"),
        label = "Epoch",
        type = "number",
        min = 1,
        value = shared_data$epoch
      )
    })

    observeEvent(input$selectepochamount, {
      selectepochamount_val <- as.numeric(input$selectepochamount)
      if (
        is.null(selectepochamount_val) ||
          is.na(selectepochamount_val) ||
          selectepochamount_val == "" ||
          selectepochamount_val < 1 ||
          (selectepochamount_val %% 1 != 0)
      ) {
        error_message("Epoch must be an integer number bigger than 0")
        error_visible(TRUE)
        shared_data$epoch <- NULL # Set to NULL or a default valid value if invalid
      } else {
        shared_data$epoch <- as.integer(selectepochamount_val)
      }
    })

    output$seed_ui <- renderUI({
      TextField.shinyInput(
        session$ns("seed"),
        label = "Seed",
        type = "number",
        min = 1,
        value = if (is.null(shared_data$seed)) 123 else shared_data$seed
      )
    })

    observeEvent(input$seed, {
      shared_data$seed <- input$seed
    })

    observeEvent(input$setseed, {
      shared_data$set_seed <- input$setseed
      if (input$setseed == TRUE) {
        enable("seed")
      } else {
        disable("seed")
      }
    })
  })
}
