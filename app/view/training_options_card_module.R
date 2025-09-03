box::use(
  shiny.fluent[Checkbox.shinyInput, DefaultButton.shinyInput, Stack],
  shiny.fluent[TextField.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, renderUI, tagList, uiOutput],
  shinyalert[shinyalert],
  shinyjs[disable, enable, hidden],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
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
        )
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
}

#' @export
server <- function(id, sf) {
  moduleServer(id, function(input, output, session) {
    output$selectepochamount_ui <- renderUI({
      TextField.shinyInput(
        session$ns("selectepochamount"),
        label = "Epoch",
        type = "number",
        min = 1,
        value = sf$epoch
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
        shinyalert(
          "Error",
          "Epoch must be an integer number bigger than 0",
          type = "error"
        )
        sf$epoch <- NULL # Set to NULL or a default valid value if invalid
      } else {
        sf$epoch <- as.integer(selectepochamount_val)
      }
    })

    output$seed_ui <- renderUI({
      TextField.shinyInput(
        session$ns("seed"),
        label = "Seed",
        type = "number",
        min = 1,
        value = if (is.null(sf$seed)) 123 else sf$seed
      )
    })

    observeEvent(input$seed, {
      sf$seed <- input$seed
    })

    observeEvent(input$setseed, {
      sf$setseed <- input$setseed
      if (input$setseed == TRUE) {
        enable("seed")
      } else {
        disable("seed")
      }
    })
  })
}
