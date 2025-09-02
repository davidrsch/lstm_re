box::use(
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack],
  shiny[div, moduleServer, NS, observeEvent, renderUI, tagList, uiOutput],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  make_card(
    title = tagList(
      "Time series transformations",
      DefaultButton.shinyInput(
        ns("toggle_card"),
        iconProps = list(iconName = "ChevronUp"),
        style = "float: right; width: 0.7em",
        styles = list(
          root = list(
            "min-width" = "32px"
          )
        )
      )
    ),
    content = div(
      id = ns("card_content"),
      Stack(
        tokens = list(childrenGap = 10),
        uiOutput(ns("selectimeseries_ui")),
        uiOutput(ns("selectimeseriescales_ui"))
      )
    ),
    style = "background-color: white;",
    is_contained = TRUE
  )
}

#' @export
server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$selectimeseries_ui <- renderUI({
      Dropdown.shinyInput(
        ns("selectimeseries"),
        label = "Time series to use",
        options = list(
          list(key = "Original", text = "Original"),
          list(
            key = "First transformation",
            text = "First transformation"
          ),
          list(
            key = "Second transformation",
            text = "Second transformation"
          )
        ),
        value = if (is.null(shared_data$transf)) {
          list("Original", "First transformation", "Second transformation")
        } else {
          shared_data$transf
        },
        multiSelect = TRUE
      )
    })

    output$selectimeseriescales_ui <- renderUI({
      Dropdown.shinyInput(
        ns("selectimeseriescales"),
        label = "Scales to use",
        options = list(
          list(key = "Exact", text = "Exact"),
          list(key = "From 0 to 1", text = "From 0 to 1"),
          list(key = "From -1 to 1", text = "From -1 to 1")
        ),
        value = if (is.null(shared_data$scales)) {
          list("Exact", "From 0 to 1", "From -1 to 1")
        } else {
          shared_data$scales
        },
        multiSelect = TRUE
      )
    })

    observeEvent(input$selectimeseries, {
      shared_data$transf <- input$selectimeseries
    })

    observeEvent(input$selectimeseriescales, {
      shared_data$scales <- input$selectimeseriescales
    })
  })
}
