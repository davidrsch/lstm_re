box::use(
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack],
  shiny.fluent[updateDefaultButton.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, renderUI, tagList, uiOutput],
  shinyjs[toggle],
)

box::use(
  app / logic / constants[scales, transformations],
  app / logic / make_card[make_card],
  app / logic / ui_helpers[collapse_on_sibling_open],
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
        ),
        `data-testid` = "toggle_ts_card"
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
server <- function(id, shared_data, visibility) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$toggle_card, {
      # Toggle this card's visibility flag and let shinyjs show/hide the content.
      # The other cards observe their siblings' flags and collapse when one opens.
      visibility$ts_transformations <- !visibility$ts_transformations
      toggle("card_content")
      updateDefaultButton.shinyInput(
        session,
        "toggle_card",
        iconProps = list(
          iconName = if (visibility$ts_transformations) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )
    })

    # Collapse this card whenever another card opens, enforcing accordion behavior.
    collapse_on_sibling_open(
      "training_vectors",
      "ts_transformations",
      visibility,
      session
    )
    collapse_on_sibling_open(
      "models_options",
      "ts_transformations",
      visibility,
      session
    )
    collapse_on_sibling_open(
      "training_options",
      "ts_transformations",
      visibility,
      session
    )

    output$selectimeseries_ui <- renderUI({
      Dropdown.shinyInput(
        ns("selectimeseries"),
        label = "Time series to use",
        options = transformations,
        value = if (is.null(shared_data$transf)) {
          list("original", "first", "second")
        } else {
          shared_data$transf
        },
        multiSelect = TRUE,
        `data-testid` = "selectimeseries",
        calloutProps = list(`data-testid` = "selectimeseries-callout")
      )
    })

    output$selectimeseriescales_ui <- renderUI({
      Dropdown.shinyInput(
        ns("selectimeseriescales"),
        label = "Scales to use",
        options = scales,
        value = if (is.null(shared_data$scales)) {
          list("exact", "zero_one", "minus_plus")
        } else {
          shared_data$scales
        },
        multiSelect = TRUE,
        `data-testid` = "selectimeseriescales",
        calloutProps = list(`data-testid` = "selectimeseriescales-callout")
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
