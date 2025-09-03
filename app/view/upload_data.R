box::use(
  shiny.fluent[updateDefaultButton.shinyInput],
  shiny[div, moduleServer, NS, observeEvent],
  shinyjs[hide, toggle],
)

box::use(
  app / view / data_amount_card_module,
  app / view / data_analysis_display_module,
  app / view / upload_card_module,
  app / view / variables_card_module,
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
        div(
          upload_card_module$ui(ns("upload_card")),
          variables_card_module$ui(ns("variables_card")),
          data_amount_card_module$ui(ns("data_amount_card")),
        )
      ),
      div(
        class = "ms-Grid-col ms-sm8",
        data_analysis_display_module$ui(ns("data_analysis_display")),
      )
    )
  )
}

#' @export
server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Call child module servers
    upload_card_module$server("upload_card", shared_data)
    variables_card_module$server("variables_card", shared_data)
    data_amount_card_module$server("data_amount_card", shared_data)
    data_analysis_display_module$server("data_analysis_display", shared_data)

    observeEvent(input$`upload_card-toggle_upload_card`, {
      # Toggle current card
      shared_data$upload_card_visible <- !shared_data$upload_card_visible
      toggle(id = "upload_card-upload_card_content")
      updateDefaultButton.shinyInput(
        session,
        "upload_card-toggle_upload_card",
        iconProps = list(
          iconName = if (shared_data$upload_card_visible) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )

      # If this card is now visible, close others
      if (shared_data$upload_card_visible) {
        if (shared_data$variables_card_visible) {
          shared_data$variables_card_visible <- FALSE
          hide(id = "variables_card-variables_card_content")
          updateDefaultButton.shinyInput(
            session,
            "variables_card-toggle_variables_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (shared_data$data_amount_card_visible) {
          shared_data$data_amount_card_visible <- FALSE
          hide(id = "data_amount_card-data_amount_card_content")
          updateDefaultButton.shinyInput(
            session,
            "data_amount_card-toggle_data_amount_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })

    observeEvent(input$`variables_card-toggle_variables_card`, {
      # Toggle current card
      shared_data$variables_card_visible <- !shared_data$variables_card_visible
      toggle(id = "variables_card-variables_card_content")
      updateDefaultButton.shinyInput(
        session,
        "variables_card-toggle_variables_card",
        iconProps = list(
          iconName = if (shared_data$variables_card_visible) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )

      # If this card is now visible, close others
      if (shared_data$variables_card_visible) {
        if (shared_data$upload_card_visible) {
          shared_data$upload_card_visible <- FALSE
          hide(id = "upload_card-upload_card_content")
          updateDefaultButton.shinyInput(
            session,
            "upload_card-toggle_upload_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (shared_data$data_amount_card_visible) {
          shared_data$data_amount_card_visible <- FALSE
          hide(id = "data_amount_card-data_amount_card_content")
          updateDefaultButton.shinyInput(
            session,
            "data_amount_card-toggle_data_amount_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })

    observeEvent(input$`data_amount_card-toggle_data_amount_card`, {
      # Toggle current card
      shared_data$data_amount_card_visible <- !shared_data$data_amount_card_visible
      toggle(id = "data_amount_card-data_amount_card_content")
      updateDefaultButton.shinyInput(
        session,
        "data_amount_card-toggle_data_amount_card",
        iconProps = list(
          iconName = if (shared_data$data_amount_card_visible) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )

      # If this card is now visible, close others
      if (shared_data$data_amount_card_visible) {
        if (shared_data$upload_card_visible) {
          shared_data$upload_card_visible <- FALSE
          hide(id = "upload_card-upload_card_content")
          updateDefaultButton.shinyInput(
            session,
            "upload_card-toggle_upload_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
        if (shared_data$variables_card_visible) {
          shared_data$variables_card_visible <- FALSE
          hide(id = "variables_card-variables_card_content")
          updateDefaultButton.shinyInput(
            session,
            "variables_card-toggle_variables_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      }
    })
  })
}
