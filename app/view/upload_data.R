box::use(
  shiny[div, moduleServer, NS],
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

    shared_data
  })
}
