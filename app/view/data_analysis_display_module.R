box::use(
  DT[DTOutput, renderDataTable],
  shiny.fluent[Pivot, PivotItem, Stack],
  shiny[div, moduleServer, NS, plotOutput, renderPlot, renderText, renderUI],
  shiny[req, textOutput, uiOutput],
)

box::use(
  app / logic / eda[database_summary, plot_eda],
  app / logic / make_card[make_card],
  app / logic / plotting[sets_vars_plots],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  make_card(
    "Data Analysis",
    Stack(
      div(
        textOutput(ns("filename")),
      ),
      Pivot(
        id = ns("tableandgraphs"),
        PivotItem(
          headerText = "Data",
          itemKey = "data",
          div(
            DTOutput(ns("files")),
            style = "max-height: 720px; overflow: auto;"
          )
        ),
        PivotItem(
          headerText = "EDA",
          itemKey = "eda",
          Stack(
            plotOutput(ns("eda"), height = "350px"),
            DTOutput(ns("summary")),
            style = "max-height: 720px; overflow: auto;"
          )
        ),
        PivotItem(
          headerText = "Graphs",
          itemKey = "graphs",
          div(
            uiOutput(ns("plotselectedvariables")),
            style = "max-height: 720px; overflow: auto;"
          )
        )
      )
    ),
    style = "max-height: 808px; background-color: white;",
    is_contained = TRUE
  )
}

#' @export
server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 04-Imported file name ----
    output$filename <- renderText({
      req(shared_data$filename)
      shared_data$filename
    })

    # 05-Imported file table----
    output$files <- renderDataTable(
      shared_data$df,
      options = list(
        searching = FALSE,
        scrollX = TRUE,
        compact = FALSE,
        lengthChange = FALSE,
        pageLength = 7,
        ordering = FALSE
      ),
      class = "nowrap hover order-column"
    )

    output$eda <- renderPlot({
      req(nrow(shared_data$EDA) > 0)
      plot_eda(shared_data$EDA)
    })

    output$summary <- renderDataTable({
      req(nrow(shared_data$EDA) > 0)
      database_summary(shared_data$EDA)
    })

    output$plotselectedvariables <- renderUI({
      req(shared_data$selected_trains)
      req(nrow(shared_data$selected_trains) > 0)
      sets_vars_plots(
        shared_data$selected_trains,
        shared_data$EDA,
        shared_data$x_data,
        shared_data$test_start_date,
        shared_data$test_end_date
      )
    })
  })
}
