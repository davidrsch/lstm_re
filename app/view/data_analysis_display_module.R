box::use(
  DT[DTOutput, renderDataTable],
  shiny.fluent[Pivot, PivotItem, Stack],
  shiny[div, moduleServer, NS, plotOutput, renderPlot, renderText, renderUI],
  shiny[req, textOutput, uiOutput],
)

box::use(
  app / logic / eda[databasesum, plotedafunc],
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
server <- function(id, database) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 04-Imported file name ----
    output$filename <- renderText({
      req(input$upload_file)
      input$upload_file$name
    })

    # 05-Imported file table----
    output$files <- renderDataTable(
      database$df,
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
      req(database$EDA)
      plotedafunc(database$EDA)
    })

    output$summary <- renderDataTable({
      req(database$EDA)
      databasesum(database$EDA)
    })

    output$plotselectedvariables <- renderUI({
      req(database$selectedtrains)
      req(nrow(database$selectedtrains) > 0)
      sets_vars_plots(
        database$selectedtrains,
        database$EDA,
        database$x_data,
        database$test_start_date,
        database$test_end_date
      )
    })
  })
}
