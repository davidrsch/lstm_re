box::use(
  htmlwidgets[onRender],
  plotly[config, plot_ly, plotlyOutput, renderPlotly],
  shiny.fluent[ProgressIndicator],
  shiny[
    div,
    moduleServer,
    NS,
    observe,
    reactiveValuesToList,
    renderUI,
    uiOutput,
    wellPanel
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    div(
      id = ns("htmlmodelfeatT"),
      style = "margin-bottom: 4px"
    ),
    div(
      id = ns("livefitpcon"),
      plotlyOutput(
        outputId = ns("liveplot"),
        height = "290px"
      )
    ),
    div(
      id = ns("containingpbs"),
      uiOutput(ns("batchpb_ui")),
      uiOutput(ns("epochpb_ui")),
      uiOutput(ns("modelpb_ui"))
    ),
    style = "background-color: white;
    border-color:black;border-radius:0;
    height:100%; margin-top: 1%"
  )
}

#' @export
server <- function(id, progress_rv, parent_ns) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      inputs <- reactiveValuesToList(input)
      lapply(names(inputs), function(key) {
        if (grepl("_batch_update$", key)) {
          progress_rv$batch <- inputs[[key]]
        }
        if (grepl("_epoch_update$", key)) {
          progress_rv$epoch <- inputs[[key]]
        }
      })
    })

    output$batchpb_ui <- renderUI({
      ProgressIndicator(
        id = ns("batchpb"),
        percentComplete = progress_rv$batch,
        description = "Samples:"
      )
    })

    output$epochpb_ui <- renderUI({
      ProgressIndicator(
        id = ns("epochpb"),
        percentComplete = progress_rv$epoch,
        description = "Epochs:"
      )
    })

    output$modelpb_ui <- renderUI({
      ProgressIndicator(
        id = ns("modelpb"),
        percentComplete = progress_rv$model,
        description = "Models:"
      )
    })

    output$liveplot <- renderPlotly({
      plot_ly(type = "scatter", mode = "markers") |>
        config(displayModeBar = FALSE) |>
        onRender(paste0(
          "
          function(){
            console.log('onRender fired!');
            Shiny.setInputValue(\"",
          parent_ns("rcalculation"),
          "\", 1, {priority: \"event\"});
          }"
        ))
    })
  })
}
