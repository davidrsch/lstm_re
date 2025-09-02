box::use(
  dplyr[arrange, slice],
  DT[datatable, DTOutput, renderDataTable],
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack, Text],
  shiny.fluent[PrimaryButton.shinyInput],
  shiny[div, moduleServer, NS, observeEvent, renderUI, req, tagList],
  shiny[uiOutput],
  shinyalert[shinyalert],
  shinyjs[hidden],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  make_card(
    tagList(
      "Select amount of data to use",
      DefaultButton.shinyInput(
        ns("toggle_data_amount_card"),
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
        id = ns("data_amount_card_content"),
        Stack(
          tokens = list(childrenGap = 10),
          Text(
            "Test set:",
            style = "text-align: center; font-weight: bold;"
          ),
          Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            div(
              uiOutput(ns("selectteststart_dropdown")),
              style = "min-width:45%; max-width:45%;"
            ),
            div(style = "flex-grow: 1;"),
            div(
              uiOutput(ns("selecttestend_dropdown")),
              style = "min-width:45%; max-width:45%;"
            )
          ),
          Text(
            "Train set:",
            style = "text-align: center; font-weight: bold;"
          ),
          uiOutput(ns("selecttrainstart_dropdown")),
          PrimaryButton.shinyInput(
            ns("adtraintotest"),
            text = "OK"
          ),
          div(
            style = paste0(
              "border-radius: 0; border: black thin solid;",
              " max-height: 140px; overflow: auto; ",
              "font-size: 0.9em; text-align: center"
            ),
            DTOutput(ns("traindatestable")),
          ),
          DefaultButton.shinyInput(
            ns("eliminatetrainsd"),
            text = "Eliminate"
          )
        )
      )
    ),
    style = "background-color: white;",
    is_contained = TRUE
  )
}

#' @export
server <- function(id, database) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 08-Select dates-periods----
    observeEvent(list(database$df, database$selected_date_variable), {
      req(database$df)
      if (
        is.null(database$selected_date_variable) ||
          database$selected_date_variable == "" ||
          !(database$selected_date_variable %in% colnames(database$df))
      ) {
        database$x_data <- seq_len(dim(database$EDA)[1])
      } else {
        database$x_data <- as.character(database$df[[
          database$selected_date_variable
        ]])
      }
    })

    output$selectteststart_dropdown <- renderUI(
      {
        options <- list()
        if (!is.null(database$x_data) && length(database$x_data) > 2) {
          options <- lapply(
            database$x_data[2:(length(database$x_data) - 1)],
            function(x) list(key = x, text = x)
          )
        }

        Dropdown.shinyInput(
          ns("selectteststart"),
          label = "Start",
          options = options,
          value = database$test_start_date %||%
            if (length(options) > 0) options[[1]]$key else NULL,
          key = "selectteststart_dropdown_key"
        )
      }
    )

    output$selecttestend_dropdown <- renderUI({
      # This renderUI will react to input$selectteststart, database$x_data, input$selecttestend
      x_data <- database$x_data
      start <- input$selectteststart
      end <- input$selecttestend # This is the current value of the dropdown

      # Ensure database$df is available before proceeding
      req(database$df)

      if (is.null(start)) {
        choices2 <- x_data # If start is NULL, all x_data are potential end dates
      } else {
        choices2 <- x_data[(which(x_data == start) + 1):length(x_data)]
      }

      Dropdown.shinyInput(
        ns("selecttestend"),
        label = "End",
        options = lapply(choices2, function(x) list(key = x, text = x)),
        value = database$test_end_date %||%
          if (length(choices2) > 0) choices2[[1]] else NULL,
        key = "selecttestend_dropdown_key"
      )
    })

    observeEvent(input$selectteststart, {
      database$test_start_date <- input$selectteststart
      x_data <- database$x_data
      start <- input$selectteststart
      end <- input$selecttestend
      choices2 <- x_data[(which(x_data == start) + 1):length(x_data)]
    })

    observeEvent(input$selecttestend, {
      database$test_end_date <- input$selecttestend
    })

    output$selecttrainstart_dropdown <- renderUI({
      # This renderUI will react to input$selectteststart, database$x_data, input$selecttrainstart
      x_data <- database$x_data
      end <- input$selectteststart
      start <- input$selecttrainstart # This is the current value of the dropdown

      # Ensure database$df is available before proceeding
      req(database$df)

      if (is.null(end)) {
        choices <- x_data # If end is NULL, all x_data are potential train start dates
      } else {
        choices <- x_data[1:(which(x_data == end) - 1)]
      }
      database$starttrainlevels <- choices # Update starttrainlevels here

      if (
        is.null(start) ||
          !is.element(start, x_data) ||
          which(x_data == start) >= which(x_data == end)
      ) {
        selected <- NULL
      } else {
        selected <- start
      }

      Dropdown.shinyInput(
        ns("selecttrainstart"),
        label = "Start",
        options = lapply(choices, function(x) list(key = x, text = x)),
        value = selected, # Use the calculated selected value
        key = "selecttrainstart_dropdown_key"
      )
    })

    observeEvent(input$selectteststart, {
      x_data <- database$x_data
      end <- input$selectteststart
      start <- input$selecttrainstart
      choices <- x_data[1:(which(x_data == end) - 1)]
      database$starttrainlevels <- choices
      if (
        is.null(start) ||
          !is.element(start, x_data) ||
          which(x_data == start) >= which(x_data == end)
      ) {
        selected <- NULL
      } else {
        selected <- start
      }
    })

    observeEvent(input$adtraintotest, {
      if (
        is.null(input$selectteststart) ||
          input$selectteststart == "" ||
          is.null(input$selecttestend) ||
          input$selecttestend == "" ||
          is.null(input$selecttrainstart) ||
          input$selecttrainstart == "" ||
          !any(database$grid$Inputs == 1) ||
          !any(database$grid$Outputs == 1)
      ) {
        shinyalert(
          "Error",
          paste0(
            "Make sure you have selected Input and Output variables, start",
            " and end dates to the test set and at least an start date for ",
            "training set"
          ),
          type = "error"
        )
      } else {
        stns <- input$selecttrainstart
        if (dim(database$selectedtrains)[1] == 0) {
          database$selectedtrains <- data.frame(`Train start dates` = stns)
          if (database$showGraphs < 2) {
            database$showGraphs <- database$showGraphs + 1
          }
          if (database$showGraphs == 1) {
            # select graph pivot
          }
        } else {
          if (!is.element(stns, database$selectedtrains[, 1])) {
            database$selectedtrains <- rbind(
              database$selectedtrains,
              data.frame(`Train start dates` = stns)
            )
            database$selectedtrains <- database$selectedtrains |>
              arrange(factor(
                Train.start.dates,
                levels = database$starttrainlevels
              ))
          }
        }
        indxofstn <- which(is.element(
          database$x_data,
          database$selectedtrains$Train.start.dates
        ))
        indxofstt <- which(database$x_data == input$selectteststart)
        if (any(indxofstn > indxofstt)) {
          whichisbig <- which(indxofstn > indxofstt)
          database$selectedtrains <- database$selectedtrains |>
            slice(-whichisbig)
        }
      }
    })

    output$traindatestable <- renderDataTable({
      datatable(
        database$selectedtrains,
        options = list(dom = "t", pageLength = dim(database$selectedtrains)[1])
      )
    })

    observeEvent(input$eliminatetrainsd, {
      rwstr <- input$traindatestable_rows_selected
      database$selectedtrains <- database$selectedtrains |> slice(-rwstr)
    })
  })
}
