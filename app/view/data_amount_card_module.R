box::use(
  dplyr[arrange, filter, slice],
  DT[datatable, DTOutput, renderDataTable],
  rlang[`%||%`],
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack, Text],
  shiny.fluent[PrimaryButton.shinyInput],
  shiny[
    div,
    moduleServer,
    NS,
    observeEvent,
    reactiveVal,
    renderUI,
    req,
    tagList
  ],
  shiny[uiOutput],
  shinyjs[hidden, hide, runjs, toggle],
)

box::use(
  app / logic / make_card[make_card],
  app / view / make_modal,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    make_modal$ui(ns("error_modal")),
    make_card(
      tagList(
        "Select amount of data to use",
        uiOutput(ns("toggle_data_amount_card_ui"))
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
              `data-testid` = "traindatestable_container",
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
  )
}

#' @export
server <- function(id, shared_data) {
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

    output$toggle_data_amount_card_ui <- renderUI({
      has_in <- FALSE
      has_out <- FALSE
      if (!is.null(shared_data$grid) && "Inputs" %in% names(shared_data$grid)) {
        has_in <- nrow(filter(shared_data$grid, Inputs == TRUE)) > 0
        has_out <- nrow(filter(shared_data$grid, Outputs == TRUE)) > 0
      }
      disabled <- !(has_in && has_out)
      icon <- if (isTRUE(shared_data$data_amount_card_visible)) "ChevronUp" else "ChevronDown"
      DefaultButton.shinyInput(
        ns("toggle_data_amount_card"),
        disabled = disabled,
        iconProps = list(iconName = icon),
        style = "float: right; width: 0.7em",
        styles = list(root = list("min-width" = "32px")),
        `data-testid` = "toggle_data_amount_card"
      )
    })

    observeEvent(input$toggle_data_amount_card, {
      shared_data$data_amount_card_visible <- !shared_data$data_amount_card_visible
      toggle("data_amount_card_content")
      if (shared_data$data_amount_card_visible) {
        runjs(paste0(
          "[...document.querySelectorAll('[role=\"tab\"]')]",
          ".find(el => el.textContent.trim() === 'Graphs')",
          "?.click();"
        ))
      }
    })

    observeEvent(
      shared_data$upload_card_visible,
      {
        if (
          shared_data$upload_card_visible &&
            shared_data$data_amount_card_visible
        ) {
          shared_data$data_amount_card_visible <- FALSE
          hide("data_amount_card_content")
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      shared_data$variables_card_visible,
      {
        if (
          shared_data$variables_card_visible &&
            shared_data$data_amount_card_visible
        ) {
          shared_data$data_amount_card_visible <- FALSE
          hide("data_amount_card_content")
        }
      },
      ignoreInit = TRUE
    )

    # 08-Select dates-periods----
    observeEvent(list(shared_data$df, shared_data$selected_date_variable), {
      req(shared_data$df)
      if (
        is.null(shared_data$selected_date_variable) ||
          shared_data$selected_date_variable == "" ||
          !(shared_data$selected_date_variable %in% colnames(shared_data$df))
      ) {
        shared_data$x_data <- seq_len(nrow(shared_data$df))
      } else {
        shared_data$x_data <- as.character(shared_data$df[[
          shared_data$selected_date_variable
        ]])
      }
    })

    output$selectteststart_dropdown <- renderUI(
      {
        options <- list()
        if (!is.null(shared_data$x_data) && length(shared_data$x_data) > 2) {
          options <- lapply(
            shared_data$x_data[2:(length(shared_data$x_data) - 1)],
            function(x) list(key = x, text = x)
          )
        }

        Dropdown.shinyInput(
          ns("selectteststart"),
          label = "Start",
          options = options,
          value = shared_data$test_start_date %||%
            if (length(options) > 0) {
              options[[max(1, ceiling(length(options) * 0.75))]]$key
            } else {
              NULL
            },
          key = "selectteststart_dropdown_key"
        )
      }
    )

    output$selecttestend_dropdown <- renderUI({
      # This renderUI will react to input$selectteststart, shared_data$x_data, input$selecttestend
      x_data <- shared_data$x_data
      start <- input$selectteststart
      end <- input$selecttestend # This is the current value of the dropdown

      # Ensure shared_data$df is available before proceeding
      req(shared_data$df)

      if (is.null(start)) {
        choices2 <- x_data # If start is NULL, all x_data are potential end dates
      } else {
        choices2 <- x_data[(which(x_data == start) + 1):length(x_data)]
      }

      Dropdown.shinyInput(
        ns("selecttestend"),
        label = "End",
        options = lapply(choices2, function(x) list(key = x, text = x)),
        value = shared_data$test_end_date %||%
          if (length(choices2) > 0) choices2[[length(choices2)]] else NULL,
        key = "selecttestend_dropdown_key"
      )
    })

    observeEvent(input$selectteststart, {
      shared_data$test_start_date <- input$selectteststart
      x_data <- shared_data$x_data
      start <- input$selectteststart
      end <- input$selecttestend
      choices2 <- x_data[(which(x_data == start) + 1):length(x_data)]
    })

    observeEvent(input$selecttestend, {
      shared_data$test_end_date <- input$selecttestend
    })

    output$selecttrainstart_dropdown <- renderUI({
      # This renderUI will react to input$selectteststart, shared_data$x_data,
      # input$selecttrainstart
      x_data <- shared_data$x_data
      end <- input$selectteststart
      start <- input$selecttrainstart # This is the current value of the dropdown

      # Ensure shared_data$df is available before proceeding
      req(shared_data$df)

      if (is.null(end)) {
        choices <- x_data # If end is NULL, all x_data are potential train start dates
      } else {
        choices <- x_data[1:(which(x_data == end) - 1)]
      }
      shared_data$start_train_levels <- choices # Update starttrainlevels here

      if (
        is.null(start) ||
          !is.element(start, x_data) ||
          which(x_data == start) >= which(x_data == end)
      ) {
        selected <- if (length(choices) > 0) choices[[1]] else NULL
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
      x_data <- shared_data$x_data
      end <- input$selectteststart
      start <- input$selecttrainstart
      choices <- x_data[1:(which(x_data == end) - 1)]
      shared_data$start_train_levels <- choices
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
      x_data <- shared_data$x_data
      teststart <- if (
        !is.null(input$selectteststart) &&
          input$selectteststart != ""
      ) {
        input$selectteststart
      } else if (!is.null(shared_data$test_start_date)) {
        shared_data$test_start_date
      } else if (length(x_data) > 2) {
        x_data[[max(1, ceiling(length(x_data) * 0.75))]]
      } else {
        ""
      }
      testend <- if (
        !is.null(input$selecttestend) &&
          input$selecttestend != ""
      ) {
        input$selecttestend
      } else if (!is.null(shared_data$test_end_date)) {
        shared_data$test_end_date
      } else if (length(x_data) > 0) {
        x_data[[length(x_data)]]
      } else {
        ""
      }
      stns <- if (
        !is.null(input$selecttrainstart) &&
          input$selecttrainstart != ""
      ) {
        input$selecttrainstart
      } else if (length(shared_data$start_train_levels) > 0) {
        shared_data$start_train_levels[[1]]
      } else if (length(x_data) > 1) {
        x_data[[1]]
      } else {
        ""
      }
      if (
        teststart == "" ||
          testend == "" ||
          stns == "" ||
          !any(shared_data$grid$Inputs == 1) ||
          !any(shared_data$grid$Outputs == 1)
      ) {
        error_message(paste0(
          "Make sure you have selected Input and Output variables, start",
          " and end dates to the test set and at least an start date for ",
          "training set"
        ))
        error_visible(TRUE)
      } else {
        shared_data$test_start_date <- teststart
        shared_data$test_end_date <- testend
        if (dim(shared_data$selected_trains)[1] == 0) {
          shared_data$selected_trains <- data.frame(`Train start dates` = stns)
          if (shared_data$show_graphs < 2) {
            shared_data$show_graphs <- shared_data$show_graphs + 1
          }
          if (shared_data$show_graphs == 1) {
            # select graph pivot
          }
        } else {
          if (!is.element(stns, shared_data$selected_trains[, 1])) {
            shared_data$selected_trains <- rbind(
              shared_data$selected_trains,
              data.frame(`Train start dates` = stns)
            )
            shared_data$selected_trains <- shared_data$selected_trains |>
              arrange(factor(
                Train.start.dates,
                levels = shared_data$start_train_levels
              ))
          }
        }
        indxofstn <- which(is.element(
          shared_data$x_data,
          shared_data$selected_trains$Train.start.dates
        ))
        indxofstt <- which(shared_data$x_data == teststart)
        if (any(indxofstn > indxofstt)) {
          whichisbig <- which(indxofstn > indxofstt)
          shared_data$selected_trains <- shared_data$selected_trains |>
            slice(-whichisbig)
        }
      }
    })

    output$traindatestable <- renderDataTable({
      datatable(
        shared_data$selected_trains,
        options = list(
          dom = "t",
          pageLength = dim(shared_data$selected_trains)[1]
        )
      )
    })

    observeEvent(input$eliminatetrainsd, {
      rwstr <- input$traindatestable_rows_selected
      shared_data$selected_trains <- shared_data$selected_trains |>
        slice(-rwstr)
    })
  })
}
