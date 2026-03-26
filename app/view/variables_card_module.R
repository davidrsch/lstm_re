box::use(
  dplyr[filter, select],
  rhandsontable[hot_col, hot_to_r, renderRHandsontable, rhandsontable],
  rhandsontable[rHandsontableOutput],
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack, Text],
  shiny.fluent[PrimaryButton.shinyInput],
  shiny[div, moduleServer, NS],
  shiny[observeEvent, renderUI, req, tagList, uiOutput],
  shinyjs[hidden, hide, runjs, toggle],
)

box::use(
  app / logic / make_card[make_card],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  make_card(
    tagList(
      "Select variables",
      uiOutput(ns("toggle_variables_card_ui"))
    ),
    hidden(
      div(
        id = ns("variables_card_content"),
        Stack(
          tokens = list(childrenGap = 10),
          uiOutput(ns("date_variable_dropdown")),
          div(
            `data-testid` = "io_gridtable",
            rHandsontableOutput(ns("io_gridtable"))
          ),
          Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = 10),
            Stack(
              tokens = list(childrenGap = 5),
              Text("Select All"),
              PrimaryButton.shinyInput(
                ns("select_all_inputs"),
                text = "Inputs"
              ),
              PrimaryButton.shinyInput(
                ns("select_all_outputs"),
                text = "Outputs"
              )
            ),
            div(style = "flex-grow: 1;"),
            Stack(
              tokens = list(childrenGap = 5),
              Text("Deselect All"),
              DefaultButton.shinyInput(
                ns("deselect_all_inputs"),
                text = "Inputs"
              ),
              DefaultButton.shinyInput(
                ns("deselect_all_outputs"),
                text = "Outputs"
              )
            )
          )
        )
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

    # Re-render the toggle button reactively so disabled state and icon are
    # always correct without relying on updateDefaultButton.shinyInput.
    output$toggle_variables_card_ui <- renderUI({
      disabled <- is.null(shared_data$df) || nrow(shared_data$df) == 0
      icon <- if (isTRUE(shared_data$variables_card_visible)) {
        "ChevronUp"
      } else {
        "ChevronDown"
      }
      DefaultButton.shinyInput(
        ns("toggle_variables_card"),
        disabled = disabled,
        iconProps = list(iconName = icon),
        style = "float: right; width: 0.7em",
        styles = list(
          root = list(
            "min-width" = "32px"
          )
        ),
        `data-testid` = "toggle_variables_card"
      )
    })

    observeEvent(input$toggle_variables_card, {
      shared_data$variables_card_visible <- !shared_data$variables_card_visible
      toggle("variables_card_content")
      if (shared_data$variables_card_visible) {
        runjs(paste0(
          "[...document.querySelectorAll('[role=\"tab\"]')]",
          ".find(el => el.textContent.trim() === 'EDA')",
          "?.click();"
        ))
      }
    })

    observeEvent(
      shared_data$upload_card_visible,
      {
        if (
          shared_data$upload_card_visible && shared_data$variables_card_visible
        ) {
          shared_data$variables_card_visible <- FALSE
          hide("variables_card_content")
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      shared_data$data_amount_card_visible,
      {
        if (
          shared_data$data_amount_card_visible &&
            shared_data$variables_card_visible
        ) {
          shared_data$variables_card_visible <- FALSE
          hide("variables_card_content")
        }
      },
      ignoreInit = TRUE
    )

    output$date_variable_dropdown <- renderUI({
      req(shared_data$df) # Ensure df is available before rendering dropdown

      dropdown_options <- lapply(colnames(shared_data$df), function(col) {
        list(key = col, text = col)
      })

      Dropdown.shinyInput(
        ns("datevariable"),
        label = "Date-sequence variable",
        options = dropdown_options,
        value = shared_data$selected_date_variable,
        key = "datevariable_dropdown_key" # Keep the key for stability
      )
    })

    observeEvent(input$datevariable, {
      shared_data$selected_date_variable <- input$datevariable
    })

    # 06-Creating input-output grid----
    observeEvent(list(shared_data$df, shared_data$selected_date_variable), {
      if (
        is.null(shared_data$selected_date_variable) ||
          shared_data$selected_date_variable == ""
      ) {
        data <- shared_data$df
      } else {
        data <- shared_data$df |> select(-shared_data$selected_date_variable)
      }
      colamount <- dim(data)[2]
      variables <- colnames(data)
      shared_data$grid <- data.frame(
        Inputs = rep(FALSE, colamount),
        Outputs = rep(FALSE, colamount),
        Variables = variables
      )
    })

    output$io_gridtable <- renderRHandsontable({
      req("Variables" %in% names(shared_data$grid))
      rhandsontable(
        shared_data$grid,
        disableVisualSelection = TRUE,
        width = "100%",
      ) |>
        hot_col("Variables", readOnly = TRUE)
    })

    # 07-EDA ----
    observeEvent(input$io_gridtable, {
      if (any(hot_to_r(input$io_gridtable) == 1)) {
        if (shared_data$show_eda < 2) {
          shared_data$show_eda <- shared_data$show_eda + 1
        }
        # Update shared_data$grid with the latest state from the handsontable
        shared_data$grid <- hot_to_r(input$io_gridtable)

        inp <- shared_data$grid |> filter(Inputs == 1) |> select(Variables)
        out <- shared_data$grid |> filter(Outputs == 1) |> select(Variables)
        variables <- merge(inp, out, all = TRUE)[[1]]
        data <- shared_data$df[variables]
        if (!identical(data, shared_data$previousEDA)) {
          shared_data$EDA <- data
          if (shared_data$show_eda == 1) {
            runjs(paste0(
              "[...document.querySelectorAll('[role=\"tab\"]')]",
              ".find(el => el.textContent.trim() === 'EDA')",
              "?.click();"
            ))
          }
        }
      }
    })

    # Server-side logic for select/deselect all buttons
    observeEvent(input$select_all_inputs, {
      shared_data$grid$Inputs <- TRUE
      inp <- shared_data$grid |> filter(Inputs == 1) |> select(Variables)
      out <- shared_data$grid |> filter(Outputs == 1) |> select(Variables)
      variables <- merge(inp, out, all = TRUE)[[1]]
      if (length(variables) > 0) {
        shared_data$EDA <- shared_data$df[variables]
      }
    })

    observeEvent(input$select_all_outputs, {
      shared_data$grid$Outputs <- TRUE
      inp <- shared_data$grid |> filter(Inputs == 1) |> select(Variables)
      out <- shared_data$grid |> filter(Outputs == 1) |> select(Variables)
      variables <- merge(inp, out, all = TRUE)[[1]]
      if (length(variables) > 0) {
        shared_data$EDA <- shared_data$df[variables]
      }
    })

    observeEvent(input$deselect_all_inputs, {
      shared_data$grid$Inputs <- FALSE
    })

    observeEvent(input$deselect_all_outputs, {
      shared_data$grid$Outputs <- FALSE
    })
  })
}
