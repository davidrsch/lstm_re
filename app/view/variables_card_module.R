box::use(
  dplyr[filter, select],
  rhandsontable[hot_col, hot_to_r, renderRHandsontable, rhandsontable],
  rhandsontable[rHandsontableOutput],
  shiny.fluent[DefaultButton.shinyInput, Dropdown.shinyInput, Stack, Text],
  shiny.fluent[PrimaryButton.shinyInput, updateDefaultButton.shinyInput],
  shiny[div, isolate, moduleServer, NS, renderUI, uiOutput],
  shiny[observeEvent, req, tagList, reactiveVal],
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
      DefaultButton.shinyInput(
        ns("toggle_variables_card"),
        disabled = TRUE,
        iconProps = list(iconName = "ChevronDown"),
        style = "float: right; width: 0.7em",
        styles = list(root = list("min-width" = "32px")),
        `data-testid` = "toggle_variables_card"
      )
    ),
    hidden(
      div(
        id = ns("variables_card_content"),
        Stack(
          tokens = list(childrenGap = 10),
          uiOutput(ns("datevariable_ui")),
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
    grid_trigger <- reactiveVal(0)

    # Enable the toggle button once data is loaded.
    observeEvent(shared_data$df, {
      disabled <- is.null(shared_data$df) || nrow(shared_data$df) == 0
      updateDefaultButton.shinyInput(
        session, "toggle_variables_card", disabled = disabled
      )
    })

    observeEvent(input$toggle_variables_card, {
      shared_data$variables_card_visible <- !shared_data$variables_card_visible
      toggle("variables_card_content")
      updateDefaultButton.shinyInput(
        session, "toggle_variables_card",
        iconProps = list(
          iconName = if (shared_data$variables_card_visible) "ChevronUp" else "ChevronDown"
        )
      )
      if (shared_data$variables_card_visible) {
        runjs(paste0(
          "[...document.querySelectorAll('[role=\"tab\"]')]",
          ".find(el => el.textContent.trim() === 'EDA')",
          "?.click();"
        ))
      }
    })

    box::use(app / logic / ui_helpers[collapse_on_sibling_open])
    collapse_on_sibling_open(
      "upload_card_visible",
      "variables_card_visible",
      shared_data,
      session,
      "variables_card_content",
      "toggle_variables_card"
    )

    collapse_on_sibling_open(
      "data_amount_card_visible",
      "variables_card_visible",
      shared_data,
      session,
      "variables_card_content",
      "toggle_variables_card"
    )

    # Render the date dropdown via renderUI so [data-testid="datevariable"]
    # only exists in DOM once df (and options) are ready. This eliminates the
    # async shiny.react reconciliation window between "element visible" and
    # "options applied to React state" that existed with updateDropdown.shinyInput.
    # The card is always closed when df first changes, so no toggle-race applies.
    output$datevariable_ui <- renderUI({
      req(shared_data$df)
      dropdown_options <- lapply(colnames(shared_data$df), function(col) {
        list(key = col, text = col)
      })
      current_val <- isolate(shared_data$selected_date_variable)
      Dropdown.shinyInput(
        ns("datevariable"),
        label = "Date-sequence variable",
        options = dropdown_options,
        value = if (!is.null(current_val) && current_val != "") current_val else NULL,
        `data-testid` = "datevariable"
      )
    })

    observeEvent(input$datevariable, {
      # Guard against empty-string events emitted by the FluentUI Dropdown
      # when its containing card is CSS-hidden (shinyjs::hide). An empty
      # string would reset selected_date_variable and cascade-reset the grid.
      val <- input$datevariable
      if (is.null(val) || nchar(val) == 0L) return()
      shared_data$selected_date_variable <- val
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
      grid_trigger(grid_trigger() + 1)
    })

    output$io_gridtable <- renderRHandsontable({
      grid_trigger() # take dependency
      req("Variables" %in% names(isolate(shared_data$grid)))
      rhandsontable(
        isolate(shared_data$grid),
        disableVisualSelection = TRUE,
        width = "100%",
      ) |>
        hot_col("Variables", readOnly = TRUE)
    })

    # 07-EDA ----
    # Debounce the grid input to prevent rapid feedback loops
    grid_input_debounced <- reactiveVal()
    observeEvent(input$io_gridtable, {
      grid_input_debounced(input$io_gridtable)
    })
    
    grid_input_effective <- shiny::debounce(grid_input_debounced, 500)

    observeEvent(grid_input_effective(), {
      req(grid_input_effective()$changes$changes)
      
      current_grid <- hot_to_r(grid_input_effective())
      shared_data$grid <- current_grid
      
      if (any(current_grid == 1)) {
        if (shared_data$show_eda < 2) {
          shared_data$show_eda <- shared_data$show_eda + 1
        }
        
        inp <- current_grid |> filter(Inputs == 1) |> select(Variables)
        out <- current_grid |> filter(Outputs == 1) |> select(Variables)
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
      } else {
        # If nothing is selected, clear EDA
        shared_data$EDA <- data.frame()
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
      grid_trigger(grid_trigger() + 1)
    })

    observeEvent(input$select_all_outputs, {
      shared_data$grid$Outputs <- TRUE
      inp <- shared_data$grid |> filter(Inputs == 1) |> select(Variables)
      out <- shared_data$grid |> filter(Outputs == 1) |> select(Variables)
      variables <- merge(inp, out, all = TRUE)[[1]]
      if (length(variables) > 0) {
        shared_data$EDA <- shared_data$df[variables]
      }
      grid_trigger(grid_trigger() + 1)
    })

    observeEvent(input$deselect_all_inputs, {
      shared_data$grid$Inputs <- FALSE
      grid_trigger(grid_trigger() + 1)
    })

    observeEvent(input$deselect_all_outputs, {
      shared_data$grid$Outputs <- FALSE
      grid_trigger(grid_trigger() + 1)
    })
  })
}
