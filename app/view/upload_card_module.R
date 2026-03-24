box::use(
  readr[locale, read_delim],
  readxl[read_excel],
  shiny.fluent[Checkbox.shinyInput, DefaultButton.shinyInput],
  shiny.fluent[PrimaryButton.shinyInput, Stack, TextField.shinyInput],
  shiny.fluent[updateCheckbox.shinyInput, updateTextField.shinyInput],
  shiny[
    div,
    fileInput,
    moduleServer,
    NS,
    observeEvent,
    reactiveVal,
    renderUI,
    req
  ],
  shiny[tagAppendAttributes, tagList, uiOutput],
  shinyjs[click, hidden, hide, toggle],
  shiny.fluent[updateDefaultButton.shinyInput],
  stats[na.omit],
  stringr[str_split_i],
  tools[file_ext],
)

box::use(
  app / logic / constants[file_formats],
  app / logic / make_card[make_card],
  app / logic / max_min_width_input[max_min_width_input],
  app / view / make_modal,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    make_modal$ui(ns("warning_modal")),
    make_card(
      tagList(
        "Upload data",
        DefaultButton.shinyInput(
          ns("toggle_upload_card"),
          iconProps = list(iconName = "ChevronUp"),
          style = "float: right; width: 0.7em",
          styles = list(
            root = list(
              "min-width" = "32px"
            )
          )
        )
      ),
      div(
        id = ns("upload_card_content"),
        Stack(
          tokens = list(childrenGap = 10),
          PrimaryButton.shinyInput(
            ns("file"),
            text = "",
            iconProps = list(iconName = "Upload"),
            `data-testid` = "file"
          ),
          Checkbox.shinyInput(
            ns("header"),
            label = "Has header?",
            value = TRUE,
            disabled = TRUE,
            inputProps = list(
              `data-testid` = "header"
            )
          ),
          Stack(
            horizontal = TRUE,
            tokens = list(childrenGap = "10%"),
            TextField.shinyInput(
              ns("delimiter"),
              label = "Delimiter",
              value = ",",
              disabled = TRUE,
              styles = max_min_width_input(45),
              `data-testid` = "delimiter"
            ),
            TextField.shinyInput(
              ns("decimal_point"),
              label = "Decimal point",
              value = ".",
              disabled = TRUE,
              styles = max_min_width_input(45),
              `data-testid` = "decimal_point"
            )
          )
        )
      ),
      style = "background-color: white;",
      is_contained = TRUE
    ),
    hidden(
      fileInput(ns("upload_file"), "") |>
        tagAppendAttributes(`data-testid` = "upload_file")
    )
  )
}

#' @export
server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    warning_visible <- reactiveVal(FALSE)
    warning_message <- reactiveVal("")
    output$warning_content <- renderUI(div(warning_message()))
    make_modal$server(
      "warning_modal",
      name = "warning_modal",
      is_open = warning_visible,
      title = "Warning",
      content = uiOutput(ns("warning_content")),
      status = "warning"
    )

    observeEvent(input$toggle_upload_card, {
      shared_data$upload_card_visible <- !shared_data$upload_card_visible
      toggle(id = ns("upload_card_content"))
      updateDefaultButton.shinyInput(
        session,
        "toggle_upload_card",
        iconProps = list(
          iconName = if (shared_data$upload_card_visible) {
            "ChevronUp"
          } else {
            "ChevronDown"
          }
        )
      )
    })

    observeEvent(
      shared_data$variables_card_visible,
      {
        if (
          shared_data$variables_card_visible && shared_data$upload_card_visible
        ) {
          shared_data$upload_card_visible <- FALSE
          hide(id = ns("upload_card_content"))
          updateDefaultButton.shinyInput(
            session,
            "toggle_upload_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      shared_data$data_amount_card_visible,
      {
        if (
          shared_data$data_amount_card_visible &&
            shared_data$upload_card_visible
        ) {
          shared_data$upload_card_visible <- FALSE
          hide(id = ns("upload_card_content"))
          updateDefaultButton.shinyInput(
            session,
            "toggle_upload_card",
            iconProps = list(iconName = "ChevronDown")
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      session,
      {
        updateCheckbox.shinyInput(
          session,
          "header",
          value = shared_data$header
        )
        updateTextField.shinyInput(
          session,
          "delimiter",
          value = shared_data$delimiter
        )
        updateTextField.shinyInput(
          session,
          "decimal_point",
          value = shared_data$decimal_point
        )
      },
      once = TRUE
    )
    observeEvent(input$file, {
      click("upload_file")
    })

    # Enabling or disabling inputs depending on the imported file
    # format
    observeEvent(input$upload_file, {
      if (!is.null(input$upload_file)) {
        file_path <- input$upload_file$datapath
        format <- str_split_i(file_path, "\\.", -1)
        if (is.element(format, file_formats[["extensions"]])) {
          updateCheckbox.shinyInput(
            inputId = "header",
            disabled = FALSE
          )
          if (
            is.element(
              format,
              file_formats[file_formats$type == "text", ][["extensions"]]
            )
          ) {
            updateTextField.shinyInput(
              inputId = "delimiter",
              disabled = FALSE,
              required = TRUE
            )
            updateTextField.shinyInput(
              inputId = "decimal_point",
              disabled = FALSE,
              required = TRUE
            )
          } else {
            updateTextField.shinyInput(
              inputId = "delimiter",
              disabled = TRUE
            )
            updateTextField.shinyInput(
              inputId = "decimal_point",
              disabled = TRUE
            )
          }
        } else {
          updateCheckbox.shinyInput(
            inputId = "header",
            disabled = TRUE
          )
          updateTextField.shinyInput(
            inputId = "delimiter",
            disabled = TRUE
          )
          updateTextField.shinyInput(
            inputId = "decimal_point",
            disabled = TRUE
          )
        }
      }
    })

    observeEvent(
      list(
        input$upload_file,
        input$header,
        input$delimiter,
        input$decimal_point
      ),
      {
        req(input$upload_file)
        ext <- file_ext(input$upload_file$name)
        if (ext %in% c("txt", "csv", "tsv", "fwf")) {
          shared_data$df <- read_delim(
            input$upload_file$datapath,
            delim = input$delimiter,
            col_names = input$header,
            locale = locale(decimal_mark = input$decimal_point)
          )
        } else if (ext %in% c("xlsx", "xls")) {
          shared_data$df <- read_excel(
            input$upload_file$datapath,
            col_names = as.logical(input$header)
          )
        }

        if (any(is.na(shared_data$df))) {
          shared_data$df <- na.omit(shared_data$df)
          warning_message(
            "Uploaded data has NaN values. Rows with NaN values have been removed."
          )
          warning_visible(TRUE)
        }
      }
    )
  })
}
