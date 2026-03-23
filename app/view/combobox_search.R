box::use(
  purrr[discard],
  shiny.fluent[ComboBox.shinyInput, updateComboBox.shinyInput],
  shiny[getDefaultReactiveDomain, moduleServer, NS, observeEvent, req],
  stringr[str_detect, str_replace, str_split_fixed, str_to_lower],
)

#' @export
ui <- function(
  id,
  cb_label,
  default_key,
  default_text,
  cb_options,
  is_visible
) {
  ns <- NS(id)
  ComboBox.shinyInput(
    ns("searchable_cb"),
    label = ifelse(is_visible, cb_label, ""),
    value = list(
      key = default_key,
      text = default_text
    ),
    allowFreeform = TRUE,
    useComboBoxAsMenuWidth = TRUE,
    options = cb_options,
    styles = list(
      root = list(
        "visibility" = ifelse(is_visible, "show", "hidden"),
        "display" = ifelse(is_visible, "block", "none")
      )
    ),
    calloutProps = list(
      styles = list(
        root = list(
          "max-height" = "300px!important"
        )
      )
    ),
    `data-test` = paste0(str_split_fixed(id, "-", 3)[3], "-cb_search")
  )
}

#' @export
server <- function(id, value, cb_label, default_text, cb_options, is_visible) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Implement fuzy search in the combobox
    observeEvent(input$searchable_cb_query, {
      query <- input$searchable_cb_query
      all_options <- cb_options

      if (query == "") {
        query <- default_text
      }

      # Filter options based on the query
      if (!is.null(query)) {
        filtered_options <- all_options |>
          lapply(function(x) {
            query_to <- query |>
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            text <- x$text |>
              str_replace("\\(", "") |>
              str_replace("\\)", "") |>
              str_to_lower()
            if (str_detect(text, query_to)) {
              x
            } else {
              NULL
            }
          })
        filtered_options <- filtered_options |>
          discard(is.null)
      } else {
        filtered_options <- all_options
      }

      # Update the ComboBox with filtered options
      updateComboBox.shinyInput(
        session = getDefaultReactiveDomain(),
        "searchable_cb",
        label = ifelse(is_visible(), cb_label, ""),
        options = filtered_options,
        styles = list(
          root = list(
            "visibility" = ifelse(is_visible(), "show", "hidden"),
            "display" = ifelse(is_visible(), "block", "none")
          )
        )
      )
    })

    # # Update combo box to include all options once one it's selected
    observeEvent(input$searchable_cb, {
      value(
        list(
          key = input$searchable_cb$key,
          text = input$searchable_cb$text
        )
      )
    })

    observeEvent(is_visible(), {
      req(input$searchable_cb)
      all_options <- cb_options
      updateComboBox.shinyInput(
        label = ifelse(is_visible(), cb_label, ""),
        session = getDefaultReactiveDomain(),
        "searchable_cb",
        styles = list(
          root = list(
            "visibility" = ifelse(is_visible(), "show", "hidden"),
            "display" = ifelse(is_visible(), "block", "none")
          )
        ),
        options = all_options
      )
    })

    observeEvent(value(), {
      req(input$searchable_cb)
      if (!is.null(value())) {
        all_options <- cb_options
        updateComboBox.shinyInput(
          label = ifelse(is_visible(), cb_label, ""),
          session = getDefaultReactiveDomain(),
          value = list(
            key = value()$key,
            text = value()$text
          ),
          "searchable_cb",
          styles = list(
            root = list(
              "visibility" = ifelse(is_visible(), "show", "hidden"),
              "display" = ifelse(is_visible(), "block", "none")
            )
          ),
          options = all_options
        )
      }
    })
  })
}
