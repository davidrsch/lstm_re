box::use(
    shiny.fluent[FontIcon, IconButton.shinyInput, Modal, reactOutput],
    shiny.fluent[Stack, Text, renderReact],
    shiny[NS, div, moduleServer, observeEvent],
)

box::use(
    app / logic / constants[status_mapping],
)

# Reusable modal module. Renders a Fluent UI Modal with a title bar that
# includes a status icon (color-coded by type), a title, and a close button.
# The modal body is arbitrary content passed by the caller.
#' @export
ui <- function(id) {
    ns <- NS(id)
    reactOutput(ns("make_modal"))
}

# The server receives:
# - name:         unique string id used for the close button data-testid
# - is_open:      reactiveVal(FALSE) that controls modal visibility
# - title:        modal header text
# - content:      shiny tag or tagList for the modal body
# - status:       one of "error", "info", "success", "warning"
# - status_table: lookup table mapping status to icon and color
#                 (defaults to constants$status_mapping)
#' @export
server <- function(
    id,
    name,
    is_open,
    title,
    content,
    status,
    status_table = status_mapping
) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        icon_name <- status_table[status_table$type == status, "icon"][[1]]
        icon_color <- status_table[status_table$type == status, "color"][[1]]
        div_icon_style <- paste0(
            "display: flex; flex-wrap: nowrap; justify-content: center;",
            " align-items: center; color: ",
            icon_color,
            ";"
        )

        # Close modal when the X button is clicked
        observeEvent(input$hideModal, is_open(FALSE))

        output$make_modal <- renderReact({
            Modal(
                isOpen = is_open(),
                Stack(
                    tokens = list(padding = "15px", childrenGap = "10px"),
                    div(
                        style = list(display = "flex"),
                        div(
                            FontIcon(iconName = icon_name),
                            style = div_icon_style
                        ),
                        Text(children = title, variant = "large"),
                        div(style = list(flexGrow = 1)),
                        IconButton.shinyInput(
                            ns("hideModal"),
                            iconProps = list(iconName = "Cancel")
                        )
                    ),
                    content
                )
            )
        })
    })
}
