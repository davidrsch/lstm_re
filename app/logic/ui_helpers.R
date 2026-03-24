box::use(
  DT[DTOutput],
  purrr[map_dfr],
  shiny.fluent[
    DefaultButton.shinyInput,
    PrimaryButton.shinyInput,
    updateDefaultButton.shinyInput
  ],
  shiny[div, observeEvent, p, tagList, tags, uiOutput],
  shinyjs[hide],
)

# Generates the full Cartesian product of neuron amounts across all LSTM layer
# counts and returns it as a tidy data frame, one column per layer.
#' @export
find_models <- function(lstm, neurons) {
  map_dfr(lstm, function(n_lstm) {
    df <- expand.grid(rep(list(neurons), n_lstm))
    names(df) <- lapply(seq_len(n_lstm), function(i) paste0(i, "_LSTM"))
    df
  })
}

# Builds the modal content for the experiment confirmation screen, summarising
# the selected training sets, transformations, scales, input amounts, and the
# full model grid with DT table and OK/Cancel/Eliminate buttons.
#' @export
select_models_to_build <- function(ns, train, ts, sc, vec, lstm, neu) {
  amountoftrain <- dim(train)[1]
  if (dim(train)[1] == 1) {
    setors <- "set"
  } else {
    setors <- "sets"
  }
  if (length(ts) == 1) {
    tfors <- "transformation"
  } else {
    tfors <- "transformations"
  }
  if (length(sc) == 1) {
    scors <- "scale"
  } else {
    scors <- "scales"
  }
  if (length(vec) == 1) {
    inputors <- "amount"
  } else {
    inputors <- "amounts"
  }
  models <- find_models(lstm = lstm, neurons = neu)
  if (dim(models)[1] == 1) {
    modelors <- "model"
  } else {
    modelors <- "models"
  }
  text <- tagList(
    div(
      style = paste0(
        "text-align:left; margin-left: 2em; margin-right: 2em; margin-bottom: 2em;",
        "margin-top: 1em; min-width: 60em; max-width: 80em; overflow: auto;"
      ),
      p("There have been selected:", style = "color:black"),
      tags$ul(
        tags$li(
          as.character(amountoftrain),
          " ",
          setors,
          " of the data.",
          style = "color:black"
        ),
        tags$li(
          as.character(length(ts)),
          " ",
          tfors,
          ".",
          style = "color:black"
        ),
        tags$li(
          as.character(length(sc)),
          " ",
          scors,
          ".",
          style = "color:black"
        ),
        tags$li(
          as.character(length(vec)),
          " input ",
          inputors,
          ".",
          style = "color:black"
        )
      ),
      uiOutput(ns("modelstobuildtext")),
      div(
        style = "text-align:center",
        p(
          tags$b(paste0(modelors, " per vector")),
          style = "color: black;"
        )
      ),
      div(
        id = "containingmodelstable",
        DTOutput(ns("modelestable")),
        style = "border-radius: 0;
          border: black thin solid;
          max-height: 350px;
          overflow: auto;
          text-align: center"
      ),
      div(style = "height:20px"),
      div(
        DefaultButton.shinyInput(
          ns("eliminatemodel"),
          text = "Eliminate",
          styles = list(
            root = list(
              float = "right"
            )
          )
        ),
        style = "display: flow-root;"
      ),
      div(style = "height:20px"),
      div(
        PrimaryButton.shinyInput(
          ns("acceptmodels"),
          text = "OK"
        ),
        DefaultButton.shinyInput(
          ns("cancelmodels"),
          text = "Cancel"
        ),
        style = "display: flex; justify-content: space-between"
      )
    )
  )
  text
}

# Extracts the rightmost n characters from string x.
#' @export
substright <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

# Collapses a character vector into a single space-separated string.
#' @export
paste_vec <- function(vect) {
  paste(vect, collapse = " ")
}

# Converts a data frame to an inline HTML table string with basic black-border
# styling. Used to embed tabular content in JavaScript-driven UI elements.
#' @export
html_table <- function(df) {
  x <- "<div>
        <style>th, td {border: 1px solid black;margin:0px;padding: 5px;}
                table{
                    border-collapse:collapse;
                    font-size:12px;
                    width: 100%;
                    text-align:center}</style><table>"
  colnames <- names(df)
  for (col in seq_along(colnames)) {
    if (col == 1) {
      x <- paste(
        x,
        "<tr><th style = \"border-color:black;\"><b>",
        colnames[col],
        "</b></th>"
      )
    } else {
      if (col > 1 && col < length(colnames)) {
        x <- paste(
          x,
          "<th style = \"border-color:black;\"><b>",
          colnames[col],
          "</b></th>"
        )
      } else {
        x <- paste(
          x,
          "<th style = \"border-color:black;\"><b>",
          colnames[col],
          "</b></th></tr>"
        )
      }
    }
  }
  for (r in seq_len(dim(df)[1])) {
    x <- paste(x, "<tr>")
    for (cl in seq_len(dim(df)[2])) {
      x <- paste(
        x,
        "<td style = \"border-color:black;\">",
        df[r, cl][[1]],
        "</td>"
      )
    }
    x <- paste(x, "</tr>")
  }
  x <- paste(x, "</table></div>")
  x
}

# Registers an observer that collapses this accordion card whenever a sibling
# card opens. Call once per sibling from inside moduleServer, passing the
# shared visibility reactiveValues and the module's session object.
#' @export
collapse_on_sibling_open <- function(
  sibling_flag,
  this_flag,
  visibility,
  session
) {
  observeEvent(
    visibility[[sibling_flag]],
    {
      if (visibility[[sibling_flag]] && visibility[[this_flag]]) {
        visibility[[this_flag]] <- FALSE
        hide("card_content")
        updateDefaultButton.shinyInput(
          session,
          "toggle_card",
          iconProps = list(iconName = "ChevronDown")
        )
      }
    },
    ignoreInit = TRUE
  )
}
