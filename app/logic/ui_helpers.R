box::use(
  dplyr[bind_rows],
  DT[DTOutput],
  shiny.fluent[DefaultButton.shinyInput, PrimaryButton.shinyInput],
  shiny[div, p, tagList, tags, uiOutput],
)

#' @export
startalert <- tagList(
  div(
    p(
      "Check have selected at least:"
    ),
    tags$ul(
      tags$li("A time serie to use."),
      tags$li("A scale to use."),
      tags$li("Specified a temporal horizon."),
      tags$li("Specified an input amount."),
      tags$li("Specified a LSTM layers amount."),
      tags$li("Specified a neurons amount."),
      tags$li("Specified an epoch amount.")
    ),
    style = "text-align:left; margin-left: 20%"
  )
)

#' @export
findmodels <- function(lstm, neurons) {
  for (i in seq_along(lstm)) {
    if (i == 1) {
      df <- expand.grid(rep(list(neurons), lstm[i]))
      colsnames <- lapply(1:lstm[i], function(x) paste0(x, "_LSTM"))
      names(df) <- colsnames
    } else {
      df2 <- expand.grid(rep(list(neurons), lstm[i]))
      colsnames <- lapply(1:lstm[i], function(x) paste0(x, "_LSTM"))
      names(df2) <- colsnames
      df <- bind_rows(df, df2)
    }
  }
  df
}

#' @export
selectmodelstobuild <- function(ns, train, ts, sc, vec, lstm, neu) {
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
  models <- findmodels(lstm = lstm, neurons = neu)
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

#' @export
substright <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' @export
pastevec <- function(vect) {
  for (i in seq_along(vect)) {
    if (i == 1) {
      x <- vect[i]
    } else {
      x <- paste(x, vect[i])
    }
  }
  x
}

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
