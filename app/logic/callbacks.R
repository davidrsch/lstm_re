box::use(
  jsonlite[fromJSON, toJSON, write_json],
  keras3[callback_lambda],
  shinyjs[runjs],
)


#' @export
updatingpg <- function(session, pgbid, amount, item) {
  progress <- (item + 1) / amount
  runjs(
    paste0(
      "Shiny.setInputValue(\"",
      pgbid,
      "\", ",
      progress,
      ", {priority: 'event'})"
    )
  )
}


#' @export
updatelayoutfunc <- function(x, valuesofx, plotid) {
  runjs(paste0(
    "
        var graphDiv = document.getElementById(\"",
    plotid,
    "\");
        var update = {
         \"margin\": {
            \"b\": 40,
            \"l\": 60,
            \"t\": 25,
            \"r\": 10
         },
         xaxis: {
           \"domain\": [0, 1],
           \"automargin\": true,
           \"title\": \"\",
           \"showgrid\": false,
           \"linecolor\": \"black\",
           \"ticks\": \"outside\",
           tickvals: ",
    toJSON(valuesofx),
    ",
           \"range\": [0.9,",
    (x[length(x)] + 0.1),
    "]
         },
         yaxis: {
           \"domain\": [0, 1],
           \"automargin\": true,
           \"title\": \"\",
           \"showgrid\": false,
           \"linecolor\": \"black\",
           rangemode: \"normal\",
           \"ticks\": \"outside\"
         },
         legend: {
           orientation: \"h\",
           x: 0.40
         },
         hovermode: \"x unified\",
         dragmode: false,
         showlegend: true
        };
        Plotly.relayout(graphDiv, update);
                     "
  ))
}

#' @export
addtracesfunction <- function(x, loss, plotid) {
  runjs(paste0(
    "
        var graphDiv = document.getElementById(\"",
    plotid,
    "\");
        Plotly.addTraces(graphDiv,[{
            mode: \"lines+markers\",
            alpha_stroke: 1,
            sizes: [10, 100],
            spans: [1, 20],
            type: \"scatter\",
            x:",
    toJSON(x),
    " ,
            y:",
    toJSON(loss),
    ",
            name: \"loss\",
            line: {
              color: \"blue\"
            },
            marker: {
              color: \"blue\"
            },
            inherit: true
           }
    ]);
                     "
  ))
}

#' @export
extendtraces <- function(loss, epoch, plotid) {
  runjs(paste0(
    "
        var graphDiv = document.getElementById(\"",
    plotid,
    "\");
        Plotly.extendTraces(graphDiv, { y: [",
    toJSON(loss[length(loss)]),
    "]},[1]);"
  ))
}

#' @export
eliminatetraces <- function(plotid) {
  runjs(paste0(
    "
  var graphDiv = document.getElementById(\"",
    plotid,
    "\");
  Plotly.deleteTraces(graphDiv,[-1]);"
  ))
}

#' @export
onepochend <- function(nmodel, directory, plotid, amountofepoch, epoch, logs) {
  logs <- lapply(logs, as.numeric)
  plotx <- 1:amountofepoch
  if (plotx[length(plotx)] > 10) {
    xticksvalues <- seq(
      1,
      plotx[length(plotx)],
      round(plotx[length(plotx)] / 10)
    )
  } else {
    xticksvalues <- plotx
  }

  loss_file <- paste0(directory, "loss.json")

  if (!file.exists(loss_file)) {
    if (nmodel != 1) {
      eliminatetraces(plotid)
    }
    updatelayoutfunc(plotx, xticksvalues, plotid)
    loss <- logs$loss
    addtracesfunction(plotx, loss, plotid)
    write_json(loss, loss_file)
  } else {
    loss <- fromJSON(loss_file)
    loss <- c(loss, logs$loss)
    extendtraces(loss, epoch, plotid)
    write_json(loss, loss_file)
  }
}

#' @export
creatingcallback <- function(
  nmodel,
  session,
  directory,
  plotid,
  batchpbid,
  batchamount,
  epochpbid,
  epochamount
) {
  cd <- callback_lambda(
    on_batch_begin = function(batch, logs) {
      updatingpg(
        session = session,
        pgbid = batchpbid,
        amount = batchamount,
        item = batch
      )
    },
    on_epoch_begin = function(epoch, logs) {
      updatingpg(
        session = session,
        pgbid = epochpbid,
        amount = epochamount,
        item = epoch
      )
    },
    on_epoch_end = function(epoch, logs) {
      onepochend(
        nmodel = nmodel,
        directory = directory,
        plotid = plotid,
        amountofepoch = epochamount,
        epoch = epoch,
        logs = logs
      )
    }
  )
  cd
}
