box::use(
  jsonlite[toJSON],
  keras3[callback_lambda],
  shinyjs[runjs],
)


# Fires a JavaScript Shiny.setInputValue event to update a Fluent ProgressIndicator
# to the fraction (item + 1) / amount.
#' @export
update_progress <- function(session, pgbid, amount, item) {
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


# Applies a Plotly relayout to set axis titles, tick values, legend position,
# and removes the mode bar on the training loss chart.
#' @export
update_layout <- function(x, valuesofx, plotid) {
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

# Adds a new scatter-line trace to the Plotly loss chart with the full x/loss
# vectors for the current model.
#' @export
add_traces <- function(x, loss, plotid) {
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

# Extends the last trace on the Plotly loss chart by appending the most recent
# loss value, enabling live epoch-by-epoch updates.
#' @export
extend_traces <- function(loss, plotid) {
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

# Removes the last trace from the Plotly loss chart. Called before drawing the
# first epoch of each new model to clear the previous model's trace.
#' @export
delete_traces <- function(plotid) {
  runjs(paste0(
    "
  var graphDiv = document.getElementById(\"",
    plotid,
    "\");
  Plotly.deleteTraces(graphDiv,[-1]);"
  ))
}

# Handles the Keras on_epoch_end event: initialises the chart layout on the
# first epoch, adds a fresh trace for each new model, and extends the trace
# on subsequent epochs.
#' @export
on_epoch_end <- function(
  nmodel,
  loss_store,
  plotid,
  amountofepoch,
  epoch,
  logs
) {
  logs <- lapply(logs, as.numeric)
  plotx <- seq_len(amountofepoch)
  if (plotx[length(plotx)] > 10) {
    xticksvalues <- seq(
      1,
      plotx[length(plotx)],
      round(plotx[length(plotx)] / 10)
    )
  } else {
    xticksvalues <- plotx
  }

  if (is.null(loss_store$loss)) {
    if (nmodel != 1) {
      delete_traces(plotid)
    }
    update_layout(plotx, xticksvalues, plotid)
    loss <- logs$loss
    add_traces(plotx, loss, plotid)
    loss_store$loss <- loss
  } else {
    loss_store$loss <- c(loss_store$loss, logs$loss)
    extend_traces(loss_store$loss, plotid)
  }
}

# Builds a Keras callback_lambda that reports batch and epoch progress to
# Shiny progress bars and delegates loss-plot updates to on_epoch_end.
#' @export
create_callback <- function(
  nmodel,
  session,
  loss_store,
  plotid,
  batchpbid,
  batchamount,
  epochpbid,
  epochamount
) {
  callback_lambda(
    on_batch_begin = function(batch, logs) {
      update_progress(
        session = session,
        pgbid = batchpbid,
        amount = batchamount,
        item = batch
      )
    },
    on_epoch_begin = function(epoch, logs) {
      update_progress(
        session = session,
        pgbid = epochpbid,
        amount = epochamount,
        item = epoch
      )
    },
    on_epoch_end = function(epoch, logs) {
      on_epoch_end(
        nmodel = nmodel,
        loss_store = loss_store,
        plotid = plotid,
        amountofepoch = epochamount,
        epoch = epoch,
        logs = logs
      )
    }
  )
}
