box::use(
  abind[abind],
  dplyr[across, filter, mutate, mutate_if, pull, select, select_if, where],
  english[ordinal],
  htmlwidgets[onRender],
  jsonlite[fromJSON, toJSON, write_json],
  keras3[clear_session, compile, fit, save_model],
  plotly[config, plot_ly, plotlyOutput, renderPlotly],
  rmarkdown[render],
  shiny.fluent[CommandBar, Pivot, PivotItem, ProgressIndicator, Stack],
  shiny[div, moduleServer, NS, observe, observeEvent, reactiveVal],
  shiny[reactiveValues, reactiveValuesToList, renderUI, req, uiOutput],
  shiny[wellPanel],
  shinyalert[shinyalert],
  shinyjs[html, runjs],
  shinyWidgets[updatePickerInput],
  utils[type.convert],
)

box::use(
  app / logic / callbacks[creatingcallback],
  app / logic / models[createmodel],
  app / logic / testing[creatingplotpreddf, gettingmetrics, predictwkeras],
  app / logic / time_series[createscts, createtrfts, createts],
  app / logic / ui_helpers[html_table, pastevec],
  app / logic / vectors[threedvectfunc],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    Stack(
      tokens = list(childrenGap = 10),
      CommandBar(
        items = list(
          list(
            key = "download",
            text = "Download",
            iconProps = list(iconName = "Download"),
            subMenuProps = list(
              items = list(
                list(key = "dashboard", text = "Dashboard"),
                list(key = "models", text = "Models")
              )
            )
          )
        )
      ),
      uiOutput(ns("pivot_placeholder"))
    )
  )
}

#' @export
server <- function(id, sf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    r <- reactiveValues()
    r$tab <- NULL
    r$calculation <- 0
    r$progress <- reactiveValues()
    pivot_items <- reactiveVal(list())
    selected_key <- reactiveVal()

    observe({
      inputs <- reactiveValuesToList(input)
      req(r$progress)
      lapply(names(inputs), function(key) {
        if (grepl("_batch_update$", key)) {
          tabname <- sub(paste0(ns(""), "(.*)_batch_update$"), "\\1", key)
          if (!is.null(r$progress[[tabname]])) {
            r$progress[[tabname]]$batch <- inputs[[key]]
          }
        }
        if (grepl("_epoch_update$", key)) {
          tabname <- sub(paste0(ns(""), "(.*)_epoch_update$"), "\\1", key)
          if (!is.null(r$progress[[tabname]])) {
            r$progress[[tabname]]$epoch <- inputs[[key]]
          }
        }
      })
    })

    output$pivot_placeholder <- renderUI({
      do.call(
        Pivot,
        c(list(selectedKey = selected_key()), pivot_items())
      )
    })

    observeEvent(sf$run_experiment, {
      r$dashboard <- "A"
      r$modelsdir <- "A"

      runjs(
        "document.querySelector('[data-content=\"Results\"]').click();"
      )

      if (is.null(r$tab)) {
        r$tab <- 1
      } else {
        r$tab <- r$tab + 1
      }

      amountofts <- dim(sf$selectedtrains)[1] *
        length(sf$transf) *
        length(sf$scales)
      amountofvec <- amountofts * length(sf$inputamnts)
      amountofmodels <- amountofvec * dim(sf$modelstable)[1]

      tabname <- paste0(
        " ",
        as.character(r$tab),
        " ",
        ordinal(r$tab),
        " "
      )
      r$tabname <- paste0(as.character(r$tab), ordinal(r$tab))

      new_key <- r$tabname
      r$progress[[new_key]] <- reactiveValues(batch = 0, epoch = 0, model = 0)

      new_item_ui <- div(
        id = paste0(r$tabname, "content"),
        div(
          class = "ms-Grid-row",
          style = "display: flex; flex-wrap: wrap;",
          div(class = "ms-Grid-col ms-lg2"),
          div(
            class = "ms-Grid-col ms-lg8",
            wellPanel(
              div(
                id = paste0(r$tabname, "htmlmodelfeatT"),
                style = "margin-bottom: 4px"
              ),
              div(
                id = paste0(r$tabname, "livefitpcon"),
                plotlyOutput(
                  outputId = ns(paste0(r$tabname, "liveplot")),
                  height = "290px"
                )
              ),
              div(
                id = paste0(r$tabname, "containingpbs"),
                uiOutput(ns(paste0(r$tabname, "batchpb_ui"))),
                uiOutput(ns(paste0(r$tabname, "epochpb_ui"))),
                uiOutput(ns(paste0(r$tabname, "modelpb_ui")))
              ),
              style = "background-color: white;
              border-color:black;border-radius:0;
              height:100%; margin-top: 1%"
            )
          ),
          div(class = "ms-Grid-col ms-lg2"),
          style = "height:480px"
        )
      )

      new_item <- PivotItem(
        headerText = tabname,
        itemKey = new_key,
        new_item_ui
      )

      pivot_items(c(pivot_items(), list(new_item)))
      selected_key(new_key)

      output[[paste0(r$tabname, "batchpb_ui")]] <- renderUI({
        req(r$progress[[r$tabname]])
        ProgressIndicator(
          id = ns(paste0(r$tabname, "batchpb")),
          percentComplete = r$progress[[r$tabname]]$batch,
          description = "Samples:"
        )
      })
      output[[paste0(r$tabname, "epochpb_ui")]] <- renderUI({
        req(r$progress[[r$tabname]])
        ProgressIndicator(
          id = ns(paste0(r$tabname, "epochpb")),
          percentComplete = r$progress[[r$tabname]]$epoch,
          description = "Epochs:"
        )
      })
      output[[paste0(r$tabname, "modelpb_ui")]] <- renderUI({
        req(r$progress[[r$tabname]])
        ProgressIndicator(
          id = ns(paste0(r$tabname, "modelpb")),
          percentComplete = r$progress[[r$tabname]]$model,
          description = "Models:"
        )
      })

      output[[paste0(r$tabname, "liveplot")]] <- renderPlotly({
        plot_ly(type = "scatter", mode = "markers") |>
          config(displayModeBar = FALSE) |>
          onRender(paste0(
            " 
            function(){
              console.log('onRender fired!');
              Shiny.setInputValue(\"",
            ns("rcalculation"),
            "\", 1, {priority: \"event\"});
            }"
          ))
      })
    })

    r$path_of_directorio <- paste0("app/static/", session$token)
    observe({
      dir.create(r$path_of_directorio)
    })

    observeEvent(input$rcalculation, {
      if (input$rcalculation == 1) {
        exp_directory <- paste0(r$path_of_directorio, "/", r$tabname)
        exp_models <- paste0(exp_directory, "/models")
        exp_dashdirect <- paste0(exp_directory, "/dashboard")
        exp_dashdata <- paste0(exp_dashdirect, "/www")
        loss_directory <- paste0(r$path_of_directorio, "/plotdata/")
        dir.create(exp_directory)
        dir.create(exp_models)
        dir.create(exp_dashdirect)
        dir.create(exp_dashdata)
        dir.create(loss_directory)
        amountofts <- dim(sf$selectedtrains)[1]
        amountoftf <- length(sf$transf)
        amountofsc <- length(sf$scales)
        amountofinps <- length(sf$inputamnts)
        print(sf$inputamnts)
        amountofmodels <- dim(sf$modelstable)[1]
        amountoftotalmodels <- amountofts *
          amountoftf *
          amountofsc *
          amountofinps *
          amountofmodels
        modelbuilding <- 0
        if (sf$setseed == TRUE) {
          set.seed(sf$seed)
        }
        if (is.null(sf$selectedtrains) || dim(sf$selectedtrains)[1] == 0) {
          shinyalert(
            "Error",
            "Please select training data in the 'Select amount of data to use' section.",
            type = "error"
          )
          return() # Stop execution if no training data
        }

        for (i in 1:amountofts) {
          ts <- createts(
            sf$x_data,
            sf$EDA,
            sf$selectedtrains,
            i,
            sf$test_end_date
          )
          if (is.null(ts)) {
            next # Skip to the next iteration if createts returned NULL
          }
          set <- sf$selectedtrains[i, 1]
          for (tf in 1:amountoftf) {
            transfts <- createtrfts(
              ts = ts,
              trf = sf$transf,
              ntrf = tf
            )
            trf <- sf$transf[tf]
            for (sc in 1:amountofsc) {
              scts <- createscts(
                transfts,
                sf$scales,
                sc
              )
              sca <- sf$scales[sc]
              for (input in 1:amountofinps) {
                steps <- as.numeric(sf$inputamnts[input]) +
                  as.numeric(sf$temporalhorizon)
                if (trf == "Second transformation") {
                  tstv <- ts[, -1, drop = FALSE]
                  date <- ts[[1]]
                  tstv <- log(tstv)
                  tstv <- data.frame(date, tstv, check.names = FALSE)
                } else {
                  tstv <- ts
                }
                vector <- threedvectfunc(
                  tstv[, , drop = FALSE],
                  steps,
                  c(1, dim(tstv)[1])
                )
                starttest <- which(tstv[[1]] == sf$test_start_date)
                truetestvector <- type.convert(
                  vector[
                    (starttest - steps + 1):dim(vector)[1],
                    ,
                    ,
                    drop = FALSE
                  ],
                  as.is = TRUE
                )
                date3dtest <- vector[
                  (starttest - steps + 1):dim(vector)[1],
                  ,
                  1,
                  drop = FALSE
                ]
                vector <- threedvectfunc(
                  scts[, -1, drop = FALSE],
                  steps,
                  c(1, dim(scts)[1])
                )
                starttest <- which(scts[[1]] == sf$test_start_date)
                trainvector <- type.convert(
                  vector[1:(starttest - steps), , , drop = FALSE],
                  as.is = TRUE
                )
                testvector <- type.convert(
                  vector[
                    (starttest - steps + 1):dim(vector)[1],
                    ,
                    ,
                    drop = FALSE
                  ],
                  as.is = TRUE
                )
                inp <- sf$grid |>
                  filter(Inputs == 1) |>
                  pull(Variables)
                out <- sf$grid |>
                  filter(Outputs == 1) |>
                  pull(Variables)
                date3dtest <- date3dtest[,
                  (as.numeric(sf$inputamnts[input]) + 1):dim(testvector)[2],
                  ,
                  drop = FALSE
                ]
                for (m in 1:amountofmodels) {
                  modelbuilding <- modelbuilding + 1
                  loss_file_path <- paste0(loss_directory, "loss.json")
                  if (file.exists(loss_file_path)) {
                    file.remove(loss_file_path)
                  }
                  r$progress[[r$tabname]]$model <- modelbuilding /
                    amountoftotalmodels
                  savemodelpath <- paste0(exp_models, "/model_", modelbuilding)
                  dir.create(savemodelpath)
                  dashdatamodelpath <- paste0(
                    exp_dashdata,
                    "/model_",
                    modelbuilding
                  )
                  dir.create(dashdatamodelpath)
                  struct <- sf$modelstable[m, , drop = FALSE] |>
                    select_if(~ !any(is.na(.)))
                  struct <- struct |>
                    mutate_if(is.factor, as.character)
                  amountofneurons <- struct |>
                    mutate_if(is.character, as.numeric) |>
                    mutate(
                      Neurons = rowSums(across(where(is.numeric)))
                    ) |>
                    select(Neurons)
                  modelfeatutable <- data.frame(
                    Model = modelbuilding,
                    Set = set,
                    Transformation = trf,
                    Scale = sca,
                    Inputs = sf$inputamnts[input],
                    Structure = pastevec(as.character(struct))
                  )
                  htmlmodelfeat <- html_table(modelfeatutable)
                  html(
                    paste0(r$tabname, "htmlmodelfeatT"),
                    htmlmodelfeat
                  )
                  model <- createmodel(
                    structure = struct,
                    inputvec = trainvector[,
                      1:as.numeric(sf$inputamnts[input]),
                      inp,
                      drop = FALSE
                    ],
                    outputvec = trainvector[,
                      (as.numeric(sf$inputamnts[input]) + 1):dim(
                        trainvector
                      )[2],
                      out,
                      drop = FALSE
                    ]
                  )
                  model |> compile(loss = "mse", optimizer = "adam")
                  samples <- dim(
                    trainvector[,
                      1:as.numeric(sf$inputamnts[input]),
                      inp,
                      drop = FALSE
                    ]
                  )[1]
                  cd <- creatingcallback(
                    nmodel = modelbuilding,
                    session = session,
                    directory = loss_directory,
                    plotid = ns(paste0(r$tabname, "liveplot")),
                    batchpbid = ns(paste0(r$tabname, "_batch_update")),
                    batchamount = samples,
                    epochpbid = ns(paste0(r$tabname, "_epoch_update")),
                    epochamount = sf$epoch
                  )
                  model |>
                    fit(
                      trainvector[,
                        1:as.numeric(sf$inputamnts[input]),
                        inp,
                        drop = FALSE
                      ],
                      trainvector[,
                        (as.numeric(sf$inputamnts[input]) + 1):dim(
                          trainvector
                        )[2],
                        out,
                        drop = FALSE
                      ],
                      epochs = sf$epoch,
                      batch_size = 1,
                      shuffle = FALSE,
                      callbacks = list(cd),
                      verbose = 0
                    )
                  last_values_out_for_pred <- truetestvector[,
                    1:as.numeric(sf$inputamnts[input]),
                    out,
                    drop = FALSE
                  ]
                  last_values_out_for_pred[] <- as.numeric(
                    last_values_out_for_pred
                  )

                  predictions <- predictwkeras(
                    model = model,
                    inputs = testvector[,
                      1:as.numeric(sf$inputamnts[input]),
                      inp,
                      drop = FALSE
                    ],
                    outputs = testvector[,
                      (as.numeric(sf$inputamnts[input]) + 1):dim(testvector)[
                        2
                      ],
                      out,
                      drop = FALSE
                    ],
                    lastvaluesout = last_values_out_for_pred,
                    scale = sca,
                    transformation = trf,
                    transf_ts = transfts
                  )
                  model_directorio <- paste0(
                    savemodelpath,
                    "/model",
                    modelbuilding,
                    ".hdf5"
                  )
                  model |> save_model(model_directorio)
                  output_with_date <- abind(
                    date3dtest,
                    predictions,
                    along = 3
                  )
                  output_with_date_x <- as.list(output_with_date)
                  dim(output_with_date_x) <- dim(output_with_date)
                  output_with_date <- type.convert(
                    output_with_date_x,
                    as.is = TRUE
                  )
                  date2d <- unique(as.matrix(output_with_date[, , 1]))
                  mmmpred <- creatingplotpreddf(
                    threddata = output_with_date,
                    xdata = date2d,
                    colnames = out
                  )
                  mmmpred <- toJSON(toJSON(mmmpred))
                  mmmpred <- paste0("var modelpred = ", mmmpred, ";")
                  writeLines(
                    mmmpred,
                    paste0(dashdatamodelpath, "/mmmpred.js"),
                    useBytes = TRUE
                  )
                  trainloss <- fromJSON(paste0(
                    loss_directory,
                    "/loss.json"
                  ))
                  trainloss <- paste0(
                    "var modellosses = [\"",
                    trainloss,
                    "\"];"
                  )
                  write(trainloss, paste0(dashdatamodelpath, "/loss.js"))
                  actual_values_for_metrics <- truetestvector[,
                    (as.numeric(sf$inputamnts[input]) + 1):dim(testvector)[2],
                    out,
                    drop = FALSE
                  ]
                  m_loss <- gettingmetrics(
                    as.numeric(actual_values_for_metrics),
                    as.numeric(predictions)
                  )
                  if (modelbuilding == 1) {
                    modelloss <- data.frame(a = m_loss)
                    colnames(modelloss) <- paste0("model", modelbuilding)
                    modelcharact <- cbind(modelfeatutable, amountofneurons)
                  } else {
                    model_loss <- data.frame(a = m_loss)
                    colnames(model_loss) <- paste0("model", modelbuilding)
                    model_charact <- cbind(modelfeatutable, amountofneurons)
                    modelloss <- cbind(modelloss, model_loss)
                    modelcharact <- rbind(modelcharact, model_charact)
                  }
                  if (modelbuilding == amountoftotalmodels) {
                    outstart <- which(ts[[1]] == date2d[1])
                    outend <- which(ts[[1]] == date2d[length(date2d)])
                    trueoutvalue <- ts[outstart:outend, c(names(ts)[1], out)]
                    rownames(modelloss) <- c("MSE", "RMSE", "MAE")
                    rownames(modelcharact) <- NULL
                    write_json(
                      toJSON(modelloss),
                      paste0(exp_dashdata, "/modelsloss.json")
                    )
                    write_json(
                      toJSON(modelcharact),
                      paste0(exp_dashdata, "/modelcharact.json")
                    )
                    write_json(
                      toJSON(trueoutvalue),
                      paste0(exp_dashdata, "/realvalues.json")
                    )
                    file.copy(
                      "app/static/dashboard/interactivedashb.Rmd",
                      paste0(exp_dashdirect, "/interactivedashb.Rmd")
                    )
                    file.copy(
                      "app/static/dashboard/plotly-latest.min.js",
                      paste0(exp_dashdata, "/plotly-latest.min.js")
                    )
                    render(
                      paste0(exp_dashdirect, "/interactivedashb.Rmd"),
                      envir = new.env()
                    )
                    file.remove(paste0(exp_dashdirect, "/interactivedashb.Rmd"))
                    htmldir <- gsub("app/static/", "", exp_dashdirect)
                    runjs(paste0(
                      "
            document.getElementById('",
                      r$tabname,
                      "content').innerHTML = ",
                      "'<embed type = \"text/html\" src = \"",
                      htmldir,
                      "/interactivedashb.html",
                      " \" style = \"height: 690px; width: 100%\">';
            document.querySelector('#downloadmodels + button').classList.remove('disabled');
            document.querySelector('#downloadmodels + button').removeAttribute('disabled');
            document.querySelector('#dragablepanelcontent').parentElement.style.display = 'block';
                  "
                    ))
                    model_builded <- list.files(paste0(exp_models, "/"))
                    model_builded <- gsub("model_", "", model_builded)
                    if (length(model_builded) == 1) {
                      model_builded <- c("1" = 1)
                    } else {}
                    updatePickerInput(
                      session = session,
                      "downloadmodels",
                      choices = list(
                        "Dashboard" = "Dashboard",
                        "Models" = model_builded
                      )
                    )
                  }
                  clear_session()
                }
              }
            }
          }
        }
      }
    })

    session$onSessionEnded(function() {
      unlink(paste0("app/static/", session$token), recursive = TRUE)
    })
  })
}
