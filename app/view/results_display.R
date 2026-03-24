box::use(
    abind[abind],
    dplyr[across, filter, mutate, pull, select, where],
    english[ordinal],
    jsonlite[toJSON, write_json],
    keras3[clear_session, compile, fit, save_model],
    rmarkdown[render],
    shiny.fluent[CommandBar, Pivot, PivotItem, Stack],
    shiny[div, moduleServer, NS, observeEvent, reactiveVal],
    shiny[reactiveValues, renderUI, req, uiOutput],
    shinyjs[html, runjs],
    shinyWidgets[updatePickerInput],
    utils[type.convert],
)

box::use(
    app / logic / callbacks[create_callback],
    app / logic / models[create_model],
    app / logic / testing[create_plot_pred_df, get_metrics, predict_with_keras],
    app /
        logic /
        time_series[create_scaled_ts, create_transformed_ts, create_ts],
    app / logic / ui_helpers[html_table, paste_vec],
    app / logic / vectors[create_3d_vector],
    app / view / training_progress,
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
server <- function(id, shared_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        r <- reactiveValues()
        r$tab <- NULL
        r$calculation <- 0
        r$progress <- reactiveValues()
        pivot_items <- reactiveVal(list())
        selected_key <- reactiveVal()

        output$pivot_placeholder <- renderUI({
            do.call(
                Pivot,
                c(list(selectedKey = selected_key()), pivot_items())
            )
        })

        observeEvent(shared_data$run_experiment, {
            runjs(
                "document.querySelector('[data-content=\"Results\"]').click();"
            )

            if (is.null(r$tab)) {
                r$tab <- 1
            } else {
                r$tab <- r$tab + 1
            }

            tabname <- paste0(
                " ",
                as.character(r$tab),
                " ",
                ordinal(r$tab),
                " "
            )
            r$tabname <- paste0(as.character(r$tab), ordinal(r$tab))

            new_key <- r$tabname
            r$progress[[new_key]] <- reactiveValues(
                batch = 0,
                epoch = 0,
                model = 0
            )

            training_progress$server(new_key, r$progress[[new_key]], ns)

            new_item_ui <- div(
                id = ns(paste0(r$tabname, "content")),
                div(
                    class = "ms-Grid-row",
                    style = "display: flex; flex-wrap: wrap;",
                    div(class = "ms-Grid-col ms-lg2"),
                    div(
                        class = "ms-Grid-col ms-lg8",
                        training_progress$ui(ns(new_key))
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
        })

        r$path_of_directorio <- paste0("app/static/", session$token)
        dir.create(r$path_of_directorio, showWarnings = FALSE)

        observeEvent(input$rcalculation, {
            if (input$rcalculation == 1) {
                exp_directory <- paste0(r$path_of_directorio, "/", r$tabname)
                exp_models <- paste0(exp_directory, "/models")
                exp_dashdirect <- paste0(exp_directory, "/dashboard")
                exp_dashdata <- paste0(exp_dashdirect, "/www")
                dir.create(exp_directory, showWarnings = FALSE)
                dir.create(exp_models, showWarnings = FALSE)
                dir.create(exp_dashdirect, showWarnings = FALSE)
                dir.create(exp_dashdata, showWarnings = FALSE)
                amountofts <- dim(shared_data$selected_trains)[1]
                amountoftf <- length(shared_data$transf)
                amountofsc <- length(shared_data$scales)
                amountofinps <- length(shared_data$input_amounts)
                amountofmodels <- dim(shared_data$models_table)[1]
                amountoftotalmodels <- amountofts *
                    amountoftf *
                    amountofsc *
                    amountofinps *
                    amountofmodels
                modelbuilding <- 0
                if (shared_data$set_seed == TRUE) {
                    set.seed(shared_data$seed)
                }

                for (i in seq_len(amountofts)) {
                    ts <- create_ts(
                        shared_data$x_data,
                        shared_data$EDA,
                        shared_data$selected_trains,
                        i,
                        shared_data$test_end_date
                    )
                    if (is.null(ts)) {
                        next
                    }
                    set <- shared_data$selected_trains[i, 1]
                    for (tf in seq_len(amountoftf)) {
                        transfts <- create_transformed_ts(
                            ts = ts,
                            trf = shared_data$transf,
                            ntrf = tf
                        )
                        trf <- shared_data$transf[tf]
                        for (sc in seq_len(amountofsc)) {
                            scts <- create_scaled_ts(
                                transfts,
                                shared_data$scales,
                                sc
                            )
                            sca <- shared_data$scales[sc]
                            for (input in seq_len(amountofinps)) {
                                steps <- as.numeric(shared_data$input_amounts[
                                    input
                                ]) +
                                    as.numeric(shared_data$temporalhorizon)
                                if (trf == "second") {
                                    tstv <- ts[, -1, drop = FALSE]
                                    date <- ts[[1]]
                                    tstv <- log(tstv)
                                    tstv <- data.frame(
                                        date,
                                        tstv,
                                        check.names = FALSE
                                    )
                                } else {
                                    tstv <- ts
                                }
                                vector <- create_3d_vector(
                                    tstv[,, drop = FALSE],
                                    steps,
                                    c(1, dim(tstv)[1])
                                )
                                starttest <- which(
                                    tstv[[1]] == shared_data$test_start_date
                                )
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
                                vector <- create_3d_vector(
                                    scts[, -1, drop = FALSE],
                                    steps,
                                    c(1, dim(scts)[1])
                                )
                                starttest <- which(
                                    scts[[1]] == shared_data$test_start_date
                                )
                                trainvector <- type.convert(
                                    vector[
                                        1:(starttest - steps),
                                        ,
                                        ,
                                        drop = FALSE
                                    ],
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
                                inp <- shared_data$grid |>
                                    filter(Inputs == 1) |>
                                    pull(Variables)
                                out <- shared_data$grid |>
                                    filter(Outputs == 1) |>
                                    pull(Variables)
                                date3dtest <- date3dtest[,
                                    (as.numeric(shared_data$input_amounts[
                                        input
                                    ]) +
                                        1):dim(testvector)[2],
                                    ,
                                    drop = FALSE
                                ]
                                for (m in seq_len(amountofmodels)) {
                                    modelbuilding <- modelbuilding + 1
                                    loss_store <- new.env(parent = emptyenv())
                                    loss_store$loss <- NULL
                                    r$progress[[
                                        r$tabname
                                    ]]$model <- modelbuilding /
                                        amountoftotalmodels
                                    savemodelpath <- paste0(
                                        exp_models,
                                        "/model_",
                                        modelbuilding
                                    )
                                    dir.create(
                                        savemodelpath,
                                        showWarnings = FALSE
                                    )
                                    dashdatamodelpath <- paste0(
                                        exp_dashdata,
                                        "/model_",
                                        modelbuilding
                                    )
                                    dir.create(
                                        dashdatamodelpath,
                                        showWarnings = FALSE
                                    )
                                    struct <- shared_data$models_table[
                                        m,
                                        ,
                                        drop = FALSE
                                    ] |>
                                        select(where(~ !any(is.na(.))))
                                    struct <- struct |>
                                        mutate(across(
                                            where(is.factor),
                                            as.character
                                        ))
                                    amountofneurons <- struct |>
                                        mutate(across(
                                            where(is.character),
                                            as.numeric
                                        )) |>
                                        mutate(
                                            Neurons = rowSums(across(where(
                                                is.numeric
                                            )))
                                        ) |>
                                        select(Neurons)
                                    modelfeatutable <- data.frame(
                                        Model = modelbuilding,
                                        Set = set,
                                        Transformation = trf,
                                        Scale = sca,
                                        Inputs = shared_data$input_amounts[
                                            input
                                        ],
                                        Structure = paste_vec(as.character(
                                            struct
                                        ))
                                    )
                                    htmlmodelfeat <- html_table(modelfeatutable)
                                    html(
                                        ns(paste0(
                                            r$tabname,
                                            "-htmlmodelfeatT"
                                        )),
                                        htmlmodelfeat
                                    )
                                    model <- create_model(
                                        structure = struct,
                                        inputvec = trainvector[,
                                            1:as.numeric(shared_data$input_amounts[
                                                input
                                            ]),
                                            inp,
                                            drop = FALSE
                                        ],
                                        outputvec = trainvector[,
                                            (as.numeric(shared_data$input_amounts[
                                                input
                                            ]) +
                                                1):dim(
                                                trainvector
                                            )[2],
                                            out,
                                            drop = FALSE
                                        ]
                                    )
                                    model |>
                                        compile(
                                            loss = "mse",
                                            optimizer = "adam"
                                        )
                                    samples <- dim(
                                        trainvector[,
                                            1:as.numeric(shared_data$input_amounts[
                                                input
                                            ]),
                                            inp,
                                            drop = FALSE
                                        ]
                                    )[1]
                                    cd <- create_callback(
                                        nmodel = modelbuilding,
                                        session = session,
                                        loss_store = loss_store,
                                        plotid = ns(paste0(
                                            r$tabname,
                                            "-liveplot"
                                        )),
                                        batchpbid = ns(paste0(
                                            r$tabname,
                                            "-_batch_update"
                                        )),
                                        batchamount = samples,
                                        epochpbid = ns(paste0(
                                            r$tabname,
                                            "-_epoch_update"
                                        )),
                                        epochamount = shared_data$epoch
                                    )
                                    model |>
                                        fit(
                                            trainvector[,
                                                1:as.numeric(shared_data$input_amounts[
                                                    input
                                                ]),
                                                inp,
                                                drop = FALSE
                                            ],
                                            trainvector[,
                                                (as.numeric(shared_data$input_amounts[
                                                    input
                                                ]) +
                                                    1):dim(
                                                    trainvector
                                                )[2],
                                                out,
                                                drop = FALSE
                                            ],
                                            epochs = shared_data$epoch,
                                            batch_size = 1,
                                            shuffle = FALSE,
                                            callbacks = list(cd),
                                            verbose = 0
                                        )
                                    last_values_out_for_pred <- truetestvector[,
                                        1:as.numeric(shared_data$input_amounts[
                                            input
                                        ]),
                                        out,
                                        drop = FALSE
                                    ]
                                    last_values_out_for_pred[] <- as.numeric(
                                        last_values_out_for_pred
                                    )

                                    predictions <- predict_with_keras(
                                        model = model,
                                        inputs = testvector[,
                                            1:as.numeric(shared_data$input_amounts[
                                                input
                                            ]),
                                            inp,
                                            drop = FALSE
                                        ],
                                        outputs = testvector[,
                                            (as.numeric(shared_data$input_amounts[
                                                input
                                            ]) +
                                                1):dim(testvector)[
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
                                    output_with_date_x <- as.list(
                                        output_with_date
                                    )
                                    dim(output_with_date_x) <- dim(
                                        output_with_date
                                    )
                                    output_with_date <- type.convert(
                                        output_with_date_x,
                                        as.is = TRUE
                                    )
                                    date2d <- unique(as.matrix(output_with_date[,,
                                        1
                                    ]))
                                    mmmpred <- create_plot_pred_df(
                                        threddata = output_with_date,
                                        xdata = date2d,
                                        colnames = out
                                    )
                                    mmmpred <- toJSON(mmmpred)
                                    mmmpred <- paste0(
                                        "var modelpred = ",
                                        mmmpred,
                                        ";"
                                    )
                                    writeLines(
                                        mmmpred,
                                        paste0(
                                            dashdatamodelpath,
                                            "/mmmpred.js"
                                        ),
                                        useBytes = TRUE
                                    )
                                    trainloss <- loss_store$loss
                                    trainloss <- paste0(
                                        "var modellosses = [\"",
                                        trainloss,
                                        "\"];"
                                    )
                                    write(
                                        trainloss,
                                        paste0(dashdatamodelpath, "/loss.js")
                                    )
                                    actual_values_for_metrics <- truetestvector[,
                                        (as.numeric(shared_data$input_amounts[
                                            input
                                        ]) +
                                            1):dim(testvector)[
                                            2
                                        ],
                                        out,
                                        drop = FALSE
                                    ]
                                    m_loss <- get_metrics(
                                        as.numeric(actual_values_for_metrics),
                                        as.numeric(predictions)
                                    )
                                    if (modelbuilding == 1) {
                                        modelloss <- data.frame(a = m_loss)
                                        colnames(modelloss) <- paste0(
                                            "model",
                                            modelbuilding
                                        )
                                        modelcharact <- cbind(
                                            modelfeatutable,
                                            amountofneurons
                                        )
                                    } else {
                                        model_loss <- data.frame(a = m_loss)
                                        colnames(model_loss) <- paste0(
                                            "model",
                                            modelbuilding
                                        )
                                        model_charact <- cbind(
                                            modelfeatutable,
                                            amountofneurons
                                        )
                                        modelloss <- cbind(
                                            modelloss,
                                            model_loss
                                        )
                                        modelcharact <- rbind(
                                            modelcharact,
                                            model_charact
                                        )
                                    }
                                    if (modelbuilding == amountoftotalmodels) {
                                        outstart <- which(ts[[1]] == date2d[1])
                                        outend <- which(
                                            ts[[1]] == date2d[length(date2d)]
                                        )
                                        trueoutvalue <- ts[
                                            outstart:outend,
                                            c(names(ts)[1], out)
                                        ]
                                        rownames(modelloss) <- c(
                                            "MSE",
                                            "RMSE",
                                            "MAE"
                                        )
                                        rownames(modelcharact) <- NULL
                                        write_json(
                                            modelloss,
                                            paste0(
                                                exp_dashdata,
                                                "/modelsloss.json"
                                            )
                                        )
                                        write_json(
                                            modelcharact,
                                            paste0(
                                                exp_dashdata,
                                                "/modelcharact.json"
                                            )
                                        )
                                        write_json(
                                            trueoutvalue,
                                            paste0(
                                                exp_dashdata,
                                                "/realvalues.json"
                                            )
                                        )
                                        file.copy(
                                            "app/static/dashboard/interactivedashb.Rmd",
                                            paste0(
                                                exp_dashdirect,
                                                "/interactivedashb.Rmd"
                                            )
                                        )
                                        file.copy(
                                            "app/static/dashboard/plotly-latest.min.js",
                                            paste0(
                                                exp_dashdata,
                                                "/plotly-latest.min.js"
                                            )
                                        )
                                        render(
                                            paste0(
                                                exp_dashdirect,
                                                "/interactivedashb.Rmd"
                                            ),
                                            envir = new.env()
                                        )
                                        file.remove(paste0(
                                            exp_dashdirect,
                                            "/interactivedashb.Rmd"
                                        ))
                                        htmldir <- gsub(
                                            "app/static/",
                                            "",
                                            exp_dashdirect
                                        )
                                        runjs(paste0(
                                            "
            document.getElementById('",
                                            ns(paste0(r$tabname, "content")),
                                            "').innerHTML = ",
                                            "'<embed type = \"text/html\" src = \"",
                                            htmldir,
                                            "/interactivedashb.html",
                                            " \" style = \"height: 690px; width: 100%\">';
            document.querySelector('#downloadmodels + button').classList.remove('disabled');
            document.querySelector('#downloadmodels + button').removeAttribute('disabled');
            document.querySelector('#dragablepanelcontent').parentElement.style.display = 'block';
                  "
                                        ))
                                        model_builded <- list.files(paste0(
                                            exp_models,
                                            "/"
                                        ))
                                        model_builded <- gsub(
                                            "model_",
                                            "",
                                            model_builded
                                        )
                                        if (length(model_builded) == 1) {
                                            model_builded <- c("1" = 1)
                                        }
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
