#TITLE:----
##Experimentation with LSTM----
##networks for time series----
##forecasting----
#ABOUT:----
# App to carry out simple experiments of the use of LSTM networks in
# time series forecasting. It allows users to solve univariate and
# multivariate regression problems by testing different set of
# features, and comparing the effectiveness of the obtained results.
#REQUIREMENT:----
# Install the libraries specified in the LIBRARIES section before
# you run the app. This can be done as follow:
# install.packages(c("shiny","shinyalert","shinycssloaders",...))
#RUN APP:----
# Alternative you can run the app by:
# - Clone it from github:
#   shiny::runGitHub(repo = "davidrsch/LSTM-regressionexperiment", ref = "main")
# - Or try it on shinyapps.io:
#   https://daviddrsch.shinyapps.io/experimentwithLSTMnetforTSforecast/
#CREATOR INFO:----
# - Name: David Díaz Rodríguez
# - Email: daviddrsch@gmail.com
# - LinkedIn: https://www.linkedin.com/in/david-d-6257951b8
# - GitHub: https://github.com/davidrsch
# - Stackoverflow: https://stackoverflow.com/users/12660035/
# - ORCid:  https://orcid.org/0000-0002-0927-9795

#LIBRARIES----
library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinyglide)
library(shinyjs)
library(shinyWidgets)
library(abind)
library(callr)
library(dplyr)
library(DT)
library(english)
library(htmlwidgets)
library(jsonlite)
library(keras)
library(Metrics)
library(tensorflow)
library(future)
library(readxl)
library(readr)
library(tools)
library(ggplot2)
library(GGally)
library(pastecs)
library(plotly)
library(rhandsontable)
library(rmarkdown)
library(runner)
library(tseries)
library(scales)
library(zip)

#UI CODE----
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
  ),

  ##App container----
  div(
    ###Title----
    div(
      id = "titlediv",
      titlePanel(
        title = h1(
          "Experimentation with LSTM networks for Time series forecasting",
          style = "text-align: center; border: 1px solid black; padding:10px 0 10px 0"
        ),
        windowTitle = "LSTMexperimentation"
      )
    ),
    ###App Main Panel----
    div(
      mainPanel(
        ####Tabset Panel containing app sections----
        tabsetPanel(
          id = "Main_tabsetpanel",
          #####01-Wellcome Panel----
          tabPanel(
            title = "Wellcome",
            source(
              file = "www/Panels/1_Wellcome/Wellcome.R",
              encoding = 'UTF-8'
            )$value,
            value = "WELLCOME_TABPANEL"
          ),
          #####02-Upload Data Panel----
          tabPanel(
            title = "Upload Data",
            source(file = "www/Panels/2_Uploading_data/UI/UI_UD.R")$value
          ),
          #####03-Selecting Features Panel----
          tabPanel(
            title = "Selecting Features",
            source(file = "www/Panels/3_Selecting_features/UI/UI_SF.R")$value
          ),
          #####04-Results Panel----
          tabPanel(
            title = "Results",
            source(file = "www/Panels/4_Results/UI/UI_R.R")$value
          )
        ),
        ###App Main Panel Style----
        style = "width: 100%; height:100%; background-color: white; padding: 1%"
      ),
      style = "margin: 0 -1.0% 0 -1.0%"
    )
  )
)
