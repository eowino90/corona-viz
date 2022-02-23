#Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(DT)
library(plotly)

#' Call the module scripts
source("selectinputFromUsers.R")

#' Define the user interface for the main dashboard
#' This is just shell that will contain the calls to the modules

shinyUI(
  dashboardPage(title = "Corona Virus",skin = "purple",
    dashboardHeader(
      title = "Corona Virus In Italy"
    ),
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Plots",tabName = "plots"),
        menuItem("Raw Dataset",tabName = "raw"),
        selectData_ui("dt"),
        selectDate_ui("date1"),
        selectVars_ui("var"),
        hr(),
        h3("Data Source"),
        HTML("Kaggle:
             Retrieved from: <a href= 'https://www.kaggle.com/sudalairajkumar/covid19-in-italy' target='_blank'>Kaggle</a>")
        
      )
    ),
    dashboardBody(
      tabItems(
        plots_ui("plt"),
        rawdataDisplay_ui("dt")
      )
    )
  )
  

  
)