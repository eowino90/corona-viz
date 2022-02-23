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

shinyServer(
  function(input,output,session){
    
    x= callModule(selectData_server,"dt")
    date1= callModule(selectDate_server,"date1")
    pltvar=callModule(selectVars_server,"var")
    
    dat=callModule(display_raw_server,"myid",x=x,date1=date1)
    
    callModule(rawdataDisplay_server,"dt",dat=dat)
    callModule(plots_server,"plt",pltvar=pltvar,dat=dat)
    
  }
)