#'Define select inputs for datasets
#'
# selecting the datasets
selectData_ui<- function(id){
  ns<-NS(id)
  
  tagList(
    selectInput(ns("dt"),"Dataset",
                choices = c("National"="National","Regional"="Regional","Provincial"="Provincial"))
  )
}

#'collecting date ranges from the user
#'
selectDate_ui<- function(id){
  ns<-NS(id)
  dateRangeInput(ns('date1'),"Date range",start="2019-02-28",end = Sys.Date())
}

#'Define the variables to be selected user interface
#'
#'define a list object to host all the variables in all the three datasets
var_choices <- list(
  "Symptoms"="hospitalized_with_symptoms",
  "Intensive care"="intensive_care",
  "Total Hospitalized"="total_hospitalized",
  "Home Isolation"="home_isolation",
  "Total Currently Positive"="total_currently_positive",
  "New Currently Positive"="new_currently_positive",
  "Recovered"="recovered","Deaths"="death",
  "Total Positive Cases"="total_positive_cases",
  "Total Tests"="total_tests"

)



selectVars_ui<- function(id){
  ns<-NS(id)
  
  selectInput(ns("var"),"Plotting Variable",choices = var_choices,selected = var_choices[[5]])
}

#define server part
selectVars_server<- function(input,output,session){
  x<- reactive({input$var})
}

#' define the server part for date inputs
selectDate_server<- function(input,output,session){
  date1<- reactive({input$date1})
}

#'Define the server part for selected user inputs

selectData_server<- function(input,output,session){
  #' return the selected inputs as reactive objects
  #' 
  
  dt<- reactive({input$dt})
  
}


#'processing data sections
#'Define the user interface for raw data
#'
rawdataDisplay_ui<- function(id){
  ns<- NS(id)
  tabItem(tabName = "raw",
          fluidRow(
            box(DTOutput(ns("dt")),width = 12,title = paste("Raw Dataset"),
                       solidHeader = T,status = "primary"
                       )
                   )
          )
}

#' define server that will return the raw dataset
#' 
display_raw_server<- function(input,output,session,x,date1){
  #define reactive to access the user input dt
  dat<- reactive({
      
    if(x()=="National"){
      dat1<- read_csv("national_data.csv")
      
    }
    else if(x()=="Regional"){
      dat1<-read_csv("regional_data.csv")
    }
    else {
      dat1<-  read_csv("provincial_data.csv")
    }
    
    dat1<- dplyr::filter(dat1,date<= date1()[2] & date>= date1()[1])
    
    dat1
  })
  
  
 
}

# Dislay the raw dataset using the below server

rawdataDisplay_server <- function(input,output,session,dat){
  
  output$dt<- renderDT({dat()})
  
}

#' Ploting line graphs for the selected variables against date
#' Define the user interface part of the plot
plots_ui<- function(id){
  ns<- NS(id)
  tabItem(tabName = "plots",
          fluidRow(
            valueBoxOutput(ns('kpi'),width = 3),valueBoxOutput(ns('cs'),width = 3),valueBoxOutput(ns('recov'),width = 3),
            valueBoxOutput(ns('cv'),width = 3)
          ),
          fluidRow(
            box(plotOutput(ns("plt")),width = 6, solidHeader = T,status = "primary"
            ),
            box(plotlyOutput(ns("plt1")),width = 6, solidHeader = T,status = "danger"
            )
          )
  )
} 
 
 
#' Define the server part of plot
plots_server<- function(input,output,session,pltvar,dat){
  kpi<- reactive({
    dt<-dat()
    #sum all the deaths
    deaths<- sum(dt$death,na.rm = T)
    deaths
  })


output$kpi <- renderValueBox({
  valueBox(kpi(),"Total Deaths",icon = icon("skull-crossbones"),color = "red")
})

#case fatality rates
cf<- reactive({
  dt<-dat()
  #sum all the deaths
  csrate<- round((sum(dt$death,na.rm = T)/sum(dt$total_currently_positive,na.rm = T))*100,digits=2)
  csrate
})

output$cs <- renderValueBox({
  valueBox(cf(),"Case fatality Percentage",icon = icon("skull-crossbones"),color = "red")
})
 
#recovered

recov<- reactive({
  dt<-dat()
  #sum all the deaths
  recovered<- sum(dt$recovered,na.rm = T)
  recovered
})


output$recov <- renderValueBox({
  valueBox(recov(),"Total Recovered",icon = icon("viruses"),color = "green")
})

#Recovery rate
cv<- reactive({
  dt<-dat()
  #sum all the deaths
  recrate<- round((sum(dt$recovered,na.rm = T)/sum(dt$total_currently_positive,na.rm = T))*100,digits=2)
  recrate
})

output$cv <- renderValueBox({
  valueBox(cv(),"Recovery Percentage",icon = icon("procedures"))
})



  plots<- reactive({
    
    pltdt <- dplyr::select(dat(),date,x=pltvar()) %>% 
             dplyr::group_by(date) %>% 
             dplyr::summarise(counting=sum(x,na.rm = T))
  

  ggplot(pltdt,aes(x=date,y=counting)) + geom_line()+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  ggtitle(names(var_choices[which(var_choices==pltvar())])) + labs(y= "Counts",x="Dates")
  })
output$plt <- renderPlot({plots()})

#rate of death against date
plot2<- reactive({
  dplyr::select(dat(),date,death,total_positive_cases) %>% 
    dplyr::mutate(rt=round((death/total_positive_cases)*100,digits = 2))->pltdt1
 
  pltdt1 %>% plot_ly(x= ~date,y= ~rt) %>% plotly::add_lines() %>% layout(title="Death Rates",xaxis=list(title="Dates"),yaxis=list(title="Death Percentage"))

})

output$plt1 <- renderPlotly({plot2()})

}
