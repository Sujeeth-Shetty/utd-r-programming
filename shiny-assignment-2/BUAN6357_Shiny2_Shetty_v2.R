
##title: "Siny Assignment 2"
##author: "Sujeeth Shetty"
##date: "3/21/2020"


if(!require("pacman")) install.packages("pacman")
pacman::p_load( plotly,radiant.data,tidyverse, data.table, shiny,leaflet,leaflet.extras,dplyr,plyr, htmlwidgets,shinydashboard,geonames,countrycode,htmltools)

#Load data
load_data<- function(){
  #load ebola file
  df <- read.csv("ebola_2014_2016.csv")
  df<-df[rowSums(is.na(df)) != ncol(df), ]
  df["code"]<-countrycode(df[,1],origin ='country.name',destination ='genc2c')
  
  #Load Countries file with average logitude and lattitude
  #countries<- read.csv("average-latitude-longitude-countries.csv")
  countries <- fread("https://raw.githubusercontent.com/Sujeeth-Shetty/utd-r-programming/master/shiny-assignment-2/average-latitude-longitude-countries.csv")
  names(countries)[1] <- "code"
  
  #merge both files
  data<-merge(df,countries,by.x = "code", by.y = "code") #,by.x = "code", by.y = "code", all.x = TRUE, all.y = FALSE
  names(data)[1:11] <-c("country_cd","country","date","suspected_cases","probable_cases","confirmed_cases","total_cases","suspected_deaths","probable_deaths","confirmed_deaths","total_deaths")
  data$Country.y <- NULL
  data$date <- as.Date(data$date, format="%m/%d/%Y")
  data[is.na(data)] = 0
  data<-ddply(data,c("country_cd","country","date","Latitude","Longitude"),numcolwise(sum))
  #data$recovered<-(data$total_cases - data$total_deaths)
  
  return (data)
}

data_atDate <- function(inputDate) {
  ebola.df[which(ebola.df$date <= inputDate),] %>%
    group_by(country_cd,country,date) %>%
    filter(suspected_cases > 0 |
             probable_cases > 0 |
             confirmed_cases > 0 |
             suspected_deaths > 0 |
             probable_deaths > 0 |
             confirmed_deaths > 0 
    )
}

summariseData <- function(df, groupBy){
  df%>%
    dplyr::group_by(!!sym(groupBy)) %>%
    dplyr::summarise(
      "Cases" = sum(total_cases),
      "Deaths" = sum(total_deaths),
      "Recovered" = (sum(total_cases)-sum(total_deaths))
    )%>%
    as.data.frame()
  
}

additionalData <- function(df, groupBy){
  df%>%
    dplyr::group_by(!!sym(groupBy)) %>%
    dplyr::summarise(
      "Suspected Cases" = sum(suspected_cases),
      "Probable Cases" = sum(probable_cases),
      "Confirmed Cases" = sum(confirmed_cases),
      "Suspected Deaths" = sum(suspected_deaths),
      "Probable Deaths" = sum(probable_deaths),
      "Confirmed Deaths" = sum(confirmed_deaths),
    )%>% as.data.frame()
}

totalCases<- function(df){
  tcases<-sum(df$total_cases)
  return(tcases)
}

totalDeaths<- function(df){
  tdeaths<-sum(df$total_deaths)
  return(tdeaths)
}


totalRecovered<- function(df){
  trec<-(sum(df$total_cases) -sum(df$total_deaths))
  return(trec)
}


addLabel <- function(data) {
  data$label <- paste0(
    '<b>', ifelse(is.na(data$`country`), data$country,data$country), '</b><br>
    <table style="width:120px;">
    <tr><td>Cases:</td><td align="right">', data$total_cases, '</td></tr>
    <tr><td>Deaths:</td><td align="right">', data$total_deaths, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)
  
  return(data)
}

capFirst <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

ebola.df<-load_data()


#-------------------UI------------------------------------#
ui<- dashboardPage( 
  dashboardHeader(title = "Western African Ebola Virus epidemic"),
  dashboardSidebar(
    sliderInput("timeSlider", "Select Date",
                min = min(ebola.df$date), max = max(ebola.df$date), value = max(ebola.df$date), animate = animationOptions(loop = TRUE)
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Plots", tabName = "plots")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview",
              fluidRow(
                valueBoxOutput("cases"),
                valueBoxOutput("recovered"),
                valueBoxOutput("deceased")
              ),
              fluidRow(
                box(
                  width = 7, status = "info", solidHeader = TRUE,
                  title = "Overview Map",
                  leafletOutput("overview_map")
                ),
                box(
                  width = 5,
                  #title = "Summary",
                  style = 'padding:0px;',
                  #tableOutput("summaryTable")
                  uiOutput("summaryTable")
                )
              )
      ),
      tabItem("plots",
              fluidRow(
                box( width=6, solidHeader = TRUE,
                     title="Evolution of Cases since outbreak",
                     plotlyOutput("case_evolution_by_date")
                ),
                box(width=6, solidHeader = TRUE,
                    title="Evolution of Cases since outbreak - Breakout",
                    plotlyOutput("case_evolution_date_breakout")
                )
              ),
              fluidRow(
                box(width=6, solidHeader = TRUE,
                    title="Cases by Country(log)",
                    plotlyOutput("case_evolution_by_country")
                ),
                box(width=6, solidHeader = TRUE,
                    title="Deaths by Country(log)",
                    plotlyOutput("case_evolution_by_country_breakout")
                )
              )
      )
  
    )
  )
)

#----------------------Server---------------#
server <- function(input,output){
  #data<-data_atDate(input$timeSlider)

  value_confirmed <- reactive({
    data <- data_atDate(input$timeSlider)
    keyFigures <- list(
      "tCases" = format(sum(data$total_cases)),
      "tRec" = format((sum(data$total_cases)-sum(data$total_deaths))),
      "tDeaths"  = format(sum(data$total_deaths))
      
    )
    return(keyFigures)
  })
  
  
  output$cases <- renderValueBox({
    valueBox(
      value = value_confirmed()$tCases,
      subtitle = "Total Cases",
      icon = icon("file-medical"),
      color    = "light-blue"
    )
  })
  
  output$recovered <- renderValueBox({
    valueBox(
      value = value_confirmed()$tRec,
      subtitle = "Total Recovered",
      icon = icon("heart"),
      color    = "light-blue"
    )
  })
  
  output$deceased <- renderValueBox({
    valueBox(
      value = value_confirmed()$tDeaths,
      subtitle = "Total Deaths",
      icon = icon("heartbeat"),
      color    = "light-blue"
    )
  })
  

  output$summaryDT_country <- renderDataTable(summariseData(data_atDate(input$timeSlider), "country"))
  output$summaryDT_country_add <- renderDataTable(additionalData(data_atDate(input$timeSlider), "country"))
  
  output$summaryTable <- renderUI({
    tabBox(
      tabPanel("Summary", dataTableOutput("summaryDT_country")),
      tabPanel("Additional Data", div(style = 'overflow-y:scroll;',dataTableOutput("summaryDT_country_add"))),
      width = 12
    )
  })
  
  
  observe({
    req(input$timeSlider, input$overview_map_zoom)
    zoomLevel               <- input$overview_map_zoom
    data                    <- data_atDate(input$timeSlider) %>% addLabel()
    #data <- ebola.df
    
    leafletProxy("overview_map", data = data) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng          = ~Longitude,
        lat          = ~Latitude,
        radius       = ~log(total_cases^(zoomLevel / 2)),
        stroke       = FALSE,
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Confirmed"
      ) %>%
      addCircleMarkers(
        lng          = ~Longitude,
        lat          = ~Latitude,
        radius       = ~log(total_deaths^(zoomLevel / 2)),
        stroke       = FALSE,
        color        = "#f49e19",
        fillOpacity  = 0.5,
        label        = ~label,
        labelOptions = labelOptions(textsize = 15),
        group        = "Active"
      ) 
  })
  
  output$overview_map <- renderLeaflet(leaflet(addLabel(data_atDate(input$timeSlider))) %>%
                                         setMaxBounds(-180, -90, 180, 90) %>%
                                         setView(0, 0, zoom = 4) %>%
                                         addTiles() %>%
                                         addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
                                         addProviderTiles(providers$HERE.satelliteDay, group = "Satellite") %>%
                                         addLayersControl(
                                           baseGroups    = c("Light", "Satellite"),
                                           overlayGroups = c("Cases", "Deaths"),
                                           options       = layersControlOptions(collapsed = FALSE)
                                         ) %>%
                                         hideGroup("Deaths") )
  
  #case_evolution_by_date, case_evolution_date_breakout, case_evolution_by_country, case_evolution_by_country_breakout
  
  output$case_evolution_by_date <- renderPlotly({
    data<-summariseData(ebola.df,"date")%>% gather(var, value, Cases:Recovered)
    p <- plot_ly(
      data,
      x     = ~date,
      y     = ~value,
      name  = sapply(data$var, capFirst),
      color = ~var,
      type  = 'scatter',
      mode  = 'lines') %>%
      layout(
        yaxis = list(title = "Count"),
        xaxis = list(title = "Date")
      )
    
    return(p)
    
  })
  
  output$case_evolution_date_breakout <- renderPlotly({
    data<-additionalData(ebola.df,"date")%>% gather(var, value, 2:7)
    p <- plot_ly(
      data,
      x     = ~date,
      y     = ~value,
      name  = sapply(data$var, capFirst),
      color = ~var,
      type  = 'scatter',
      mode  = 'lines') %>%
      layout(
        yaxis = list(title = "Count"),
        xaxis = list(title = "Date")
      )
    
    return(p)
    
  })
  
  output$case_evolution_by_country <- renderPlotly({
    #data<-ebola.df%>%select(country,date,total_cases)%>%group_by(country) %>%gather(var, value, total_cases)
    data<-summariseData(ebola.df,"country")%>%select(country,Cases)
    p <- plot_ly(data = data, x = ~country, y = ~log(Cases), color = ~country, type = 'bar',text = ~paste("Country: ", country, '<br>Total Cases:', Cases)) %>%
      layout(
        yaxis = list(title = "# Cases"),
        xaxis = list(title = "Country")
      )
    
    return(p)
    
  })
  
  output$case_evolution_by_country_breakout <- renderPlotly({
    #data<-ebola.df%>%select(country,date,total_cases)%>%group_by(country) %>%gather(var, value, total_cases)
    data<-summariseData(ebola.df,"country")%>%select(country,Deaths)
    p <- plot_ly(data = data, x = ~country, y = ~log(Deaths), color = ~country, type = 'bar',text = ~paste("Country: ", country, '<br>Total Deaths:', Deaths)) %>%
      layout(
        yaxis = list(title = "# Cases"),
        xaxis = list(title = "Country")
      )
    
    return(p)
    
  })
 
}

shinyApp(ui = ui, server = server)


