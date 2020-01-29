##title: "Siny Assignment"
##author: "Sujeeth Shetty"
##date: "1/25/2020"


if(!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, data.table, shiny)

#Polular Baby Example

#Load data
load_data<- function(sheet){
  data <- read_xlsx("Top100_Popular_Baby_Names.xlsx", sheet = sheet)
  data<-data[rowSums(is.na(data)) != ncol(data), ]
  return (data)
}

#create an array of Year
create_array_year <- function(df){
  year <-df[2,3]
  for (i in 4:ncol(df)){
         if (!is.na(df[2,i])){
           year<- as.numeric(append(year, df[2,i]))
         }
  }
  return (year)
}

#insert year
insert_year <- function(df, year){
  k=0
  for (i in 1:length(year)){
    for (j in (1:100)){
      #girl_child.df[(k+j),1] <- year[i]
      df[(k+j),1] <- year[i]
    }
    k=k+100
  }
  return (df)
}


#insert ranks
insert_ranks <- function(df,year){
  k=0
  for( i in 1:length(year))
  {
    
    for (j in 1:100) 
    {
      df[(k+j),2] <- j
      
    }
    k=k+100
  }
  return (df)
}

#insert name
insert_name <- function(in_data,df){
  i=0
  j=0
  k=0
  repeat
  {
    #if (k == 93) {
      #k<- k+2
    #} else {
      k<- k+3
    #}
    for (i in 1:100) 
    {
      df[(j+i),3] <- in_data[(i+3),k]
      
    }
    j=j+100
    
    if (k >= 195){
      break
    }
    
  }
  return (df)
}

#Insert No. 
insert_num <- function(in_data,df){
  i=0
  j=0
  k=1
  
  repeat{
    #if (k == 94) {
     # k<-k+2
    #} else {
      k<-k+3
    #}
    for (i in 1:100) {
      df[(j+i),4] <- in_data[(i+3),k]
      
    }
    j=j+100
    
    if (k >= 195){
      break
    }
    
  }
  return (df)
}

#-----------Girl Child Names File Transformation------------------------------------#
#Load Girl Child Data
data_gc <- load_data(1)

#Create an array of year
year_g<- create_array_year(data_gc)

#Create new empty Df 
girl_child.df <- data.frame("Year"=integer(),"Rank"=integer(),"Name"=character(),"No"=integer(), stringsAsFactors = FALSE)

girl_child.df<-insert_year(girl_child.df,year_g)
girl_child.df<-insert_ranks(girl_child.df,year_g)
girl_child.df<-insert_name(data_gc,girl_child.df)
girl_child.df<-insert_num(data_gc,girl_child.df)

girl_child.df$Year <- as.factor(girl_child.df$Year)

#Create new data frame for top 10 girl child names 
girl_top10<-setorder(setDT(girl_child.df), -Rank)[, tail(.SD,10), keyby = Year]

#Sort the girl_top10 in descending order
girl_top10 <- transform.data.frame(girl_top10, Year = rev(Year), Rank = rev(Rank), Name = rev(Name), No = rev(No))


#----------Boy Child Names File Transformation-------------------------------------#
#Load Boy Child Data
data_bc<- load_data(2)

#create an array of year
year_b<- create_array_year(data_bc)
year_b<-year_b[!duplicated(year_b)]

#Create new empty Df 
boy_child.df <- data.frame("Year"=integer(),"Rank"=integer(),"Name"=character(),"No"=integer(), stringsAsFactors = FALSE)

boy_child.df<-insert_year(boy_child.df,year_b)
boy_child.df<-insert_ranks(boy_child.df,year_b)
boy_child.df<-insert_name(data_bc,boy_child.df)
boy_child.df<-insert_num(data_bc,boy_child.df)

boy_child.df$Year <- as.factor(boy_child.df$Year)

#Create new data frame for top 10 boy child names 
boy_top10<-setorder(setDT(boy_child.df), -Rank)[, tail(.SD,10), keyby = Year]

#Sort the boy_top10 in descending order
boy_top10 <- transform.data.frame(boy_top10, Year = rev(Year), Rank = rev(Rank), Name = rev(Name), No = rev(No))



##------------Shiny App---------------------##

 ui <- fluidPage(titlePanel("Popularity of Baby Names") ,tabsetPanel(
   type = "tab",
   tabPanel(
     "Top 10 Girl Names",fluid = TRUE,
     sidebarLayout(sidebarPanel(
       #Select Year input from drop down
       selectInput(
         inputId = "GYear",
         label = "Select a Year from dropdown for top 10 Girl Child names",
         choices = girl_top10$Year
         , selected = ''
       )
     ), mainPanel()),
     tableOutput("Table_girl")
   ),
   tabPanel(
     "Top 10 Boy Names",fluid = TRUE,
     sidebarLayout(sidebarPanel(
       #Select Year input from drop down
       selectInput(
         inputId = "BYear",
         label = "Select a Year from dropdown for top 10 Boy Child names",
         choices = girl_top10$Year
         , selected = ''
       )
     ), mainPanel()),
     tableOutput("Table_boy")
   ),
   tabPanel(
     "Trend of Girl Names over time",fluid = TRUE,
     sidebarLayout(sidebarPanel(
       selectInput(
         inputId = "GirlName",
         label = "Select a girl name to view Popularity over time",
         choices = sort(girl_child.df$Name)
         , selected = ''
       )
     ), mainPanel()),
     plotOutput("GirlChart")
   ),
   tabPanel(
     "Trend of Boy Names over time",fluid = TRUE,
     sidebarLayout(sidebarPanel(
       selectInput(
         inputId = "BoyName",
         label = "Select a Boy name to view Popularity over time",
         choices = boy_child.df$Name
         , selected = ''
       )
     ), mainPanel()),
     plotOutput("BoyChart"))
   )
 )

server <- function(input, output) {
  
  output$Table_girl <- renderTable({
    yearfilter1 <- subset(girl_top10, girl_top10$Year == input$GYear)
  })
  
  output$Table_boy <- renderTable({
    yearfilter2 <- subset(boy_top10, boy_top10$Year == input$BYear)
  })
  
  output$GirlChart <- renderPlot({
    name <- subset(girl_child.df, girl_child.df$Name == input$GirlName)
    ggplot(data=name, aes(x=Year, y=Rank, group=1))+
      geom_line()+
      geom_point()
  })
  
  output$BoyChart <- renderPlot({
    name1 <- subset(boy_child.df, boy_child.df$Name == input$BoyName)
    ggplot(data=name1, aes(x=Year, y=Rank, group=1))+
      geom_line()+
      geom_point()
  })
  
  
}
shinyApp(ui = ui, server = server)