l

ibrary(markdown)
library("readxl")
getwd()
setwd("C:/Users/user/Desktop")
kar <- read_excel("stat365.data.xlsx")
colnames(kar) <- c("symboling","normalized_losses","brand","fuel_type",
                   "aspiration","num_of_doors",
                   "body_style","drive_wheels","engine_location",
                   "wheel_base","length","width","height","curb_weight",
                   "engine_type","num_of_cylinders","engine_size",
                   "fuel_system","bore","stroke","compression_ratio",
                   "horsepower","peak_rpm","city_mpg","highway_mpg","price")


kar <- read_excel("stat365.data.xlsx")
kar[kar == "?"] <- NA
kar$price <- as.numeric(as.character(kar$price))
kar$horsepower <-as.numeric(as.character(kar$horsepower))
kar <- na.omit(kar)



library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(DT)
library(shiny)
library(plotly)
library(leaflet)
library(shinythemes)


ui<-navbarPage("kars",shinythemes::themeSelector(),
               tabPanel("data",
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput(inputId = "brand", 
                                               label = "Choose brand:",
                                               choices = unique(kar$brand),
                                               selected = "mazda"),
                            sliderInput("price",
                                        "Choose price",
                                        value = 7000,
                                        min = min(kar$price),
                                        max = max(kar$price)),
                            
                            selectInput(
                              "X",
                              label = "Select variables:",
                              choices = c("gpa")
                              
                            )
                            
                            ,
                            
                            
                            selectInput("Y", label = "Select variable to predict:",
                                        choices = c("freqofmusic")),
                            
                            
                            
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Data",dataTableOutput("data"),
                                       verbatimTextOutput("text")),
                              tabPanel("Summary",verbatimTextOutput("summary")
                                       
                              ),
                              tabPanel("graph",plotOutput("plo"),plotOutput("plo2"),
                                       plotlyOutput("graph"))
                              
                            )
                            
                          )
                        )),
               tabPanel("normalty check",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              "norm",
                              label = "Select variables:",
                              choices = c("price","city_mpg","highway_mpg","horsepower","engine_size")
                              
                            ),
                            
                            
                            
                            
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel("shapiro",verbatimTextOutput("shapiro")),
                              tabPanel("graph",plotOutput("normal"),
                                       verbatimTextOutput("text2"))
                              
                              
                            )
                            
                          )
                          
                        )
               ))
server<-function(input,output){
  
  norm<-reactive({kar[,input$norm]})
  a<-reactive({kar[,input$X]})
  b<-reactive({kar[,input$Y]})
  data <-reactive({kar %>% filter( 
    price >= input$price,
    brand == input$brand
  )})
  
  
  
  
  output$data <- renderDataTable({
    tail(kar)
  })
  output$summary <- renderPrint({print(summary(data()))
    
  })
  output$plo<-renderPlot({
    
    ggplot(kar, aes(input$X, input$Y, color = department))+
      geom_point(size = 4, alpha = 0.7) 
      
  })
  
  
  output$plo2<-renderPlot({
    ggplot(kar, aes(a(), b(), fill = body_style))+
      geom_col(position = "dodge")
     
      
      facet_wrap(~brand, ncol =5)
  })
  
  
  output$graph<-renderPlotly({
    fig<-plot_ly(kar, x = a(), y = b(),
                 text = ~brand, type = 'scatter', mode = 'markers', 
                 size = ~price, color = ~brand, colors = 'Paired',
                 marker = list(opacity = 0.5, sizemode = 'diameter'))
    fig<-fig %>% layout(title = 'KARS',
                        xaxis = list(showgrid = FALSE),
                        yaxis = list(showgrid = FALSE),
                        showlegend = FALSE)
  })
  
  
  output$shapiro <- renderText({
    
    b <- shapiro.test(norm())
    if (b$p.value<0.01){
      paste("The p value is",b$p.value,"and it is less than 0.01.So,it is not normal according to shapiro wilk test")} else {
        paste("The p value is",b$p.value,"and it is greater than 0.01.So,it is normal according to shapiro wilk test")}
  })
  
  output$normal <- renderPlot({ggplot(kar, aes(norm())) +
      geom_density(col = "Dark Blue") +
      geom_vline(aes(xintercept = mean(norm())), linetype = 2)
  })
  
  output$text2 <- renderPrint({print("Normal")})
  
  
}
shinyApp(ui,server)

