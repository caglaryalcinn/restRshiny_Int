library(shiny)
library(tidyverse)
library(MASS)
library(ggplot2)
library(dplyr)
library(DT)
library(shiny)
library(plotly)
library(leaflet)
library(shinythemes)
library("janitor")
library("readxl")
library(shinyBS)
anket <- read_excel("stat365.data.xlsx")
anket1 <- clean_names(iris)
catag <- anket1 %>% select_if(is.factor)
nume <- anket1 %>% select_if(is.numeric)
nume <- na.omit(nume)
js <- c(
  "var tbl = $(table.table().node());",
  "var id = tbl.closest('.datatables').attr('id');",
  "table.on('autoFill', function(e, datatable, cells){",
  "  var out = [];",
  "  for(var i = 0; i < cells.length; ++i){",
  "    var cells_i = cells[i];",
  "    for(var j = 0; j < cells_i.length; ++j){",
  "      var c = cells_i[j];",
  "      var value = c.set === null ? '' : c.set;", # null => problem in R
  "      out.push({",
  "        row: c.index.row + 1,",
  "        col: c.index.column,",
  "        value: value",
  "      });",
  # to color the autofilled cells, uncomment the two lines below  
  #  "      $(table.cell(c.index.row, c.index.column).node())",
  #  "        .css('background-color', 'yellow');",
  "    }",
  "  }",
  "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
  "  table.rows().invalidate();", # this updates the column type
  "});",
  "$.contextMenu({",
  "  selector: '#table th',", 
  "  trigger: 'right',",
  "  autoHide: true,",
  "  items: {",
  "    text: {",
  "      name: 'Enter column header:',", 
  "      type: 'text',", 
  "      value: ''", 
  "    }",
  "  },",
  "  events: {",
  "    show: function(opts){",
  "      $.contextMenu.setInputValues(opts, {text: opts.$trigger.text()});",
  "    },",
  "    hide: function(opts){",
  "      var $this = this;",
  "      var data = $.contextMenu.getInputValues(opts, $this.data());",
  "      var $th = opts.$trigger;",
  "      $th.text(data.text);",
  "    }",
  "  }",
  "});" 

)





### Module
##modFunction <- function(input, output, session, data,reset) {
  
  ##v <- reactiveValues(data = data)
  
  ##proxy = dataTableProxy("mod_table")
  
  ##observeEvent(input$teamsTable_cell_edit, {
    ##info = input$teamsTable_cell_edit
    ##i = info$row
    ##j = info$col + 1L  # column index offset by 1 b/c TeamID column hidden
    ##v = info$value
    ##rv[["teams"]][i, j] = coerceValue(v, rv[["teams"]][i, j])
    ##replaceData(proxyTeams, rv[["teams"]], resetPaging = FALSE, rownames = FALSE) 
  ##})
  
  ### Reset Table
##  observeEvent(reset(), {
  ##  v$data <- data # your default data
  ##})
  
  ##print(isolate(colnames(v$data)))
  ##output$mod_table <- DT::renderDataTable({
    ##DT::datatable(v$data, editable = TRUE)
    
  ##})
  
  ##return(v)
##}









ui <- fluidPage(
  
  navbarPage(
    "Caglar Yalcin",
    id="main_navbar",
    
    #data 
    tabPanel(
      "Data",
      tags$head(
        tags$link(
          rel = "stylesheet", 
          href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
        ),
        tags$script(
          src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
        )
      ),
      "Data",
      sidebarPanel(
        #title and names
        h3("LEMONBALM", align="center"),
        h4("CREATED BY CAGLAR YALCIN",align="center"),
        varSelectInput("variable", "Variable:", anket1),
        verbatimTextOutput("count"),
        varSelectInput("column", "column seciniz:", anket1),
        actionButton('factor', 'do factor'),
        actionButton('numeric', 'do numeric'),
        verbatimTextOutput("str")
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Data",uiOutput("modals"),DTOutput("table"),verbatimTextOutput("summary")),
                    tabPanel("Univariate",varSelectInput("column_c", "categorik column seciniz:", catag),plotOutput("plot2"),varSelectInput("column_n", "numeric column seciniz:", nume),plotOutput("plot"),plotOutput("plot3")),
                    tabPanel("Multivariate",plotOutput("plot4"),varSelectInput("column_m", "categorik column seciniz:", catag),varSelectInput("column_m2", "categorik column seciniz:", catag),
                             plotOutput("plot5"),varSelectInput("column_s", "numeric column seciniz:", nume),varSelectInput("column_s2", "numeric column seciniz:", nume),plotOutput("plot6")),
                    
      ),
    )),
    
    
))
server <- function(input, output, session){
  
  
  

  
  
  
  x<-reactiveValues(a=anket1)
  
  
  
  
  
  
  output[["table"]] <- renderDT({
    ##forplot
    ##sketch <- tags$table(
      ##class = "row-border stripe hover compact",
      ##tableHeader(c("", names(anket1))),
      ##tableFooter(c("", buttons))
    ##)
    ##
    datatable(
      anket1,
      ##container = sketch, 
      selection = "none",
      editable = list(target = "cell"), 
      callback = JS(js),
      ##extensions = "KeyTable",  ## OPTİONAL
      extensions = "AutoFill",
      options = list(
        autoFill = TRUE
      )
    )
  }, server = FALSE)
  
  Data <- reactive({
    info <- rbind(input[["table_cells_filled"]], input[["table_cell_edit"]])
    if(!is.null(info)){
      info <- unique(info)
      info$value[info$value==""] <- NA
      anket1 <<- editData(anket1, info, proxy = "dt")
    } 
    observeEvent(input$factor, {
      
      
      x$a<-x$a %>%
        mutate(!!input$column := as.factor(!!input$column))
    })
    
    observeEvent(input$numeric, {
      
      x$a<-x$a %>%
        mutate(!!input$column := as.numeric(!!input$column))
    })
    
   
    return(anket1)
  })
  
 
  
  
  
  
  b<-reactive({
    c<-data.frame(count(Data(),!!input$variable))
    return(c)
  })
  
  output[["count"]]  <- renderPrint({b()})
  
  
  
  
  output$str <- renderPrint({
    print(str(x$a))
          })
  
  a<-reactive({
    d<-summary(Data())
    return(d)
  })
  output$summary <- renderPrint({a()})
  
  
  

  output$plot<- renderPlot({
    ggp3 <- ggplot(anket1, aes(x = !!input$column_n)) +    # Draw histogram & density
      geom_density(col = "#1b98e0", size = 2) 
    
    ggp3
  })
  output$plot3<- renderPlot({
    ggplot(anket1, aes(x=!!input$column_n)) + 
      geom_boxplot(
        
        color="blue",
        fill="blue",
        alpha=0.2,
        
        notch=TRUE,
        notchwidth = 0.8,
        
        outlier.colour="red",
        outlier.fill="red",
        outlier.size=3
      )
  })
  output$plot2<- renderPlot({
    ggplot(anket1,aes(x=!!input$column_c , fill = !!input$column_c))+geom_bar()+
      geom_text(stat="count",aes(label =..count..),vjust=-0.25,fontface="bold")
  })

  output$plot4<- renderPlot({
    
    res <- cor(nume, method="pearson")
    corrplot::corrplot(res, method= "color", order = "hclust")
      
  })
  
  output$plot5<- renderPlot({
    
    mosaicplot( ~ !!input$column_m+!!input$column_m2,data=anket1,color = TRUE)
    
  })
  
  output$plot6<- renderPlot({
    
    ggplot(anket1,aes(x=!!input$column_s,y=!!input$column_s2))+geom_point(col="darkred",position="jitter")+geom_smooth()
    
  })
  
  
 
}

shinyApp(ui, server)
