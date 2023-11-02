# Load required libraries
library(shiny)
library(readr)
library(writexl)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(DT)
library(rpivotTable)
library(shinyjs)
library(ggplot2)

# Source the data wrangling script to get the cleaned_default_data
source("data.wrangling.R")

# UI Part
ui <- shiny::navbarPage("Order Late Acknowledgement",
                        id = "navbarID",
                        tags$head(
                          tags$style(HTML(".navbar {background-color: #001F3F;}"))
                        ),
                        tabPanel("Upload Data",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file1", "Choose CSV File"),
                                     downloadButton("downloadData", "Download Cleaned .xlsx file")
                                   ),
                                   mainPanel(
                                     img(src = "as400.png", height = 700, width = 1000),
                                     tags$br(),
                                     tags$br(),
                                     tags$br(),
                                     tags$p("If you'd like to view data for a different date range, please feel free to upload your own AS400 data (2269)",
                                            style = "font-size: 18px; font-weight: bold;")
                                   )
                                 )
                        ),
                        tabPanel("View Data",
                                 fluidRow(
                                   column(width = 12, 
                                          dataTableOutput("viewData"))
                                 )
                        ),
                        tabPanel("Customer Summary",
                                 fluidRow(
                                   column(width = 12, 
                                          rpivotTableOutput("pivot")),
                                   div(style = "position: absolute; top: 52px; right:1000px;", 
                                       tags$p("Please ensure to sort by clicking \"⭥\" or \"⭤\" button", 
                                              style = "font-size: 12px; font-weight: bold; color: red;"))
                                 )
                        ),
                        tabPanel("Summary by Profile name",
                                 tabsetPanel(
                                   tabPanel("Non Compliance to Order Acknowledgement Table",
                                            rpivotTableOutput("pivot1"),
                                            div(style = "position: absolute; top: 850px; right: 1250px;", 
                                                tags$p("Please ensure that you select \"Yes\" or \"No\" from the \"Fail\" accordingly", 
                                                       style = "font-size: 11px; font-weight: bold; color: blue;")
                                            )
                                   ),
                                   tabPanel("Non Compliance to Order Acknowledgement Plot",
                                            rpivotTableOutput("barplot"),
                                            div(style = "position: absolute; top: 850px; right: 1250px;", 
                                                tags$p("Please ensure that you select \"Yes\" or \"No\" from the \"Fail\" accordingly", 
                                                       style = "font-size: 11px; font-weight: bold; color: blue;")
                                            )
                                   ),
                                   tabPanel("Fail Status Table",
                                            rpivotTableOutput("pivot2")
                                   ),
                                   tabPanel("Fail Status Plot",
                                            rpivotTableOutput("stackedbarplot")
                                   )
                                 )
                        
                                 
                        ),
                        tabPanel("Not Acknowledged",
                                 fluidRow(
                                   column(width = 12, 
                                          rpivotTableOutput("pivot3")
                                   ),
                                   div(style = "position: absolute; top: 590px; right: 1250px;", 
                                       tags$p("Please ensure that you unselect \"E\" from the \"Order ack\" option in the left panel", 
                                              style = "font-size: 9px; font-weight: bold; color: blue;")
                                   ),

                                 )
                        ),
                        
                        tabPanel("Overall Process Summary",
                                 tabsetPanel(
                                   tabPanel("Fail Distribution - Pie Chart",
                                            selectInput("Week", "Select Week Number:", unique(cleaned_default_data$Week), multiple = TRUE),
                                            plotOutput("pieChart")
                                   ),
                                   tabPanel("Stacked Bar Chart",
                                            plotOutput("stackedBar")
                                   ),
                                   tabPanel("Line Graph",
                                            plotOutput("lineGraph")
                                   ),
                                   tabPanel("Order Date Average Graph",
                                            plotOutput("avgGraph")
                                   )
                                 )
                        )

)

# Server logic
server <- function(input, output, session) {
  data_to_display <- reactiveVal(cleaned_default_data)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cleaned_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(data_to_display(), file)
    }
  )
  
  output$viewData <- renderDataTable({
    datatable(data_to_display(), extensions = 'Buttons', 
              options = list(
                pageLength = 100,
                scrollX = TRUE,
                scrollY = "600px",
                dom = "Blfrtip",
                buttons = c("copy", "csv", "excel"),
                fixedColumns = list(leftColumns = 2),
                searchDelay = 500
              ),
              filter = 'top'
    )
  })
  
  output$pivot <- renderRpivotTable({
    rpivotTable(data = data_to_display(), 
                rows = "CustomerName", 
                cols = "Fail", 
                vals = "CustomerName", 
                aggregatorName = "Count as Fraction of Rows", 
                rendererName = "Table", 
                width="100%", 
                height="400px")
  })
  
  output$pivot1 <- renderRpivotTable({
    rpivotTable(data = data_to_display(),
                rows = "Profile name",
                vals = "CustomerName",
                aggregatorName = "Count",
                rendererName = "Table",
                width="100%",
                height="400px")
  })
  
  output$barplot <- renderRpivotTable({
    rpivotTable(data = data_to_display(),
                cols = "Profile name",
                vals = "CustomerName",
                aggregatorName = "Count",
                rendererName = "Bar Chart",
                width="100%",
                height="400px")
  })
  
  output$pivot2 <- renderRpivotTable({
    rpivotTable(data = data_to_display(),
                rows = "Profile name",
                cols = "Fail",
                vals = "CustomerName",
                aggregatorName = "Count",
                rendererName = "Table",
                width="100%",
                height="400px")
  })
  
  output$stackedbarplot <- renderRpivotTable({
    rpivotTable(data = data_to_display(),
                rows = "Fail",
                cols = "Profile name",
                vals = "CustomerName",
                aggregatorName = "Count",
                rendererName = "Stacked Bar Chart",
                width="100%",
                height="400px")
  })
  
  output$pivot3 <- renderRpivotTable({
    rpivotTable(data = data_to_display(),
                rows = c("Order date", "Profile name"),
                filters = c("Order ack"),  # User will have to manually deselect "E"
                vals = "Enter by name",
                aggregatorName = "Count",
                rendererName = "Table",
                width="100%",
                height="400px")
  })
  
  output$pieChart <- renderPlot({
    pie_data <- data_to_display() %>% 
      dplyr::filter(Week == input$Week | is.null(input$Week)) %>% 
      group_by(Fail) %>% 
      summarise(Count = n_distinct(`CustomerName`))
    
    ggplot2::ggplot(pie_data, ggplot2::aes(x = "", y = Count, fill = Fail)) +
      ggplot2::geom_bar(stat = "identity", width = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::labs(fill = "Fail", title = paste("Fail Distribution")) +
      ggplot2::geom_text(aes(label = scales::percent(Count/sum(Count))), position = position_stack(vjust = 0.5), size = 5, fontface = "bold") + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     plot.title = ggplot2::element_text(size = 20, face = "bold"),
                     legend.text = ggplot2::element_text(size = 12))  # Making legend text bigger
  })
  
  output$stackedBar <- renderPlot({
    bar_data <- data_to_display() %>%
      dplyr::filter(!is.na(Week)) %>%
      group_by(Week, Fail) %>%
      summarise(Count = n_distinct(`CustomerName`))
    
    total_counts <- bar_data %>% group_by(WeekNumber) %>% summarise(Total = sum(Count))
    
    bar_data <- merge(bar_data, total_counts, by = "Week")
    bar_data$Percentage <- bar_data$Count / bar_data$Total
    
    ggplot2::ggplot(bar_data, ggplot2::aes(x = Week, y = Count, fill = Fail)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::geom_text(aes(label = scales::percent(Percentage), y = (cumsum(Count) - (0.5 * Count))), position = position_stack()) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank())
  })
  
  
  output$lineGraph <- renderPlot({
    line_data <- data_to_display() %>%
      dplyr::filter(Fail == "Yes") %>%
      group_by("Order date") %>%
      summarise(Count = n_distinct("CustomerName"))
    
    ggplot2::ggplot(line_data, ggplot2::aes(x = "Order date", y = Count)) +
      ggplot2::geom_line() +
      ggplot2::theme_minimal()
  })
  
  output$avgGraph <- renderPlot({
    avg_data <- data_to_display() %>%
      group_by("Order date", "Week", Fail) %>%
      summarise(Avg_Target = mean(Target, na.rm = TRUE),
                Avg_Days_to_acknowledge = mean("Days to acknowledge", na.rm = TRUE))
    
    ggplot2::ggplot(avg_data, ggplot2::aes(x = "Order date")) +
      ggplot2::geom_line(ggplot2::aes(y = Avg_Target, color = "Average Target")) +
      ggplot2::geom_line(ggplot2::aes(y = Avg_Days_to_acknowledge, color = "Average Days to Acknowledge")) +
      ggplot2::theme_minimal() +
      ggplot2::labs(color = "Metric")
  })
}

shinyApp(ui, server)