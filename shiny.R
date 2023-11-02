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
                                            selectInput("Week", "Select Week Number:", unique(cleaned_default_data$Week), multiple = TRUE, selected = unique(cleaned_default_data$Week)),
                                            plotOutput("pieChart")
                                   ),
                                   tabPanel("Stacked Bar Chart",
                                            plotOutput("stackedBar")
                                   ),
                                   tabPanel("Line Graph",
                                            plotOutput("lineGraph")
                                   ),
                                   tabPanel("Order Date Average Graph",
                                            selectInput("Week", "Select Week Number:", 
                                                        choices = unique(cleaned_default_data$Week), 
                                                        multiple = TRUE, 
                                                        selected = unique(cleaned_default_data$Week)),
                                            selectInput("Fail", "Select Fail Status:", 
                                                        choices = unique(cleaned_default_data$Fail),
                                                        selected = unique(cleaned_default_data$Fail),
                                                        multiple = TRUE),
                                            plotOutput("avgGraph")
                                            
                                   )
                                 )
                        )
                        
                        
)

# Server logic
server <- function(input, output, session) {
  
  data_to_display <- reactiveVal(cleaned_default_data)
  
  observeEvent(input$file1, {
    # Check if a file is uploaded
    if (!is.null(input$file1)) {
      # Read uploaded file
      uploaded_data <- readr::read_csv(input$file1$datapath)
      # Clean the uploaded data using the `clean_data` function
      cleaned_uploaded_data <- clean_data(uploaded_data)
      # Update the reactive value
      data_to_display(cleaned_uploaded_data)
    }
  })
  
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
      summarise(Count = n_distinct(CustomerName)) %>%
      ungroup() # Ungroup the data for further operations
    
    # Convert Week to a factor
    bar_data$Week <- factor(bar_data$Week, levels = unique(bar_data$Week), ordered = TRUE)
    
    # Calculate total counts for each week
    total_counts <- bar_data %>% group_by(Week) %>% summarise(Total = sum(Count))
    
    # Merge total counts to bar_data
    bar_data <- merge(bar_data, total_counts, by = "Week")
    
    # Calculate percentages
    bar_data$Percentage <- (bar_data$Count / bar_data$Total) * 100
    
    # Calculate cumulative percentage
    bar_data <- bar_data %>%
      arrange(Week, desc(Fail)) %>%
      group_by(Week) %>%
      mutate(CumulativePercentage = cumsum(Percentage) - 0.5 * Percentage)
    
    ggplot2::ggplot(bar_data, ggplot2::aes(x = Week, y = Percentage, fill = Fail)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::geom_text(aes(label = sprintf("%.1f%%", Percentage), y = CumulativePercentage), 
                         size = 6, face = "bold", color = "black") +
      ggplot2::labs(y = "Percentage (%)", x = "Week", fill = "Fail") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(face = "bold", size = 12),
        axis.text.y = ggplot2::element_text(face = "bold", size = 12),
        axis.title.x = ggplot2::element_text(face = "bold", size = 14),
        axis.title.y = ggplot2::element_text(face = "bold", size = 14)
      )
  })
  
  
  
  output$lineGraph <- renderPlot({
    line_data <- data_to_display() %>%
      group_by(`Order date`, Fail) %>%
      summarise(Count = n_distinct(CustomerName)) %>%
      ungroup()
    
    # Convert Order date to factor
    line_data$`Order date` <- factor(line_data$`Order date`, levels = unique(line_data$`Order date`))
    
    p <- ggplot2::ggplot(line_data, ggplot2::aes(x = `Order date`, y = Count, color = Fail, group = Fail)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +  # making the text label larger and bold
      ggplot2::theme_classic() +
      ggplot2::labs(y = "Number of Customers", x = "Order Date", color = "Fail Status") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm")  # Increase legend key size
      )
    print(p)
  })
  
  output$avgGraph <- renderPlot({
    avg_data <- data_to_display() %>%
      dplyr::filter(Week %in% input$Week, Fail %in% input$Fail) %>%
      dplyr::group_by(`Order date`) %>%
      dplyr::summarise(
        Avg_Target = mean(Target, na.rm = TRUE),
        Avg_Days_to_acknowledge = mean(`Days to acknowledge`, na.rm = TRUE)
      ) %>%
      tidyr::gather(key = "Metric", value = "Value", Avg_Days_to_acknowledge)
    
    # Convert Order date to factor
    avg_data$`Order date` <- factor(avg_data$`Order date`, levels = unique(avg_data$`Order date`))
    
    last_date <- tail(levels(avg_data$`Order date`), 1)  # Get the last level of the factor
    
    p <- ggplot2::ggplot(avg_data, ggplot2::aes(x = `Order date`, y = Value, color = Metric, group = Metric)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_hline(yintercept = 2, linetype = "dashed", color = "blue", size = 1) +  # Adding a target line at y=2
      ggplot2::geom_text(aes(label=sprintf("%.2f", Value)), vjust = -0.5, size = 5, fontface = "bold") +  # Adding text labels for the plotted values, made larger and bold
      ggplot2::annotate("text", x = last_date, y = 2.2, label = "Target", hjust = -1.9, color = "black", size = 6, fontface = "bold") +  # Making "Target" label larger and bold
      ggplot2::ylim(0, max(avg_data$Value, na.rm = TRUE) + 1) +  # Setting Y-axis to start from 0
      ggplot2::theme_classic() +
      ggplot2::labs(y = "Value", x = "Order Date", color = "Metric") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm")  # Increase legend key size
      )
    print(p)
  })
  
  
  
}

shinyApp(ui, server)