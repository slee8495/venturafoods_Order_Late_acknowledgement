library(tidyverse)
library(shiny)
library(readr)
library(writexl)
library(janitor)
library(tidyr)
library(lubridate)
library(DT)
library(rpivotTable)
library(shinyjs)
library(shinyWidgets)

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
                                     tags$p(
                                       HTML(paste("<span style='color:red;'>", data_info(), "</span>",
                                                  tags$br(), 
                                                  "If you wish to explore data for a different date range,",
                                                  "you are welcome to upload your own AS400 data (2269).",
                                                  "The image above provides an example of retrieving original raw data from AS400")),
                                       style = "font-size: 18px; font-weight: bold;"
                                     ),
                                     absolutePanel(
                                       top = "105%",  
                                       left = "55%",  
                                       img(src = "VenturaFoodsLogo.png", height = 70, width = 500)
                                     )
                                   )
                                 )
                        ),
                        tabPanel("View Data",
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(width = 12, 
                                          dataTableOutput("viewData"),
                                          downloadButton("downloadViewData", "Download Data"))
                                 )
                        ),
                        tabPanel("Customer Summary",
                                 br(),
                                 fluidRow(
                                   column(width = 3,
                                          pickerInput("customerSummaryPicker", "Customer:", 
                                                      choices = sort(unique(cleaned_default_data$CustomerName)), 
                                                      selected = cleaned_default_data$CustomerName,
                                                      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
                                                      multiple = TRUE),
                                          dateRangeInput("dateRange", "Order Date:",
                                                         start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                         end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                   ),
                                   column(width = 9,
                                          dataTableOutput("pivot")
                                   )
                                 )
                        ),
                        tabPanel("Summary by Profile name",
                                 br(),
                                 tabsetPanel(
                                   tabPanel("Non Compliance to Order Acknowledgement Table",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     pickerInput("failFilter", "Fail Status:", 
                                                                 choices = sort(unique(cleaned_default_data$Fail)),
                                                                 selected = cleaned_default_data$Fail[1], multiple = TRUE),
                                                     dateRangeInput("dateRange_2", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))),
                                              column(width = 9,
                                                     DT::dataTableOutput("pivot1"))
                                            )
                                   ),
                                   tabPanel("Non Compliance to Order Acknowledgement Plot",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     pickerInput("failFilter_2", "Fail Status:", 
                                                                 choices = sort(unique(cleaned_default_data$Fail)),
                                                                 selected = cleaned_default_data$Fail[1], multiple = TRUE),
                                                     dateRangeInput("dateRange_3", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     plotOutput("barplot")))),
                                   
                                   tabPanel("Fail Status Table",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     dateRangeInput("dateRange_4", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     dataTableOutput("pivot2")))),
                                   
                                   tabPanel("Fail Status Plot",
                                            br(),
                                            fluidRow(
                                              column(width = 3,
                                                     dateRangeInput("dateRange_5", "Order Date:",
                                                                    start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                                    end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                              ),
                                              column(width = 9,
                                                     plotOutput("stackedbarplot")))),
                                 )
                        ),
                        tabPanel("Not Acknowledged",
                                 br(),
                                 fluidRow(
                                   column(width = 3, 
                                          pickerInput("profilename", "Profile Name:",
                                                      choices = sort(unique(cleaned_default_data$`Profile name`)),
                                                      selected = unique(cleaned_default_data$`Profile name`),
                                                      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
                                                      multiple = TRUE),
                                          dateRangeInput("dateRange_6", "Order Date:",
                                                         start = min(cleaned_default_data$OrderDate, na.rm = TRUE), 
                                                         end = max(cleaned_default_data$OrderDate, na.rm = TRUE))
                                   ),
                                   column(width = 9,
                                          dataTableOutput("pivot3"))
                                   
                                 )
                        ),
                        
                        tabPanel("Overall Process Summary",
                                 tabsetPanel(
                                   tabPanel("Fail Distribution - Pie Chart",
                                            tags$br(),
                                            tags$br(),
                                            plotOutput("pieChart")
                                   ),
                                   tabPanel("Stacked Bar Chart",
                                            tags$br(),
                                            tags$br(),
                                            plotOutput("stackedBar")
                                   ),
                                   tabPanel("Line Graph",
                                            tags$br(),
                                            tags$br(),
                                            plotOutput("lineGraph")
                                   ),
                                   tabPanel("Order Date Average Graph",
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
  
  min_date <- reactive({
    min(data_to_display()$OrderDate, na.rm = TRUE)
  })
  
  max_date <- reactive({
    max(data_to_display()$OrderDate, na.rm = TRUE)
  })
  
  # Observe event for file upload
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      uploaded_data <- readr::read_csv(input$file1$datapath)
      
      if (identical(names(uploaded_data), names(raw_data_as400))) {
        cleaned_uploaded_data <- clean_data(uploaded_data)
        data_to_display(cleaned_uploaded_data)
        
        # Update the date range inputs
        updateDateRangeInput(session, "dateRange", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_2", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_3", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_4", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_5", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
        updateDateRangeInput(session, "dateRange_6", 
                             start = min(cleaned_uploaded_data$OrderDate, na.rm = TRUE), 
                             end = max(cleaned_uploaded_data$OrderDate, na.rm = TRUE))
      } else {
        showModal(
          modalDialog(
            title = "Data Structure Error",
            "The structure of the uploaded data does not match the expected structure.",
            footer = NULL
          ))}}
  })
  
  
  
  
  
  customer_names <- reactive({
    unique(data_to_display()$CustomerName)
  })
  
  
  
  
  
  observe({
    updatePickerInput(session, "customerSummaryPicker", choices = sort(customer_names()))
  })
  
  observeEvent(input$file1, {
    
    if (!is.null(input$file1)) {
      
      uploaded_data <- readr::read_csv(input$file1$datapath)
      
      if (identical(names(uploaded_data), names(raw_data_as400))) {
        
        cleaned_uploaded_data <- clean_data(uploaded_data)
        
        data_to_display(cleaned_uploaded_data)
      } else {
        
        showModal(
          modalDialog(
            title = "Data Structure Error",
            "The structure of the uploaded data does not match the expected structure.",
            footer = NULL
          )
        )
        
      }
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
              filter = 'top',
              rownames = FALSE
    )
  })
  
  
  
  output$pivot <- renderDataTable({
    
    selected_dates <- input$dateRange
    
    pivot_data <- data_to_display() %>%
      filter(OrderDate >= selected_dates[1] & OrderDate <= selected_dates[2]) %>%
      filter(CustomerName %in% input$customerSummaryPicker | length(input$customerSummaryPicker) == 0) %>%
      group_by(CustomerName, Fail) %>%
      summarise(Count = n(), .groups = 'drop') %>% 
      pivot_wider(names_from = Fail, values_from = Count, values_fill = list(Count = 0)) 
    
    if (!"Yes" %in% names(pivot_data)) {
      pivot_data$Yes <- 0
    }
    if (!"No" %in% names(pivot_data)) {
      pivot_data$No <- 0
    }
    
    column_totals <- colSums(select(pivot_data, -CustomerName), na.rm = TRUE)
    total_row <- data.frame(CustomerName = "Total", t(column_totals))
    
    pivot_data <- bind_rows(total_row, pivot_data) %>% 
      mutate(yes_no = Yes + No,
             Yes = Yes / yes_no * 1,
             No = No / yes_no * 1) %>% 
      select(-yes_no)
    
    datatable(pivot_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE) %>%
      formatPercentage(columns = c("Yes", "No"), digits = 2) %>%
      formatStyle(
        columns = 1:ncol(pivot_data),
        fontWeight = styleEqual("Total", "bold"),
        backgroundColor = styleEqual("Total", "lightyellow"),
        target = "row"
      )
  })
  
  
  
  output$pivot1 <- renderDataTable({
    
    selected_dates_2 <- input$dateRange_2
    
    pivot1_data <- data_to_display() %>%
      filter(OrderDate >= selected_dates_2[1] & OrderDate <= selected_dates_2[2]) %>%
      filter(Fail %in% input$failFilter) %>%  
      group_by(`Profile name`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    datatable(pivot1_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE)
  })
  
  observeEvent(input$file1, {
    updatePickerInput(session, "failFilter", 
                      choices = unique(data_to_display()$Fail),
                      selected = unique(data_to_display()$Fail[1]))
  })
  
  
  
  
  
  
  
  output$barplot <- renderPlot({
    
    
    
    barplot_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_3[1] & OrderDate <= input$dateRange_3[2]) %>%
      filter(Fail %in% input$failFilter_2) %>%  
      group_by(`Profile name`) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    ggplot(barplot_data, aes(x = reorder(`Profile name`, -Count), y = Count, fill = Count)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "lightblue", high = "blue") +  
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4.5, color = "white", fontface = "bold") + 
      theme_classic() +
      labs(y = "", x = "", title = "Non Compliance to Order Acknowledgement") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "none"
      )
  }, width = 1200, height = 800) 
  
  observeEvent(input$file1, {
    updatePickerInput(session, "failFilter_2", 
                      choices = unique(data_to_display()$Fail),
                      selected = unique(data_to_display()$Fail[1]))
  })
  
  
  
  
  output$pivot2 <- renderDataTable({
    fail_status_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_4[1] & OrderDate <= input$dateRange_4[2]) %>%
      group_by(`Profile name`, Fail) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Fail, values_from = Count, values_fill = list(Count = 0)) %>%
      replace_na(list(Yes = 0, No = 0)) 
    
    totals <- fail_status_data %>%
      summarise(across(-`Profile name`, sum, na.rm = TRUE))
    
    totals$`Profile name` <- "Total"
    
    fail_status_data <- bind_rows(totals, fail_status_data)
    fail_status_data <- fail_status_data %>% 
      relocate(`Profile name`)
    
    datatable(fail_status_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE) %>%
      formatStyle(
        columns = 1:ncol(fail_status_data),
        fontWeight = styleEqual("Total", "bold"),
        backgroundColor = styleEqual("Total", "lightyellow"),
        target = "row"
      )
  })
  
  
  
  
  
  output$stackedbarplot <- renderPlot({
    stacked_bar_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_5[1] & OrderDate <= input$dateRange_5[2]) %>%
      group_by(`Profile name`, Fail) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Fail = factor(Fail, levels = c("Yes", "No"))) 
    
    totals <- stacked_bar_data %>%
      group_by(`Profile name`) %>%
      summarise(Total = sum(Count)) %>%
      arrange(desc(Total))
    
    stacked_bar_data <- stacked_bar_data %>%
      inner_join(totals, by = "Profile name") %>%
      arrange(desc(Total))
    
    # Create ggplot2 stacked bar plot with labels
    ggplot(stacked_bar_data, aes(x = reorder(`Profile name`, -Total), y = Count, fill = Fail)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 4.5, color = "white", fontface = "bold") +
      theme_classic() +
      labs(x = "", y = "", fill = "Fail Status", 
           title = "Profile Name vs Count by Fail Status") +
      scale_fill_manual(values = c("Yes" = "#1f77b4", "No" = "#ff7f0e")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
            axis.text.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 14, face = "bold")) 
  }, width = 1300, height = 800)
  
  
   
  
  
  output$pivot3 <- renderDataTable({
    pivot3_data <- data_to_display() %>%
      filter(OrderDate >= input$dateRange_6[1] & OrderDate <= input$dateRange_6[2]) %>%
      filter(`Profile name` %in% input$profilename) %>%  
      filter(`Order ack` != "E") %>%
      group_by(OrderDate, `Profile name`) %>%
      summarise(Count = n(), .groups = 'drop') %>% 
      mutate(OrderDate = as.character(OrderDate))
    
    totals <- tibble(
      OrderDate = "Total", 
      `Profile name` = "Total",
      Count = sum(pivot3_data$Count, na.rm = TRUE)
    )
    
    # Append the total row at the top
    pivot3_data <- bind_rows(totals, pivot3_data)
    
    
    datatable(pivot3_data, 
              extensions = c("Buttons", "FixedHeader"), 
              options = list(pageLength = 5000,
                             dom = "Blfrtip",
                             buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE,
                             scrollY = "1500px",
                             fixedHeader = TRUE,
                             fixedColumns = list(leftColumns = 2)), 
              rownames = FALSE) %>% 
      formatStyle(
        columns = 1:ncol(pivot3_data),
        fontWeight = styleEqual("Total", "bold"),
        backgroundColor = styleEqual("Total", "lightyellow"),
        target = "row"
      )
  })
  
  
  
  
  
  output$pieChart <- renderPlot({
    pie_data <- data_to_display() %>% 
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
                     legend.text = ggplot2::element_text(size = 12)) 
  })
  
  output$stackedBar <- renderPlot({
    bar_data <- data_to_display() %>%
      dplyr::filter(!is.na(Week)) %>%
      group_by(Week, Fail) %>%
      summarise(Count = n_distinct(CustomerName)) %>%
      ungroup()
    
    
    bar_data$Week <- factor(bar_data$Week, levels = unique(bar_data$Week), ordered = TRUE)
    
    
    total_counts <- bar_data %>% group_by(Week) %>% summarise(Total = sum(Count))
    
    
    bar_data <- merge(bar_data, total_counts, by = "Week")
    
    
    bar_data$Percentage <- (bar_data$Count / bar_data$Total) * 100
    
    
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
      group_by(`OrderDate`, Fail) %>%
      summarise(Count = n_distinct(CustomerName)) %>%
      ungroup()
    
    
    line_data$`OrderDate` <- factor(line_data$`OrderDate`, levels = unique(line_data$`OrderDate`))
    
    p <- ggplot2::ggplot(line_data, ggplot2::aes(x = `OrderDate`, y = Count, color = Fail, group = Fail)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +  
      ggplot2::theme_classic() +
      ggplot2::labs(y = "Number of Customers", x = "OrderDate", color = "Fail Status") +
      ggplot2::ylim(0, max(line_data$Count, na.rm = TRUE) + 1) +  
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm")  
        
      )
    print(p)
  })
  
  output$avgGraph <- renderPlot({
    avg_data <- data_to_display() 
    
    
    if (!is.null(input$Fail)) {
      avg_data <- avg_data %>%
        dplyr::filter(Fail %in% input$Fail)
    }
    
    avg_data <- avg_data %>%
      dplyr::group_by(`OrderDate`) %>%
      dplyr::mutate(`Days to acknowledge` = as.numeric(`Days to acknowledge`)) %>% 
      dplyr::summarise(
        Target = mean(Target, na.rm = TRUE),
        Avg_Days_to_acknowledge = mean(`Days to acknowledge`, na.rm = TRUE)
      ) %>%
      tidyr::gather(key = "Metric", value = "Value", -`OrderDate`) %>% 
      dplyr::mutate(Value = ifelse(Metric == "Target", 2, Value))
    
    
    avg_data$`OrderDate` <- factor(avg_data$`OrderDate`, levels = unique(avg_data$`OrderDate`))
    
    last_date <- tail(levels(avg_data$`OrderDate`), 1) 
    
    p <- ggplot2::ggplot(avg_data, ggplot2::aes(x = `OrderDate`, y = Value, color = Metric, group = Metric)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_text(aes(label=sprintf("%.2f", Value)), vjust = -0.5, size = 5, fontface = "bold") +  
      ggplot2::ylim(0, max(avg_data$Value, na.rm = TRUE) + 1) +  
      ggplot2::theme_classic() +
      ggplot2::labs(y = "Value", x = "OrderDate", color = "Metric") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.x = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.y = ggplot2::element_text(size = 16, face = "bold"),
        legend.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.text = ggplot2::element_text(size = 14, face = "bold"),
        legend.key.size = unit(1.5, "cm") 
        
      )
    print(p)
  })
  
  
  
  
  output$downloadViewData <- downloadHandler(
    filename = function() {
      paste("view_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_to_display(), file, row.names = FALSE)
    })  
  
  
  
  
  
}

shinyApp(ui, server)