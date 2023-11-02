# Load required libraries
library(shiny)
library(readr)
library(writexl)
library(dplyr)
library(janitor)
library(tidyr)
library(lubridate)
library(DT)

# Function to convert Excel numeric date to R date
excel_numeric_to_date <- function(numeric_date) {
  as.Date(numeric_date, origin = "1899-12-31")
}

# UI Part
ui <- shiny::navbarPage("Order Late Acknowledgement",
                        tags$head(
                          tags$style(HTML("
                            #custom-logo {
                              position: fixed;
                              right: 10px;
                              bottom: 10px;
                              z-index: 100;
                            }
                          "))
                        ),
                        tabPanel("Upload Data",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file1", "Choose CSV File"),
                                     downloadButton("downloadData", "Download Cleaned .xlsx file")
                                   ),
                                   mainPanel(
                                     img(src = "as400.png", height = 700, width = 1200),
                                     tags$br(),  # Line break
                                     tags$br(),  # Additional line break
                                     tags$br(),  # Additional line break
                                     tags$p("If you'd like to view data for a different date range, please feel free to upload your own AS400 data (2269)",
                                            style = "font-size: 18px; font-weight: bold;") # CSS styling for bigger and bold text
                                   )
                                 )),
                        tabPanel("View Data",
                                 fluidRow(
                                   column(width = 12, 
                                          dataTableOutput("viewData"))
                                 )),
                        absolutePanel(id = "custom-logo", 
                                      img(src = "VenturaFoodsLogo.png", height = 60, width = 300)) # Adjust height and width as needed
)



# Server logic
server <- function(input, output) {
  data_to_download <- reactiveVal()
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    
    raw_data_as400 <- read_csv(inFile$datapath)
    
    raw_data_as400[1:6, ] -> data_info
    raw_data_as400[c(-1:-6, -8:-9), ] -> cleaned_data
    
    cleaned_data %>% 
      janitor::clean_names() %>% 
      tidyr::separate(report_id_cs730br, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"), sep = ",") -> cleaned_data
    
    colnames(cleaned_data) <- cleaned_data[1, ]
    
    cleaned_data %>% 
      janitor::clean_names() %>% 
      dplyr::slice(-1) %>% 
      data.frame() %>% 
      dplyr::mutate(across(contains("date"), ~as.Date(.x, format="%m/%d/%Y")),
                    week_number = lubridate::isoweek(order_date),
                    date_acknowledgement_calc = ifelse(is.na(date_acknowledge), as.character(Sys.Date()), as.character(date_acknowledge))) %>% 
      dplyr::mutate(date_acknowledgement_calc = as.Date(date_acknowledgement_calc, format="%Y-%m-%d")) %>% 
      dplyr::mutate(target = "2") %>% 
      dplyr::mutate(days_to_acknowledge = ifelse(!is.na(date_acknowledgement_calc) & !is.na(order_date), as.numeric(date_acknowledgement_calc - order_date), "-")) %>% 
      dplyr::mutate(fail = ifelse(days_to_acknowledge > 2, "yes", "No")) %>% 
      dplyr::relocate(week_number, .after = order_date) %>% 
      dplyr::mutate(across(-contains("date"), ~ifelse(is.na(.x), "-", .x))) %>% 
      dplyr::rename("Profile owner" = profile_owner,
                    "Profile name" = profile_name,
                    "Enter by" = enter_by,
                    "Enter by name" = enter_by_name,
                    Leader = leader,
                    "Leader name" = leader_name,
                    Loc = loc,
                    Order = order,
                    "Customer name" = customer_name,
                    Customer = customer,
                    "Order date" = order_date,
                    "Week Number" = week_number,
                    "Delivery date" = delivery_date,
                    "Ship date" = ship_date,
                    "Detail hold code" = detail_hold_code,
                    "Order ack" = order_ack,
                    "Date acknowledge" = date_acknowledge,
                    "Date Acknowledgement Calc." = date_acknowledgement_calc,
                    Target = target,
                    "Days to acknowledge" = days_to_acknowledge,
                    Fail = fail) -> cleaned_data_for_viz
    
    
    data_to_download(cleaned_data_for_viz)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cleaned_data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write_xlsx(data_to_download(), file)
    }
  )
  
  output$viewData <- renderDataTable({
    datatable(data_to_download(), extensions = 'Buttons', 
              options = list(
                pageLength = 100,
                scrollX = TRUE,
                scrollY = "600px",  # set a fixed height for vertical scrolling
                dom = "Blfrtip",
                buttons = c("copy", "csv", "excel"),
                fixedColumns = list(leftColumns = 2),
                searchDelay = 500
              ),
              filter = 'top' # This adds the filter input on top of each column
    )
  })
}

# Create Shiny app
shinyApp(ui, server)
