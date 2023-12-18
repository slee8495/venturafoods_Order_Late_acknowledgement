library(tidyverse)
library(openxlsx)
library(readxl)
library(writexl)
library(lubridate)



clean_data <- function(raw_data_as400) {
  
  raw_data_as400[3, ] -> data_info
  raw_data_as400[c(-1:-6, -8:-9), ] -> cleaned_data
  
  cleaned_data %>% 
    janitor::clean_names() %>% 
    tidyr::separate(report_id_cs730br, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"), sep = ",") -> cleaned_data
  
  colnames(cleaned_data) <- cleaned_data[1, ]
  
  cleaned_data <- cleaned_data %>% 
    janitor::clean_names() %>% 
    dplyr::slice(-1) %>% 
    data.frame() %>% 
    dplyr::mutate(across(contains("date"), ~as.Date(.x, format="%m/%d/%Y")),
                  week_number = lubridate::isoweek(order_date),
                  date_acknowledgement_calc = ifelse(is.na(date_acknowledge), as.character(Sys.Date()), as.character(date_acknowledge)),
                  date_acknowledgement_calc = as.Date(date_acknowledgement_calc, format="%Y-%m-%d"),
                  days_to_acknowledge = ifelse(!is.na(date_acknowledgement_calc) & !is.na(order_date), as.numeric(date_acknowledgement_calc - order_date), "-"),
                  fail = ifelse(days_to_acknowledge > 2, "Yes", "No")) %>% 
    
    plyr::mutate(`On Time` = ifelse(fail == "Yes", "Not on Time", "On Time")) %>%
    dplyr::select(-fail) %>%
    
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
                  CustomerName = customer_name,
                  Customer = customer,
                  OrderDate = order_date,
                  Week = week_number,
                  DeliveryDate = delivery_date,
                  ShipDate = ship_date,
                  "Detail hold code" = detail_hold_code,
                  "Order Acknowledgement Flag" = order_ack,
                  "Date acknowledge" = date_acknowledge,
                  "Date Acknowledgement Calc." = date_acknowledgement_calc,
                  "Days to acknowledge" = days_to_acknowledge) %>% 
    dplyr::filter(!is.na(OrderDate) & OrderDate != 0)
  
  return(cleaned_data)
}


data_info <- function() {
  data_info <- raw_data_as400[3, ]
  return(data_info)
}

# Default Data 
raw_data_as400 <- read_csv("SUM518685.CSV")
cleaned_default_data <- clean_data(raw_data_as400)


