#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

server <- function(input, output, session) {
  # Upload ---------------------------------------------------------
  raw <- reactive({
    req(input$file)
    read_csv(input$file$datapath, 
             na = "not requested", 
             col_types = cols(.default = "c",
                              `Sample Account` = "i", 
                              `Sample Number` = "i")) %>%
      pivot_longer(Moisture:last_col(), names_to = "analyte", values_to = "value") %>%
      mutate(value = as.numeric(value),
             `Date Printed` = parse_date(`Date Printed`, "%m/%d/%Y"),
             `Sample Date` = parse_date(`Sample Date`, "%m/%d/%Y")) %>%
      drop_na(value)
  })
  
  output$preview1 <- renderTable(head(raw(), 3))
  
  # Clean ----------------------------------------------------------
  as_fed <- reactive({
    
    dms <- raw() %>%
      filter(analyte == "Dry Matter") %>%
      select(`Sample Number`, `Dry Matter` = value)
    
    
    if (input$DAK) {
      as_fed <- left_join(dms, raw(), by = "Sample Number") %>%
        mutate(af_value = as.double(value * `Dry Matter`/100),
               final_value = case_when(
                 analyte == "Dry Matter" ~ `Dry Matter`,
                 analyte == "Gross Energy  cal/g" ~ af_value/10,
                 TRUE ~ af_value)
        ) %>%
        arrange(`Sample Number`, analyte)  %>%
        mutate(dak_id1 = str_extract(`Desc 1`, "DAK *[:digit:]+"),
               dak_id2 = str_extract(`Description 2`, "DAK *[:digit:]+"),
               dak_id3 = as.character(parse_number(`Desc 1`)),
               dak_id4 = as.character(parse_number(`Description 2`)),
               dak_id5 = str_extract(`Desc 1`, "[DAK *|DK|D A K  |DAK-][:digit:]+"),
               dak_id6 = coalesce(dak_id1, dak_id2, dak_id3, dak_id4, dak_id5),
               `Farm Sample Number` = abs(parse_number(dak_id6)),
               ZDN_FoodID = 0,
               .before = 1) %>%
        filter(`Farm Sample Number` > 200) %>%
        select(`Sample Number`, `Date Printed`, `Desc 1`, `Description 2`, `Desc 3`, Kind, `Farm Sample Number`, analyte, final_value, ZDN_FoodID)
        
    } else {
      
      as_fed <- left_join(dms, raw(), by = "Sample Number") %>%
        mutate(af_value = as.double(value * `Dry Matter`/100),
               final_value = case_when(
                 analyte == "Dry Matter" ~ `Dry Matter`,
                 analyte == "Gross Energy  cal/g" ~ af_value/10,
                 TRUE ~ af_value),
               ZDN_FoodID = 0
        ) %>%
        select(`Sample Number`, `Date Printed`, `Desc 1`, `Description 2`, `Desc 3`, Kind, `Farm Sample Number`, analyte, final_value, ZDN_FoodID) %>%
        arrange(`Sample Number`, analyte)
      
    }
    
    as_fed
    
  })
  output$preview2 <- renderTable(head(as_fed(), 3))
  
  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "-ready4ZDN", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(as_fed(), file)
    }
  )
}