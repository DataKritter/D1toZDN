# Run this before publishing:
# .rs.files.restoreBindings()

library(shiny)



ui_upload <- sidebarLayout(

  sidebarPanel(
    fileInput("file", "Load CSV file", buttonLabel = "Upload..."),
    "Important: Dairy One files must be in the '-29.csv' format.",
  ),
  mainPanel(
    h3("Raw D1 data"),
    
    tableOutput("preview1")
  )
)


ui_clean <- sidebarLayout(
  sidebarPanel(
    checkboxInput("DAK", "Does this dataset contain zoo sample ID numbers (within the D1 descriptions)?", value = FALSE),

  ),
  mainPanel(
    h3("Cleaned data ready for ZDN"),
    tableOutput("preview2")
  )
)


ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
)

ui <- fluidPage(
  titlePanel(HTML(paste0("Convert ", "<b>","Dairy One","</b> ", " to ", "<b>","Zoo Diets NaviGator","</b>"))),
  ui_upload,
  ui_clean,
  ui_download,


)