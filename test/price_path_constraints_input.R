library(shiny)

# ------------------------------------------------------------------------------
# Tariff master data
# ------------------------------------------------------------------------------

tariffs <- data.frame(
  tariff = c(
    "Water.Rural.Fixed",
    "Water.Rural.Variable",
    "Sewerage.Non-residential.Fixed",
    "Sewerage.Non-residential.Variable",
    "Trade-Waste.Non-residential.Fixed",
    "Trade-Waste.Non-residential.Variable",
    "Water.Non-residential.Fixed",
    "Water.Non-residential.Variable",
    "Water.Residential.Fixed",
    "Water.Residential.Variable"
  ),
  p0 = c(
    180.1002,
    0.2653,
    696.4600,
    0.9926,
    163.1300,
    0.6565,
    414.4120,
    2.2860,
    234.4724,
    2.2860
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------------------------

row_with_tariff <- function(tariff, id) {
  
  fluidRow(
    style = "display:flex; align-items:center;",
    column(3, tags$h5(tariff)),
    column(2, selectInput(
      inputId = paste0(id, "_opt"), label = NULL, width = "100%",
        choices = c("Price target", "Fixed percent change", "Price path group 1", "Price path group 2"),
        selected = "Price target")),
    column(1, numericInput(inputId = paste0(id, "_p1"), label = NULL, value = 0)),
    column(1, numericInput(inputId = paste0(id, "_p2"), label = NULL, value = 0)),
    column(1, numericInput(inputId = paste0(id, "_p3"), label = NULL, value = 0)),
    column(1, numericInput(inputId = paste0(id, "_p4"), label = NULL, value = 0)),
    column(1, numericInput(inputId = paste0(id, "_p5"), label = NULL, value = 0))
  )
  
}


renderTariffInputs <- function() {
  wellPanel(
    tags$h4("Tariff settings"),
    fluidRow(
      column(3, tags$b("Tariff")),
      column(2, tags$b("Option")),
      column(1, tags$b("P1")),
      column(1, tags$b("P2")),
      column(1, tags$b("P3")),
      column(1, tags$b("P4")),
      column(1, tags$b("P5"))
    ),
    tags$hr(),
    uiOutput("tariff_rows")
  )
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Tariff Configuration"),
  
  selectInput(
    inputId = "selected_tariffs",
    label = "Select tariffs",
    choices = tariffs$tariff,
    selected = tariffs$tariff[1:3],
    multiple = TRUE
  ),
  
  br(),
  
  renderTariffInputs(),
  
  hr(),
  
  h4("Resulting dataframe"),
  
  tableOutput("result")
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Dynamically create tariff rows
  output$tariff_rows <- renderUI({
    
    req(input$selected_tariffs)
    
    tagList(
      
      lapply(
        seq_along(input$selected_tariffs),
        function(i) {
          
          row_with_tariff(
            tariff = input$selected_tariffs[i],
            id = paste0("tariff_", i)
          )
          
        }
      )
      
    )
    
  })
  
  # Reconstruct dataframe from inputs
  tariff_df <- reactive({
    
    req(input$selected_tariffs)
    
    do.call(
      rbind,
      lapply(
        seq_along(input$selected_tariffs),
        function(i) {
          
          tariff_name <- input$selected_tariffs[i]
          
          data.frame(
            tariff = tariff_name,
            p0 = tariffs$p0[
              match(tariff_name, tariffs$tariff)
            ],
            opt = input[[paste0("tariff_", i, "_opt")]],
            p1 = input[[paste0("tariff_", i, "_p1")]],
            p2 = input[[paste0("tariff_", i, "_p2")]],
            p3 = input[[paste0("tariff_", i, "_p3")]],
            p4 = input[[paste0("tariff_", i, "_p4")]],
            p5 = input[[paste0("tariff_", i, "_p5")]],
            stringsAsFactors = FALSE
          )
        }
      )
    )
    
  })
  
  output$result <- renderTable({
    tariff_df()
  }, rownames = FALSE)
  
}

# ------------------------------------------------------------------------------
# Run app
# ------------------------------------------------------------------------------

shinyApp(ui, server)