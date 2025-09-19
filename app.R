# https://shiny.posit.co/r/gallery/application-layout/retirement-simulation/

library(shiny)
library(dplyr)
library(tidyr)
library(slider)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)

source("./R/funs.R")
source("./R/f.R")

# Read data ----------------------------------------------------------------------------------------------------------------

d <- get_data()
dat_df    <- d$dat
chart_df  <- d$chart 
ref_df    <- d$ref
txn_df    <- d$txn_type
cx_df     <- d$cx_delta 
ox_df     <- d$ox_delta
initial_fcast_yr <- min(dat_df[dat_df$entity == "CW", ]$year)


# Function to specify UI inputs for rendering
renderInputs <- function(prefix) {
  wellPanel(  # Creates a panel with a slightly inset border and grey background.
    fluidRow(
      column(
        6,
        sliderInput(
          inputId = paste0(prefix, "_", "cost_of_debt_nmnl"), 
          label   = "Nominal cost of debt :", 
          min     = 0.01, 
          max     = 0.10, 
          step    = 0.01, 
          value   = 0.04
          ),
        sliderInput(
          inputId = paste0(prefix, "_", "fcast_infltn"),
          label   = "Forecast inflation :", 
          min     = 0.01, 
          max     = 0.07, 
          step    = 0.005, 
          value   = 0.025
          ),
        sliderInput(
          inputId = paste0(prefix, "_", "roe"),
          #inputId = "fcast_infltn", 
          label   = "Allowable return on equity :", 
          min     = 0.03, 
          max     = 0.06, 
          step    = 0.001, 
          value   = 0.041
        )
      ),
      column(
        6,
        selectInput(
          inputId = paste0(prefix, "_", "oxcx_scenario"), 
          #inputId = "oxcx_scenario", 
          label   = "Scenario:", 
          choices = c("scnr1","scnr2","scnr3","scnr4")
          ),
        br(),
        checkboxInput(paste0(prefix, "_", "single_price_delta"), "Price adjustment to occur in first year only", FALSE)
      )
    ),
    p(actionButton(inputId = "recalc", label = "Re-run simulation", icon = icon("random"))
    )
  )
}



# Define UI for application that plots KPI's
ui <- navbarPage(
  "App Title",
  tabPanel(
    title="Scenario input",
    fluidPage(
      theme="simplex.min.css",
      tags$style(
        type="text/css",
        "label {font-size: 12px;}",
        ".recalculating {opacity: 1.0;}"
        ),
      
      # Application title
      tags$h2("Regulatory price path model: scenario based KPI simulation"),
      p("Refer to the ",
        tags$a(href="https://www.audit.vic.gov.au/report/auditor-generals-report-annual-financial-report-state-victoria-2017-18?section=33061&show-sections=1#33059--appendix-f-water-sector-financial-sustainability-risk-indicators", "Long-term financial sustainability indicators"),
        "from the Victorial Auditor-Generals report."
        ),
      hr(),
      
      fluidRow(
        column(6, tags$h3("Scenario A")),
        column(6, tags$h3("Scenario B"))
        ),
      
      # Renders the data input sliders
      fluidRow(
        column(6, renderInputs("a")),
        column(6, renderInputs("b"))
        ),
      
      # Renders the plots 
      fluidRow(
        column(6, plotOutput("a_plot", height = "600px")),
        column(6, plotOutput("b_plot", height = "600px"))
        )
      )
    ),
  tabPanel(
    title="Financials", 
    fluidPage(
      theme="simplex.min.css",
      tags$style(
        type="text/css",
        "label {font-size: 12px;}",
        ".recalculating {opacity: 1.0;}"
        ),
      sidebarLayout(
        selectInput(
          inputId  = "fy_select", 
          label    = "Select financial years to display below:",  
          choices  = c(paste("FY", 2024:2043, sep = "")), 
          selected = c("FY2024","FY2025","FY2028","FY2033","FY2038","FY2043"),
          multiple = TRUE
          ),
        mainPanel(tableOutput("fins_kable"))
        )
      )
    ),
  tabPanel(
    title="Downloads", 
    fluidRow(
      column(3, downloadButton("tb_dload" , "Download simulation TB .csv")),
      column(3, downloadButton("rab_dload", "Download RAB .csv"))
    )
  )
)



# Server function
server <- function(input, output, session) {
  
  simA <- reactive(list(
    f( dat=dat_df, chart=chart_df, txn_type=txn_df, cx_delta=cx_df, ox_delta=ox_df,  
       q_grow            = 0.019,
       cost_of_debt_nmnl = input$a_cost_of_debt_nmnl, 
       fcast_infltn      = input$a_fcast_infltn,
       roe               = input$a_roe, 
       single_price_delta= input$a_single_price_delta,
       debt_sens         = NULL, 
       oxcx_scenario     = input$a_oxcx_scenario,
       verbose           = F))
    )
  simB <- reactive(list(
    f( dat=dat_df, chart=chart_df, txn_type=txn_df, cx_delta=cx_df, ox_delta=ox_df,  
       q_grow            = 0.019,
       cost_of_debt_nmnl = input$b_cost_of_debt_nmnl, 
       fcast_infltn      = input$b_fcast_infltn,
       roe               = input$b_roe, 
       single_price_delta= input$b_single_price_delta,
       debt_sens         = NULL, 
       oxcx_scenario     = input$b_oxcx_scenario,
       verbose           = F))
    )
  
  output$a_plot     <- renderPlot( {plot_kpi( simA(), initial_fcast_yr )} )
  output$b_plot     <- renderPlot( {plot_kpi( simB(), initial_fcast_yr )} )
  output$fins_kable <- renderText( {plot_fins(d=simA(), chart=chart_df, ref=ref_df, sel=input$fy_select)} )
  output$tb_dload   <- downloadHandler(
    filename = function() {"trial_balance.csv"},
    content = function(file) {write.csv(tb(d=simA(), chart=chart_df, ref=ref_df), file, quote = FALSE)}
    )
  output$rab_dload   <- downloadHandler(
    filename = function() {"rab_balance.csv"},
    content = function(file) {write.csv(rab(simA()), file, quote = FALSE)}
  )
  
}



# app.R
shinyApp(ui, server)
