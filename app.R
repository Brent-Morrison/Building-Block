# https://shiny.posit.co/r/gallery/application-layout/retirement-simulation/

library(shiny)
library(shinyBS)
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


# Helper functions for UI
row_with_label3 <- function(label, input1, input2=NULL, input3=NULL) {
  fluidRow(
    style = "display: flex; align-items: center;",
    column(2, tags$h5(label, class = "text-end")),
    column(3, input1),
    if (!is.null(input2)) column(3, input2),
    if (!is.null(input3)) column(3, input3)
  )
}

row_with_label4 <- function(label, input1, input2=NULL, input3=NULL, input4=NULL) {
  fluidRow(
    style = "display: flex; align-items: center;",
    column(2, tags$h5(label, class = "text-end")),
    column(2, input1),
    if (!is.null(input2)) column(2, input2),
    if (!is.null(input3)) column(2, input3),
    if (!is.null(input3)) column(2, input4)
  )
}


# Function to specify UI inputs for rendering ------------------------------------------------------------------------------

renderInputs <- function(prefix) {
  wellPanel(  # Creates a panel with a slightly inset border and grey background.
    tags$h4("Current price submission : FY24-28"),
    br(),
    fluidRow(
      column(2, tags$h5(" ", style = "text-align: left;")),
      column(2, tags$h5("Quantity", style = "text-align: left;")),
      column(2, tags$h5("Price", style = "text-align: left;")),
      column(2, tags$h5("Quantity growth", style = "text-align: left;")),
      column(2, tags$h5("NI price delta", style = "text-align: left;"))
    ),
    br(),
    fluidPage(
      row_with_label4(
        "Labour",
        numericInput(inputId = paste0(prefix, "_", "fte"), label = "Full time equivalent", value = 200, min = 50, max = 500),
        numericInput(inputId = paste0(prefix, "_", "cost_fte"), label = "Cost per FTE ($000)", value = 100, min = 30, max = 200),
        numericInput(inputId = paste0(prefix, "_", "q_grow_fte"), label = "Percentage growth in FTE's", value = 2, min = -5, max = 10),
        numericInput(inputId = paste0(prefix, "_", "ni_cost_fte"), label = "Relative price change", value = 2, min = -5, max = 10)
      ),
      #br(),
      row_with_label4(
        "Electricity",
        numericInput(inputId = paste0(prefix, "_", "kwh"), label = "Kilowatt-hour (kWh)", value = 99, min = 30, max = 99),
        numericInput(inputId = paste0(prefix, "_", "cost_kwh"), label = "Price per kilowatt-hour", value = 10, min = 5, max = 20),
        numericInput(inputId = paste0(prefix, "_", "q_grow_kwh"), label = "Percentage growth in kWh", value = 2, min = -5, max = 10),
        numericInput(inputId = paste0(prefix, "_", "ni_cost_kwh"), label = "Relative price change", value = 2, min = -5, max = 10)
      ),
      row_with_label4(
        "Chemicals",
        numericInput(inputId = paste0(prefix, "_", "ml"), label = "Water volume (ML)", value = 22, min = 10, max = 30),
        numericInput(inputId = paste0(prefix, "_", "cost_ml"), label = "Chemical costs per ML", value = 120, min = 80, max = 200),
        numericInput(inputId = paste0(prefix, "_", "q_grow_ml"), label = "Percentage growth in ML", value = 2, min = -5, max = 10),
        numericInput(inputId = paste0(prefix, "_", "ni_cost_ml"), label = "Relative price change", value = 2, min = -5, max = 10)
      )
      
    ),
    br(),
    hr(),
    tags$h4("Subsequent price submissions"),
    fluidRow(
      column(2, tags$h5(" ", style = "text-align: center;")),
      column(3, tags$h5("FY29-33", style = "text-align: center;")),
      column(3, tags$h5("FY34-38", style = "text-align: center;")),
      column(3, tags$h5("FY39-43", style = "text-align: center;"))
    ),
    br(),
    fluidPage(
      row_with_label3(
        "Opex",
        sliderInput(inputId = paste0(prefix, "_", "opex_ps2"), label = NULL, min = 100, max = 750, step = 50, value = 250),
        sliderInput(inputId = paste0(prefix, "_", "opex_ps3"), label = NULL, min = 100, max = 750, step = 50, value = 250),
        sliderInput(inputId = paste0(prefix, "_", "opex_ps4"), label = NULL, min = 100, max = 750, step = 50, value = 250)
      ),
      br(),
      row_with_label3(
        "Capex",
        sliderInput(inputId = paste0(prefix, "_", "capex_ps2"), label = NULL, min = 250, max = 1250, step = 250, value = 500),
        sliderInput(inputId = paste0(prefix, "_", "capex_ps3"), label = NULL, min = 250, max = 1250, step = 250, value = 500),
        sliderInput(inputId = paste0(prefix, "_", "capex_ps4"), label = NULL, min = 250, max = 1250, step = 250, value = 500)
        
      ),
      br(),
      row_with_label3(
        "FY39-43",
        sliderInput(inputId = paste0(prefix, "_", "roe"), label = "Allowable return on equity :", min = 0.03, max = 0.06, step = 0.001, value = 0.041),
        sliderInput(inputId = paste0(prefix, "_", "fcast_infltn"), label = "Forecast inflation :", post = "%", min = 1, max = 7, step = 0.5, value = 2.5),
        sliderInput(inputId = paste0(prefix, "_", "cost_of_debt_nmnl"), label = "Nominal cost of debt :", min = 0.01, max = 0.10, step = 0.01, value = 0.04)
      ),
      hr(),
      row_with_label3(
        "Other",
        numericInput(inputId = paste0(prefix, "_", "desired_fixed"), label = "Tariff composition :", value = 99, min = 30, max = 99),
        checkboxInput(paste0(prefix, "_", "single_price_delta"), "Price adjustment to occur in first year only", FALSE)
      ),
      bsTooltip(id = paste0(prefix, "_", "desired_fixed"), title = "Stipulate the tariff composition with respect to fixed versus variable charges.  Default value of 99 retains existing tariff composition", placement = "bottom"),
    )

    
    # fluidRow(
    #   column( # 1st column
    #     4,
    #     sliderInput(inputId = paste0(prefix, "_", "cost_of_debt_nmnl"), label = "Nominal cost of debt :", min = 0.01, max = 0.10, step = 0.01, value = 0.04),
    #     sliderInput(inputId = paste0(prefix, "_", "fcast_infltn"), label = "Forecast inflation :", post = "%", min = 1, max = 7, step = 0.5, value = 2.5),
    #     sliderInput(inputId = paste0(prefix, "_", "roe"), label = "Allowable return on equity :", min = 0.03, max = 0.06, step = 0.001, value = 0.041),
    #     numericInput(inputId = paste0(prefix, "_", "desired_fixed"), label = "Tariff composition :", value = 99, min = 30, max = 99),
    #     bsTooltip(id = paste0(prefix, "_", "desired_fixed"), title = "Stipulate the tariff composition with respect to fixed versus variable charges.  Default value of 99 retains existing tariff composition", placement = "bottom")
    #   ),
    #   column( # 2nd column
    #     4,
    #     sliderInput(inputId = paste0(prefix, "_", "capex_ps2"), label = "Capex (FY29-33) :", min = 250, max = 1250, step = 250, value = 500),
    #     sliderInput(inputId = paste0(prefix, "_", "capex_ps3"), label = "Capex (FY34-38) :", min = 250, max = 1250, step = 250, value = 500),
    #     sliderInput(inputId = paste0(prefix, "_", "capex_ps4"), label = "Capex (FY39-43) :", min = 250, max = 1250, step = 250, value = 500),
    #     #selectInput(inputId = paste0(prefix, "_", "oxcx_scenario"), label = "Scenario:", choices = c("scnr1","scnr2","scnr3","scnr4")),
    #     br(),
    #     checkboxInput(paste0(prefix, "_", "single_price_delta"), "Price adjustment to occur in first year only", FALSE)
    #   ),
    #   column( # 3rd column
    #     4,
    #     sliderInput(inputId = paste0(prefix, "_", "opex_ps2"), label = "Opex (FY29-33) :", min = 100, max = 750, step = 50, value = 250),
    #     sliderInput(inputId = paste0(prefix, "_", "opex_ps3"), label = "Opex (FY34-38) :", min = 100, max = 750, step = 50, value = 250),
    #     sliderInput(inputId = paste0(prefix, "_", "opex_ps4"), label = "Opex (FY39-43) :", min = 100, max = 750, step = 50, value = 250),
    #     )
    # ) # end fluidRow
    #p(actionButton(inputId = "recalc", label = "Re-run simulation", icon = icon("random")))
  )
}



# Define UI for application that plots KPI's
ui <- navbarPage(
  "Financial Model",
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
          selected = c("FY2024","FY2025","FY2026","FY2027","FY2028","FY2033","FY2038","FY2043"),
          multiple = TRUE
          ),
        mainPanel(tableOutput("fins_kable"))
        )
      )
    ),
  tabPanel(
    title="Tariffs", 
    fluidRow(
      column(6, tags$h3("Scenario A")),
      column(6, tags$h3("Scenario B"))
    ),
    fluidRow(
      column(6, plotOutput("a_tariff_plot", height = "600px")),
      column(6, plotOutput("b_tariff_plot", height = "600px"))
    )
    # mainPanel(
    #   plotOutput("tariff_plot", height = "600px")
    # )
  ),
  tabPanel(
    title="Downloads", 
    fluidRow(
      column(3, downloadButton("tb_dload" , "Download simulation TB .csv")),
      column(3, downloadButton("rab_dload", "Download RAB .csv"))
    )
  )
)



# Server function ----------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  simA <- reactive(list(
    f( dat=dat_df, chart=chart_df, txn_type=txn_df, ref=ref_df, cx_delta=cx_df, ox_delta=ox_df,  
       q_grow            = 0.019,
       cost_of_debt_nmnl = input$a_cost_of_debt_nmnl, 
       fcast_infltn      = input$a_fcast_infltn/100,
       roe               = input$a_roe, 
       single_price_delta= input$a_single_price_delta,
       desired_fixed     = input$a_desired_fixed,
       debt_sens         = NULL, 
       #oxcx_scenario     = input$a_oxcx_scenario,
       ml                = input$a_ml,
       cost_ml           = input$a_cost_ml,
       q_grow_ml         = input$a_q_grow_ml / 100,
       ni_cost_ml        = input$a_ni_cost_ml / 100,
       capex_ps2         = input$a_capex_ps2,
       capex_ps3         = input$a_capex_ps3,
       capex_ps4         = input$a_capex_ps4,
       opex_ps2          = input$a_opex_ps2,
       opex_ps3          = input$a_opex_ps3,
       opex_ps4          = input$a_opex_ps4,
       verbose           = F))
    )
  simB <- reactive(list(
    f( dat=dat_df, chart=chart_df, txn_type=txn_df, ref=ref_df, cx_delta=cx_df, ox_delta=ox_df,  
       q_grow            = 0.019,
       cost_of_debt_nmnl = input$b_cost_of_debt_nmnl, 
       fcast_infltn      = input$b_fcast_infltn/100,
       roe               = input$b_roe, 
       single_price_delta= input$b_single_price_delta,
       desired_fixed     = input$b_desired_fixed,
       debt_sens         = NULL, 
       #oxcx_scenario     = input$b_oxcx_scenario,
       ml                = input$b_ml,
       cost_ml           = input$b_cost_ml,
       q_grow_ml         = input$b_q_grow_ml / 100,
       ni_cost_ml        = input$b_ni_cost_ml / 100,
       capex_ps2         = input$b_capex_ps2,
       capex_ps3         = input$b_capex_ps3,
       capex_ps4         = input$b_capex_ps4,
       opex_ps2          = input$b_opex_ps2,
       opex_ps3          = input$b_opex_ps3,
       opex_ps4          = input$b_opex_ps4,
       verbose           = F))
    )
  
  output$a_plot        <- renderPlot( {plot_kpi( simA(), initial_fcast_yr )} )
  output$b_plot        <- renderPlot( {plot_kpi( simB(), initial_fcast_yr )} )
  output$fins_kable    <- renderText( {plot_fins(d=simA(), chart=chart_df, ref=ref_df, sel=input$fy_select)} )
  output$a_tariff_plot <- renderPlot( {plot_tariffs( simA() )} )
  output$b_tariff_plot <- renderPlot( {plot_tariffs( simB() )} )
  output$tb_dload      <- downloadHandler(
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
