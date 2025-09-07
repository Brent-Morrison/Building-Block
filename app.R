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
dat_df          <- d$dat
chart_df        <- d$chart 
txn_df          <- d$txn_type
cx_df           <- d$cx_delta 
ox_df           <- d$ox_delta

renderInputs <- function(prefix) {
  wellPanel(
    fluidRow(
      column(6,
             sliderInput(paste0(prefix, "_", "cost_of_debt_nmnl"), "Nominal cost of debt :", min = 0.01, max = 0.10, step = 0.01 , value = 0.04),
             sliderInput(paste0(prefix, "_", "fcast_infltn")     , "Forecast inflation :"  , min = 0.01, max = 0.07, step = 0.005, value = 0.025)
      ),
      column(6,
             selectInput(paste0(prefix, "_", "oxcx_scenario")    , "Scenario:", choices = c("scnr1","scnr2","scnr3","scnr4"))
      )
    ),
    p(actionButton(paste0(prefix, "_", "recalc"),
                   "Re-run simulation", icon("random")
    ))
  )
}

# Define UI for application that plots random distributions
ui <- fluidPage(
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
  fluidRow(
    column(6, renderInputs("a")),
    column(6, renderInputs("b"))
  ),
  fluidRow(
    column(6, plotOutput("a_distPlot", height = "600px")),
    column(6, plotOutput("b_distPlot", height = "600px"))
  )
)







paramNames <- c("dat","chart","txn_type","cx_delta","ox_delta","cost_of_debt_nmnl", "fcast_infltn", "oxcx_scenario")

server <- function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  # Function that generates scenarios and computes NAV. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  simA <- reactive(do.call(f, getParams("a")))
  simB <- reactive(do.call(f, getParams("b")))
  
  # Expression that plot NAV paths. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  #
  output$a_distPlot <- renderPlot({
    plot_kpi(simA())
  })
  output$b_distPlot <- renderPlot({
    plot_kpi(simB())
  })
  
}


# app.R
shinyApp(ui, server)
