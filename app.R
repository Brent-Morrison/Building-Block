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
    if (!is.null(input4)) column(2, input4)
  )
}


# Function to specify UI inputs for rendering ------------------------------------------------------------------------------

renderInputs <- function(prefix) {
  wellPanel(  # Creates a panel with a slightly inset border and grey background.
    
    tags$h4("Current price submission : FY24-28"),
    br(),
    tags$h4("Operational expenditure"),
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
        numericInput(inputId = paste0(prefix, "_", "kwh"), label = "GWh (million kWh)", value = 25, min = 20, max = 50),
        numericInput(inputId = paste0(prefix, "_", "cost_kwh"), label = "Price per kWh (cents)", value = 22, min = 5, max = 20),
        numericInput(inputId = paste0(prefix, "_", "q_grow_kwh"), label = "Percentage growth in kWh", value = 2, min = -5, max = 10),
        numericInput(inputId = paste0(prefix, "_", "ni_cost_kwh"), label = "Relative price change", value = 2, min = -5, max = 10)
        ),
      row_with_label4(
        "Chemicals",
        numericInput(inputId = paste0(prefix, "_", "ml"), label = "Water volume (GL)", value = 22, min = 10, max = 30),
        numericInput(inputId = paste0(prefix, "_", "cost_ml"), label = "Chemical costs per ML", value = 120, min = 80, max = 200),
        numericInput(inputId = paste0(prefix, "_", "q_grow_ml"), label = "Percentage growth in ML", value = 2, min = -5, max = 10),
        numericInput(inputId = paste0(prefix, "_", "ni_cost_ml"), label = "Relative price change", value = 2, min = -5, max = 10)
        ),
      row_with_label4(
        "Other",
        numericInput(inputId = paste0(prefix, "_", "dl"), label = "Dollars (mn)", value = 60, min = 40, max = 100),
        numericInput(inputId = paste0(prefix, "_", "cost_dl"), label = "Dummy", value = 1, min = 1, max = 1),
        numericInput(inputId = paste0(prefix, "_", "q_grow_dl"), label = "Percentage growth", value = 2, min = -5, max = 10),
        numericInput(inputId = paste0(prefix, "_", "ni_cost_dl"), label = "Relative price change", value = 2, min = -5, max = 10)
        ),
      br(),
      row_with_label3(
        "Cost of capital",
        sliderInput(inputId = paste0(prefix, "_", "roe"),               label = "Allowable return on equity :", post = "%", min = 3, max = 6, step = 0.1, value = 4.1),
        sliderInput(inputId = paste0(prefix, "_", "fcast_infltn"),      label = "Forecast inflation :"        , post = "%", min = 1, max = 7, step = 0.5, value = 2.5),
        sliderInput(inputId = paste0(prefix, "_", "cost_of_debt_nmnl"), label = "Nominal cost of debt :"      , post = "%", min = 1, max = 8, step = 0.5, value = 4)
      ),
      bsTooltip(id = paste0(prefix, "_", "cost_of_debt_nmnl"), title = "Cost of debt for return on assets WACC", placement = "bottom")
      
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
        "Incremental Opex (million p.a.)",
        sliderInput(inputId = paste0(prefix, "_", "opex_ps2"), label = NULL, min = -20, max = 50, step = 5, value = 10),
        sliderInput(inputId = paste0(prefix, "_", "opex_ps3"), label = NULL, min = -20, max = 50, step = 5, value = 10),
        sliderInput(inputId = paste0(prefix, "_", "opex_ps4"), label = NULL, min = -20, max = 50, step = 5, value = 10)
        ),
      br(),
      row_with_label3(
        "Capex (million p.a.)",
        sliderInput(inputId = paste0(prefix, "_", "capex_ps2"), label = NULL, min = 50, max = 250, step = 25, value = 100),
        sliderInput(inputId = paste0(prefix, "_", "capex_ps3"), label = NULL, min = 50, max = 250, step = 25, value = 100),
        sliderInput(inputId = paste0(prefix, "_", "capex_ps4"), label = NULL, min = 50, max = 250, step = 25, value = 100)
        ),
      br(),
      row_with_label3(
        "Allowable return on equity",
        sliderInput(inputId = paste0(prefix, "_", "roe1"), label = NULL, post = "%", min = 3, max = 6, step = 0.1, value = 4.1),
        sliderInput(inputId = paste0(prefix, "_", "roe2"), label = NULL, post = "%", min = 3, max = 6, step = 0.1, value = 4.1),
        sliderInput(inputId = paste0(prefix, "_", "roe3"), label = NULL, post = "%", min = 3, max = 6, step = 0.1, value = 4.1)
        ),
      br(),
      row_with_label3(
        "Forecast inflation",
        sliderInput(inputId = paste0(prefix, "_", "fcast_infltn1"), label = NULL, post = "%", min = 1, max = 7, step = 0.5, value = 2.5),
        sliderInput(inputId = paste0(prefix, "_", "fcast_infltn2"), label = NULL, post = "%", min = 1, max = 7, step = 0.5, value = 2.5),
        sliderInput(inputId = paste0(prefix, "_", "fcast_infltn3"), label = NULL, post = "%", min = 1, max = 7, step = 0.5, value = 2.5)
      ),
      br(),
      row_with_label3(
        "Nominal cost of debt",
        sliderInput(inputId = paste0(prefix, "_", "cost_of_debt_nmnl1"), label = NULL, post = "%", min = 3, max = 8, step = 0.5, value = 4),
        sliderInput(inputId = paste0(prefix, "_", "cost_of_debt_nmnl2"), label = NULL, post = "%", min = 3, max = 8, step = 0.5, value = 4),
        sliderInput(inputId = paste0(prefix, "_", "cost_of_debt_nmnl3"), label = NULL, post = "%", min = 3, max = 8, step = 0.5, value = 4)
      ),
      
      hr(),
      
      row_with_label3(
        "Other",
        numericInput(inputId = paste0(prefix, "_", "desired_fixed"), label = "Tariff composition :", value = 99, min = 30, max = 99),
        checkboxInput(inputId = paste0(prefix, "_", "single_price_delta"), label = "Price adjustment to occur in first year only", TRUE)
        ),
      bsTooltip(id = paste0(prefix, "_", "desired_fixed"), title = "Stipulate the tariff composition with respect to fixed versus variable charges.  Default value of 99 retains existing tariff composition", placement = "bottom"),
      
      hr(),

      tags$h4("Sensitivity analysis"),
      fluidRow(
        column(2, tags$h5(" ", style = "text-align: center;")),
        column(3, tags$h5("Parameter value", style = "text-align: center;")),
        column(3, tags$h5("Lower", style = "text-align: center;")),
        column(3, tags$h5("Upper", style = "text-align: center;"))
        ),

      row_with_label3(
        "", # Select desired attribute
        selectInput(inputId = paste0(prefix, "_", "sens_param"), label = "", choices = c("None", "Labour quantity","Labour price","Labour growth"), selected = "None"),
        numericInput(inputId = paste0(prefix, "_", "sens_lower"), label = "", value = 120, min = 80, max = 200),
        numericInput(inputId = paste0(prefix, "_", "sens_upper"), label = "", value = 300, min = -5, max = 10)
        ),
      
      hr(),

      actionButton(inputId = paste0("run_sim", "_", prefix), label = "Run simulation", class = "btn-primary")
      )
  ) # end WellPanel
}



# Define UI for application that plots KPI's
ui <- navbarPage(
  "Financial Model",
  tabPanel(
    title="Scenario input and KPI's",
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
      tags$h2("Key performance indicators"),
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
      column(2, downloadButton("tb_dload" , "Download simulation TB .csv")),
      column(2, downloadButton("rab_dload", "Download RAB .csv")),
      column(2, downloadButton("revreq_dload", "Download Rev Req .csv"))
    )
  )
)



# Server function ----------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  simA_inputs <- eventReactive(input$run_sim_a, {
    
    req(
      length(input$a_cost_of_debt_nmnl) == 1,
      length(input$a_fcast_infltn) == 1,
      length(input$a_roe) == 1,
      length(input$a_single_price_delta) == 1,
      length(input$a_desired_fixed) == 1,
      length(input$a_sens_param) == 1,
      length(input$a_fte) == 1,
      length(input$a_cost_fte) == 1,
      length(input$a_q_grow_fte) == 1,
      length(input$a_ni_cost_fte) == 1,
      length(input$a_kwh) == 1,
      length(input$a_cost_kwh) == 1,
      length(input$a_q_grow_kwh) == 1,
      length(input$a_ni_cost_kwh) == 1,
      length(input$a_ml) == 1,
      length(input$a_cost_ml) == 1,
      length(input$a_q_grow_ml) == 1,
      length(input$a_ni_cost_ml) == 1,
      length(input$a_dl) == 1,
      length(input$a_cost_dl) == 1,
      length(input$a_q_grow_dl) == 1,
      length(input$a_ni_cost_dl) == 1,
      length(input$a_capex_ps2) == 1,
      length(input$a_capex_ps3) == 1,
      length(input$a_capex_ps4) == 1,
      length(input$a_opex_ps2) == 1,
      length(input$a_opex_ps3) == 1,
      length(input$a_opex_ps4) == 1,
      length(input$a_sens_lower) == 1,
      length(input$a_sens_upper) == 1
      
    )
    
    
    list(
      q_grow               = 0.019,
      cost_of_debt_nmnl    = isolate(input$a_cost_of_debt_nmnl),
      fcast_infltn         = isolate(input$a_fcast_infltn),
      roe                  = isolate(input$a_roe),
      single_price_delta   = isolate(input$a_single_price_delta),
      desired_fixed        = isolate(input$a_desired_fixed),
      sens_param           = isolate(input$a_sens_param),
      fte                  = isolate(input$a_fte),
      cost_fte             = isolate(input$a_cost_fte),
      q_grow_fte           = isolate(input$a_q_grow_fte),
      ni_cost_fte          = isolate(input$a_ni_cost_fte),
      kwh                  = isolate(input$a_kwh),
      cost_kwh             = isolate(input$a_cost_kwh),
      q_grow_kwh           = isolate(input$a_q_grow_kwh),
      ni_cost_kwh          = isolate(input$a_ni_cost_kwh),
      ml                   = isolate(input$a_ml),
      cost_ml              = isolate(input$a_cost_ml),
      q_grow_ml            = isolate(input$a_q_grow_ml),
      ni_cost_ml           = isolate(input$a_ni_cost_ml),
      dl                   = isolate(input$a_dl),
      cost_dl              = isolate(input$a_cost_dl),
      q_grow_dl            = isolate(input$a_q_grow_dl),
      ni_cost_dl           = isolate(input$a_ni_cost_dl),
      capex_ps2            = isolate(input$a_capex_ps2),
      capex_ps3            = isolate(input$a_capex_ps3),
      capex_ps4            = isolate(input$a_capex_ps4),
      opex_ps2             = isolate(input$a_opex_ps2),
      opex_ps3             = isolate(input$a_opex_ps3),
      opex_ps4             = isolate(input$a_opex_ps4),
      sens_lower           = isolate(input$a_sens_lower),
      sens_upper           = isolate(input$a_sens_upper)
    )
  }, ignoreInit  =  TRUE
  )
  
  
  argsA <- reactive({
    
    inp <- simA_inputs()
    req(inp)
    
    expand.grid(
      q_grow               = inp$q_grow,
      cost_of_debt_nmnl    = inp$cost_of_debt_nmnl,
      fcast_infltn         = inp$fcast_infltn,
      roe                  = inp$roe,
      single_price_delta   = inp$single_price_delta,
      desired_fixed        = inp$desired_fixed,
      debt_sens            = list(NULL), 
      fte                  = if (inp$sens_param == "Labour quantity") c(inp$sens_lower, inp$fte,      inp$sens_upper) else inp$fte,
      cost_fte             = if (inp$sens_param == "Labour price")    c(inp$sens_lower, inp$cost_fte, inp$sens_upper) else inp$cost_fte,
      q_grow_fte           = if (inp$sens_param == "Labour growth")   c(inp$sens_lower, inp$cost_fte, inp$sens_upper) else inp$cost_fte,
      ni_cost_fte          = inp$ni_cost_fte,
      kwh                  = inp$kwh,
      cost_kwh             = inp$cost_kwh,
      q_grow_kwh           = inp$q_grow_kwh,
      ni_cost_kwh          = inp$ni_cost_kwh,
      ml                   = inp$ml,
      cost_ml              = inp$cost_ml,
      q_grow_ml            = inp$q_grow_ml,
      ni_cost_ml           = inp$ni_cost_ml,
      dl                   = inp$dl,
      cost_dl              = inp$cost_dl,
      q_grow_dl            = inp$q_grow_dl,
      ni_cost_dl           = inp$ni_cost_dl,
      capex_ps2            = inp$capex_ps2,
      capex_ps3            = inp$capex_ps3,
      capex_ps4            = inp$capex_ps4,
      opex_ps2             = inp$opex_ps2,
      opex_ps3             = inp$opex_ps3,
      opex_ps4             = inp$opex_ps4
      
    )
  })
  
  
  simA <- reactive({
    
    args_df <- argsA()
    req(nrow(args_df) > 0)
    
    mapply(
       FUN               = f, 
       dat=list(dat_df), chart=list(chart_df), txn_type=list(txn_df), ref=list(ref_df), cx_delta=list(cx_df), ox_delta=list(ox_df),
       q_grow            = args_df$q_grow,
       cost_of_debt_nmnl = args_df$cost_of_debt_nmnl/100, 
       fcast_infltn      = args_df$fcast_infltn/100,
       roe               = args_df$roe/100, 
       single_price_delta= args_df$single_price_delta,
       desired_fixed     = args_df$desired_fixed,
       debt_sens         = args_df$debt_sens, 
       fte               = args_df$fte,
       cost_fte          = args_df$cost_fte,
       q_grow_fte        = args_df$q_grow_fte/100,
       ni_cost_fte       = args_df$ni_cost_fte/100,
       kwh               = args_df$kwh,
       cost_kwh          = args_df$cost_kwh,
       q_grow_kwh        = args_df$q_grow_kwh/100,
       ni_cost_kwh       = args_df$ni_cost_kwh/100,
       ml                = args_df$ml,
       cost_ml           = args_df$cost_ml,
       q_grow_ml         = args_df$q_grow_ml/100,
       ni_cost_ml        = args_df$ni_cost_ml/100,
       dl                = args_df$dl,
       cost_dl           = args_df$cost_dl,
       q_grow_dl         = args_df$q_grow_dl/100,
       ni_cost_dl        = args_df$ni_cost_dl/100,
       capex_ps2         = args_df$capex_ps2,
       capex_ps3         = args_df$capex_ps3,
       capex_ps4         = args_df$capex_ps4,
       opex_ps2          = args_df$opex_ps2,
       opex_ps3          = args_df$opex_ps3,
       opex_ps4          = args_df$opex_ps4,
       verbose           = F,
       SIMPLIFY          = FALSE
       )
    
    })
  
  # ----
  
  simB_inputs <- eventReactive(input$run_sim_b, {
    
    req(
      length(input$b_cost_of_debt_nmnl) == 1,
      length(input$b_fcast_infltn) == 1,
      length(input$b_roe) == 1,
      length(input$b_single_price_delta) == 1,
      length(input$b_desired_fixed) == 1,
      length(input$b_sens_param) == 1,
      length(input$b_fte) == 1,
      length(input$b_cost_fte) == 1,
      length(input$b_q_grow_fte) == 1,
      length(input$b_ni_cost_fte) == 1,
      length(input$b_kwh) == 1,
      length(input$b_cost_kwh) == 1,
      length(input$b_q_grow_kwh) == 1,
      length(input$b_ni_cost_kwh) == 1,
      length(input$b_ml) == 1,
      length(input$b_cost_ml) == 1,
      length(input$b_q_grow_ml) == 1,
      length(input$b_ni_cost_ml) == 1,
      length(input$b_dl) == 1,
      length(input$b_cost_dl) == 1,
      length(input$b_q_grow_dl) == 1,
      length(input$b_ni_cost_dl) == 1,
      length(input$b_capex_ps2) == 1,
      length(input$b_capex_ps3) == 1,
      length(input$b_capex_ps4) == 1,
      length(input$b_opex_ps2) == 1,
      length(input$b_opex_ps3) == 1,
      length(input$b_opex_ps4) == 1,
      length(input$b_sens_lower) == 1,
      length(input$b_sens_upper) == 1
      
    )
    
    
    list(
      q_grow               = 0.019,
      cost_of_debt_nmnl    = isolate(input$b_cost_of_debt_nmnl),
      fcast_infltn         = isolate(input$b_fcast_infltn),
      roe                  = isolate(input$b_roe),
      single_price_delta   = isolate(input$b_single_price_delta),
      desired_fixed        = isolate(input$b_desired_fixed),
      sens_param           = isolate(input$b_sens_param),
      fte                  = isolate(input$b_fte),
      cost_fte             = isolate(input$b_cost_fte),
      q_grow_fte           = isolate(input$b_q_grow_fte),
      ni_cost_fte          = isolate(input$b_ni_cost_fte),
      kwh                  = isolate(input$b_kwh),
      cost_kwh             = isolate(input$b_cost_kwh),
      q_grow_kwh           = isolate(input$b_q_grow_kwh),
      ni_cost_kwh          = isolate(input$b_ni_cost_kwh),
      ml                   = isolate(input$b_ml),
      cost_ml              = isolate(input$b_cost_ml),
      q_grow_ml            = isolate(input$b_q_grow_ml),
      ni_cost_ml           = isolate(input$b_ni_cost_ml),
      dl                   = isolate(input$b_dl),
      cost_dl              = isolate(input$b_cost_dl),
      q_grow_dl            = isolate(input$b_q_grow_dl),
      ni_cost_dl           = isolate(input$b_ni_cost_dl),
      capex_ps2            = isolate(input$b_capex_ps2),
      capex_ps3            = isolate(input$b_capex_ps3),
      capex_ps4            = isolate(input$b_capex_ps4),
      opex_ps2             = isolate(input$b_opex_ps2),
      opex_ps3             = isolate(input$b_opex_ps3),
      opex_ps4             = isolate(input$b_opex_ps4),
      sens_lower           = isolate(input$b_sens_lower),
      sens_upper           = isolate(input$b_sens_upper)
    )
  }, ignoreInit  =  TRUE
  )
  
  
  argsB <- reactive({
    
    inp <- simB_inputs()
    req(inp)
    
    expand.grid(
      q_grow               = inp$q_grow,
      cost_of_debt_nmnl    = inp$cost_of_debt_nmnl,
      fcast_infltn         = inp$fcast_infltn,
      roe                  = inp$roe,
      single_price_delta   = inp$single_price_delta,
      desired_fixed        = inp$desired_fixed,
      debt_sens            = list(NULL), 
      fte                  = if (inp$sens_param == "Labour quantity") c(inp$sens_lower, inp$fte,      inp$sens_upper) else inp$fte,
      cost_fte             = if (inp$sens_param == "Labour price")    c(inp$sens_lower, inp$cost_fte, inp$sens_upper) else inp$cost_fte,
      q_grow_fte           = if (inp$sens_param == "Labour growth")   c(inp$sens_lower, inp$cost_fte, inp$sens_upper) else inp$cost_fte,
      ni_cost_fte          = inp$ni_cost_fte,
      kwh                  = inp$kwh,
      cost_kwh             = inp$cost_kwh,
      q_grow_kwh           = inp$q_grow_kwh,
      ni_cost_kwh          = inp$ni_cost_kwh,
      ml                   = inp$ml,
      cost_ml              = inp$cost_ml,
      q_grow_ml            = inp$q_grow_ml,
      ni_cost_ml           = inp$ni_cost_ml,
      dl                   = inp$dl,
      cost_dl              = inp$cost_dl,
      q_grow_dl            = inp$q_grow_dl,
      ni_cost_dl           = inp$ni_cost_dl,
      capex_ps2            = inp$capex_ps2,
      capex_ps3            = inp$capex_ps3,
      capex_ps4            = inp$capex_ps4,
      opex_ps2             = inp$opex_ps2,
      opex_ps3             = inp$opex_ps3,
      opex_ps4             = inp$opex_ps4
      
    )
  })
  
  
  simB <- reactive({
    
    args_df <- argsB()
    req(nrow(args_df) > 0)
    
    mapply(
      FUN               = f, 
      dat=list(dat_df), chart=list(chart_df), txn_type=list(txn_df), ref=list(ref_df), cx_delta=list(cx_df), ox_delta=list(ox_df),
      q_grow            = args_df$q_grow,
      cost_of_debt_nmnl = args_df$cost_of_debt_nmnl/100, 
      fcast_infltn      = args_df$fcast_infltn/100,
      roe               = args_df$roe/100, 
      single_price_delta= args_df$single_price_delta,
      desired_fixed     = args_df$desired_fixed,
      debt_sens         = args_df$debt_sens, 
      fte               = args_df$fte,
      cost_fte          = args_df$cost_fte,
      q_grow_fte        = args_df$q_grow_fte/100,
      ni_cost_fte       = args_df$ni_cost_fte/100,
      kwh               = args_df$kwh,
      cost_kwh          = args_df$cost_kwh,
      q_grow_kwh        = args_df$q_grow_kwh/100,
      ni_cost_kwh       = args_df$ni_cost_kwh/100,
      ml                = args_df$ml,
      cost_ml           = args_df$cost_ml,
      q_grow_ml         = args_df$q_grow_ml/100,
      ni_cost_ml        = args_df$ni_cost_ml/100,
      dl                = args_df$dl,
      cost_dl           = args_df$cost_dl,
      q_grow_dl         = args_df$q_grow_dl/100,
      ni_cost_dl        = args_df$ni_cost_dl/100,
      capex_ps2         = args_df$capex_ps2,
      capex_ps3         = args_df$capex_ps3,
      capex_ps4         = args_df$capex_ps4,
      opex_ps2          = args_df$opex_ps2,
      opex_ps3          = args_df$opex_ps3,
      opex_ps4          = args_df$opex_ps4,
      verbose           = F,
      SIMPLIFY          = FALSE
    )
    
  })
  
  
  #output$a_plot        <- renderPlot( {str( args())} )
  output$a_plot        <- renderPlot({
    req(simA()) 
    plot_kpi( simA(), initial_fcast_yr )
    })
  #output$b_plot        <- renderPlot( {plot_kpi( simB(), initial_fcast_yr )} )
  output$b_plot        <- renderPlot({
    req(simB()) 
    plot_kpi( simB(), initial_fcast_yr )
  })
  output$fins_kable    <- renderText( {plot_fins(sim=simA(), chart=chart_df, ref=ref_df, sel=input$fy_select)} )
  output$a_tariff_plot <- renderPlot( {plot_tariffs( simA() )} )
  output$b_tariff_plot <- renderPlot( {plot_tariffs( simB() )} )
  output$tb_dload      <- downloadHandler(
    filename = function() {"trial_balance.csv"},
    content = function(file) {write.csv(tb(sim=simA(), chart=chart_df), file, quote = FALSE)}
  )
  output$rab_dload   <- downloadHandler(
    filename = function() {"rab_balance.csv"},
    content = function(file) {write.csv(rab(simA()), file, quote = FALSE)}
  )
  output$revreq_dload   <- downloadHandler(
    filename = function() {"revenue_req.csv"},
    content = function(file) {write.csv(revreq(simA()), file, quote = FALSE)}
  )
  
}



# app.R
shinyApp(ui, server)
