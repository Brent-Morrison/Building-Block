# https://sscc.wisc.edu/shiny/users/jstruck2/layouts/
# https://posit.co/blog/how-to-use-shinymatrix-and-plotly-graphs/
# https://pub.demo.posit.team/public/shinymatrix-app/


# library(shiny)
# library(shinyBS)
# shinyApp(
#   ui =
#     fluidPage(
#       sidebarLayout(
#         sidebarPanel(
#           sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 50, value = 30),
#           bsTooltip(id = "bins", title = "The wait times will be broken into this many equally spaced bins", placement = "bottom")#, options = list(container = "body"))
#         ),
#         mainPanel(
#           plotOutput("distPlot")
#         )
#       )
#     ),
#   server =
#     function(input, output, session) {
#       output$distPlot <- renderPlot({
#         
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
#         
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#         
#       })
#       addPopover(session, id = "distPlot", title = "Data", 
#         content = "Waiting time between eruptions and the
#           duration of the eruption for the Old Faithful geyser 
#           in Yellowstone National Park, Wyoming, USA.  Azzalini 
#           , A. and Bowman, A. W. (1990). A look at some data on 
#           the Old Faithful geyser. Applied Statistics 39, 357-365.", 
#         trigger = 'hover')
#     }
# )




# --------------------------------------------------------------------------------------------------------------------------
# https://posit.co/blog/how-to-use-shinymatrix-and-plotly-graphs/
# https://pub.demo.posit.team/public/shinymatrix-app/

### Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinyMatrix)

### Define default matrix
rateInputs_m <-
  matrix(
    c(0, 10, 15, 26, 29, 39, 70, 0.78, 1.05, 1.21, 0.67, 0.61, 0.67, 0.67),
    nrow = 7,
    ncol = 2,
    dimnames = list(letters[1:7], c("Time", "Speed"))
  )

### Define UI
ui <- fluidPage(
  titlePanel("Plotly and Shiny Matrix Input Demonstration"),
  
  column(
    4,
    radioButtons(
      "toggleInputSelect",
      "Input Method:",
      choices = c("Drag-and-Drop" = "dragDrop", "Hand Typed" =
                    "handTyped")
    ),
    br(),
    conditionalPanel(condition = "input.toggleInputSelect=='dragDrop'",
                     plotlyOutput("speed_p", height = "250px")),
    conditionalPanel(
      condition = "input.toggleInputSelect=='handTyped'",
      matrixInput(
        "rateInputs_mi",
        value = rateInputs_m,
        class = "numeric",
        row = list(names = FALSE)
      )
    )
  ),
  column(8,
         tabsetPanel(
           id = "tabs",
           tabPanel(
             "Algorithm Tab",
             value = "algorithmOutput",
             column(3, br(),
                    tags$h4("Original Values"),
                    tableOutput("table1")),
             column(3, br(),
                    tags$h4("Matix Inputs"),
                    tableOutput("table2")),
             column(3, br(),
                    tags$h4("Reactive Values"),
                    tableOutput("table3"))
           )
         ))
)


### Define server logic
server <- function(input, output, session) {
  output$table1 <- renderTable({
    rateInputs_m
  })
  
  output$table2 <- renderTable({
    input$rateInputs_mi
  })
  
  output$table3 <- renderTable({
    req(rv$time)
    data.frame(rv$time, rv$speed)
    
  })
  
  # Creating Reactive Values
  rv <- reactiveValues(time = rateInputs_m[, 1],
                       speed = rateInputs_m[, 2])
  
  # Speed 1's Plot and Table and Feedback
  output$speed_p <- renderPlotly({
    speed_c <- map2(
      rv$time,
      rv$speed,
      ~ list(
        type = "circle",
        xanchor = .x,
        yanchor = .y,
        x0 = -4,
        x1 = 4,
        y0 = -4,
        y1 = 4,
        xsizemode = "pixel",
        ysizemode = "pixel",
        fillcolor = "grey",
        line = list(color = "black")
      )
    )
    
    
    plot_ly(source = "speed_s") %>%
      add_lines(x = rv$time,
                y = rv$speed,
                color = I("black")) %>%
      layout(
        shapes = speed_c,
        xaxis = list(title = "Time"),
        yaxis = list(title = "Speed"),
        showlegend = FALSE
      ) %>%
      config(edits = list(shapePosition = TRUE),
             displayModeBar = FALSE)
    
  })
  
  
  observeEvent(event_data(event = "plotly_relayout", source = "speed_s"), {
    # Speed 1 Event Data
    speed_ed <- event_data("plotly_relayout", source = "speed_s")
    speed_sa <-
      speed_ed[grepl("^shapes.*anchor$", names(speed_ed))]
    speed_ri <- unique(readr::parse_number(names(speed_sa)) + 1)
    speed_pts <- as.numeric(speed_sa)
    
    # Speed 1 Point Updates
    temp_matrix <- matrix(
      c(round(rv$time, 2), round(rv$speed, 2)),
      nrow = 7,
      ncol = 2,
      dimnames = list(NULL, c("Time", "Speed"))
    )
    temp_matrix[speed_ri, 1] <- round(speed_pts[1], 2)
    temp_matrix[speed_ri, 2] <- round(speed_pts[2], 2)
    temp_matrix <-
      temp_matrix[order(temp_matrix[, 1], decreasing = FALSE), ]
    temp_matrix[1, 1] <- 0
    temp_matrix[7, 1] <- 70
    
    # Update reactive values
    rv$time <- round(temp_matrix[, 1], 2)
    rv$speed <- round(temp_matrix[, 2], 2)
    
    updateMatrixInput(session, "rateInputs_mi", temp_matrix)
    
  })
  
  observeEvent(req(input$rateInputs_mi &
                     input$toggleInputSelect == "handTyped"),
               {
                 temp_matrix <-
                   matrix(
                     input$rateInputs_mi,
                     nrow = 7,
                     ncol = 2,
                     dimnames = list(NULL, c("Time", "Speed"))
                   )
                 temp_matrix[1, 1] <- 0
                 temp_matrix[7, 1] <- 70
                 temp_matrix <-
                   temp_matrix[order(temp_matrix[, 1], decreasing = FALSE), ]
                 
                 rv$time <- temp_matrix[, 1]
                 rv$speed <- temp_matrix[, 2]
                 
                 updateMatrixInput(session, "rateInputs_mi", temp_matrix)
                 
               })
  
}

### Run the application
shinyApp(ui = ui, server = server)




# --------------------------------------------------------------------------------------------------------------------------

# library(shiny)
# #rm(ui) ; rm(server)
# 
# ui <- fluidPage(
#   br(),
#   #sliderInput(inputId = "range", label = "range", min = -3, max = 3, step = 0.5, value = c(-2, 2)),
#   dateRangeInput(
#     inputId = "range1", 
#     label = "Date range:",
#      start  = "2001-01-01",
#      end    = "2010-12-31",
#      min    = "2001-01-01",
#      max    = "2012-12-21",
#      format = "M-yyyy",
#      separator = " to "
#     ),
#   br(),
#   sliderInput(
#     inputId = "range2",
#     label = "Date range 2:",
#     min = as.Date("2021-01-01"),
#     max = as.Date("2021-12-31"),
#     value = c(as.Date("2021-02-02"), as.Date("2021-03-03")),
#     timeFormat = "%Y-%b"
#   ),
#   br(),
#   sliderInput(
#     inputId = "range3",
#     label = "Date range 3:",
#     min = 2024,
#     max = 2043,
#     sep = "", pre = "FY",
#     value = c(2028, 2032)
#   ),
#   br(),
#   numericInput(
#     inputId = "range4",
#     label = "Input 4",
#     value = 2025,
#     min = 2024,
#     max = 2043,
#     step = 1,
#     width = "100px"
#   ),
#   
#   br(),
#   div( verbatimTextOutput("out1"), style = "width: 300px;" ),
#   br(),
#   div( verbatimTextOutput("out2"), style = "width: 300px;" ),
#   br(),
#   div( verbatimTextOutput("out3"), style = "width: 300px;" ),
#   br(),
#   div( verbatimTextOutput("out4"), style = "width: 100px;" )
# 
# )
# 
# server <- function(input, output) { 
#   
#   output$out1 <- renderText({ input$range1 })
#   output$out2 <- renderText({ input$range2 })
#   output$out3 <- renderText({ input$range3 })
#   output$out4 <- renderText({ input$range4 })
#   
# }
# shinyApp(ui, server)



# -------------------------------------------------------------------------------------------------



# library(shiny)
# library(shinyBS)
# 
# ui <- fluidPage(
#   selectInput("input1", "Select input", c("choice1", "choice2")),
#   bsTooltip(id = "input1", 
#             title = "Here is some text with your instructions")
# )
# 
# server <- function(input, output) {
# }
# 
# shinyApp(ui = ui, server = server)




# -------------------------------------------------------------------------------------------------
#https://dreamrs.github.io/shinyWidgets/reference/noUiSliderInput.html

# library( shiny )
# library( shinyWidgets )
# 
# ui <- (
#   fluidPage(
#     # theme="simplex.min.css",
#     # tags$style(
#     #   type="text/css",
#     #   "label {font-size: 12px;}",
#     #   ".recalculating {opacity: 1.0;}"
#     #   ),
#     column(6,
#       splitLayout(
#         noUiSliderInput(
#           inputId = "cpx24", label = "FY2024:",
#           min = 0, max = 250, step = 25, value = 100, 
#           orientation = "vertical", direction = "rtl", color = "#428BCA",
#           width = "100px", height = "300px", format = wNumbFormat(decimals = 0)
#         ),
#         noUiSliderInput(
#           inputId = "cpx25", label = "FY2025:",
#           min = 0, max = 250, step = 25, value = 100,
#           orientation = "vertical", direction = "rtl", color = "#3498db",
#           width = "100px", height = "300px", format = wNumbFormat(decimals = 0)
#         ),
#         noUiSliderInput(
#           inputId = "cpx26", label = "FY2026:",
#           min = 0, max = 250, step = 25, value = 100,
#           orientation = "vertical", direction = "rtl", color = "#000",
#           width = "100px", height = "300px", format = wNumbFormat(decimals = 0)
#         ),
#         noUiSliderInput(
#           inputId = "cpx27", label = "FY2027:",
#           min = 0, max = 250, step = 25, value = 100, 
#           orientation = "vertical", direction = "rtl", color = "#ff0",
#           width = "100px", height = "300px", format = wNumbFormat(decimals = 0)
#         ),
#         noUiSliderInput(
#           inputId = "cpx28", label = "FY2028:",
#           min = 0, max = 250, step = 25, value = 100, 
#           orientation = "vertical", direction = "rtl",
#           width = "100px", height = "300px", format = wNumbFormat(decimals = 0)
#         )
#       ),
#       splitLayout(
#         verbatimTextOutput(outputId = "res1"),
#         verbatimTextOutput(outputId = "res2"),
#         verbatimTextOutput(outputId = "res3"),
#         verbatimTextOutput(outputId = "res4"),
#         verbatimTextOutput(outputId = "res5")
#     )
#     )
#   )
# )
# 
# server <- function(input, output, session) {
# 
#   output$res1 <- renderPrint(input$cpx24)
#   output$res2 <- renderPrint(input$cpx25)
#   output$res3 <- renderPrint(input$cpx26)
#   output$res4 <- renderPrint(input$cpx27)
#   output$res5 <- renderPrint(input$cpx28)
# 
# }
# 
# shinyApp(ui, server)



# -------------------------------------------------------------------------------------------------





# library(shiny)
# 
# ui <- fluidPage(
# 
#   # Application title
#   titlePanel("mtcars"),
# 
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput(
#         "mpg", "mpg Limit", min = 11, max = 33, value = 20)
#     ),
# 
#     mainPanel(
#       tableOutput("mtcars_kable")
#     )
#   )
# )
# 
# server <- function(input, output) {
#   library(dplyr)
#   library(kableExtra)
#   output$mtcars_kable <- function() {
#     req(input$mpg)
#     mtcars %>%
#       mutate(car = rownames(.)) %>%
#       select(car, everything()) %>%
#       filter(mpg <= input$mpg) %>%
#       knitr::kable("html") %>%
#       kable_classic(full_width = F, html_font = "Cambria") %>%
#       #kable_styling("striped", full_width = F) %>%
#       row_spec(10, bold = TRUE) %>%  #, italic = TRUE
#       add_header_above(c(" ", "Group 1" = 5, "Group 2" = 7))
#   }
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)



# --------------------------------------------------------------------------------------------------------------------------


# library(shiny)
# 
# monthFeedbackUI <- function(id) {
#   textOutput(NS(id, "feedback"))
# }
# monthFeedbackServer <- function(id, month) {
#   stopifnot(is.reactive(month))
#   
#   moduleServer(id, function(input, output, session) {
#     output$feedback <- renderText({
#       if (month() == "October") {
#         "You picked a great month!"
#       } else {
#         "Eh, you could do better."
#       }
#     })
#   })
# }
# 
# stones <- vroom::vroom("birthstones.csv")
# birthstoneUI <- function(id) {
#   p(
#     "The birthstone for ", textOutput(NS(id, "month"), inline = TRUE),
#     " is ", textOutput(NS(id, "stone"), inline = TRUE)
#   )
# }
# birthstoneServer <- function(id, month) {
#   stopifnot(is.reactive(month))
#   
#   moduleServer(id, function(input, output, session) {
#     stone <- reactive(stones$stone[stones$month == month()])
#     output$month <- renderText(month())
#     output$stone <- renderText(stone())
#   })
# }
# 
# months <- c(
#   "January", "February", "March", "April", "May", "June",
#   "July", "August", "September", "October", "November", "December"
# )
# ui <- navbarPage(
#   "Sample app",
#   tabPanel("Pick a month",
#            selectInput("month", "What's your favourite month?", choices = months)
#   ),
#   tabPanel("Feedback", monthFeedbackUI("tab1")),
#   tabPanel("Birthstone", birthstoneUI("tab2"))
# )
# server <- function(input, output, session) {
#   monthFeedbackServer("tab1", reactive(input$month))
#   birthstoneServer("tab2", reactive(input$month))
# }
# shinyApp(ui, server)