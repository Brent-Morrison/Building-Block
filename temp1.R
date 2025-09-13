library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("mtcars"),

  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "mpg", "mpg Limit", min = 11, max = 33, value = 20)
    ),

    mainPanel(
      tableOutput("mtcars_kable")
    )
  )
)

server <- function(input, output) {
  library(dplyr)
  library(kableExtra)
  output$mtcars_kable <- function() {
    req(input$mpg)
    mtcars %>%
      mutate(car = rownames(.)) %>%
      select(car, everything()) %>%
      filter(mpg <= input$mpg) %>%
      knitr::kable("html") %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      #kable_styling("striped", full_width = F) %>%
      row_spec(10, bold = TRUE) %>%  #, italic = TRUE
      add_header_above(c(" ", "Group 1" = 5, "Group 2" = 7))
  }
}

# Run the application
shinyApp(ui = ui, server = server)



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