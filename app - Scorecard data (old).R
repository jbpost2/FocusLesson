#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readr)
library(dplyr)
library(ggplot2)

ed_data <- read_csv("C:/US_Department_of_Education_College_Scorecard_2015-2016.csv")
ed_data$Institution_Type <- factor(ed_data$Institution_Type, labels = c("Public", "Private nonprofit", "Private for-profit"))
# ed_data %>%
#   group_by(Institution_Type) %>%
#   filter(MedianDebtforCompletedStudents != 999999999) %>%
#   summarize(mean_med = mean(MedianDebtforCompletedStudents),
#             sd_med = sd(MedianDebtforCompletedStudents))
#
# #ok get those values I suppose
# ed_data <- ed_data %>%
#   filter(MedianDebtforCompletedStudents != 999999999)
#
# ed_data %>%
#   filter(Institution_Type == "Public") %>%
#   ggplot(aes(x = MedianDebtforCompletedStudents)) +
#   geom_histogram()
#
# ed_data %>%
#   filter(Institution_Type == "Private nonprofit") %>%
#   ggplot(aes(x = MedianDebtforCompletedStudents)) +
#   geom_histogram()
#
# ed_data %>%
#   filter(Institution_Type == "Private for-profit") %>%
#   ggplot(aes(x = MedianDebtforCompletedStudents)) +
#   geom_histogram()







# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Focus Lesson Draft App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectizeInput("school",
                        "Institution:",
                        choices = ed_data$Institution_Name,
                        selected = "North Carolina State University at Raleigh"),
            radioButtons("inst", "Summarize by Institution Type?", selected = "No", choices = c("Yes", "No")),
            selectizeInput("variable",
                           "Variable to Investigate",
                           choices = colnames(ed_data),
                           selected = "MedianDebtforCompletedStudents")
        ),

        # Show a plot of the generated distribution
        mainPanel(
#          column(6,
#           plotOutput("debt_normal_dist")
#          ),
#          column(6,
#                 plotOutput("debt_normal_dist_compare"))
           plotOutput("hists")
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$debt_normal_dist <- renderPlot({
        # generate bins based on input$bins from ui.R
       mu <- ed_data %>%
         filter(Institution_Name == input$school) %>%
         pull(MedianDebtforCompletedStudents)
       sd <- mu/10

       debt <- seq(mu - 4*sd, to = mu + 4*sd, length = 10000)
       plot(debt, dnorm(debt, mean = mu, sd = sd),
            main = "Approximate Distribution of Debt",
            xlab = "Debt", ylab = "PDF", type = "l", lwd = 2)
    })

    output$debt_normal_dist_compare <- renderPlot({
      # generate bins based on input$bins from ui.R
      mu <- ed_data %>%
        filter(Institution_Name == input$school) %>%
        pull(MedianDebtforCompletedStudents)
      sd <- mu/10

      debt <- seq(mu - 4*sd, to = mu + 4*sd, length = 10000)
      plot(debt, dnorm(debt, mean = mu, sd = sd),
           main = "Approximate Distribution of Debt",
           xlab = "Debt", ylab = "PDF", type = "l", lwd = 2)
    })

    output$hists <- renderPlot({
      school <- input$school
      group <- input$inst
      var <- input$variable
      plot_data <- ed_data %>%
        filter(!!as.symbol(var) != 999999999)

      if(group == "Yes"){
        g <- ggplot(plot_data, aes_string(x = var, y = "..density..")) +
          geom_histogram() +
          geom_density(lwd = 2, color = "blue") +
          facet_wrap(~Institution_Type)
      } else {
        g <- ggplot(plot_data, aes_string(x = var, y = "..density..")) +
          geom_histogram() +
          geom_density(lwd = 2, color = "blue")
      }
      g
    })
}

# Run the application
shinyApp(ui = ui, server = server)
