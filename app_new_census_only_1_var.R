library(tidyverse)
library(shiny)
library(shinydashboard)
library(tidycensus)

my_key <- "e267f117801b2ef741e54620602b0903c5f4d3c8"

#create state names with abbreviations
state_abb <- datasets::state.name
names(state_abb) <- datasets::state.abb

# Define UI for application that draws a histogram
ui <- dashboardPage(
      # withMathJax(),
        dashboardHeader(title = "Z-Score Activity", disable = FALSE),
        dashboardSidebar(disable = FALSE,
          sidebarMenu(
            menuItem("Z-Score Exploration", tabName = "first", icon = icon("archive")),
            menuItem("Comparing Two Distributions", tabName = "second", icon = icon("laptop"))
          )
        ),
        dashboardBody(
          tabItems(
            tabItem(tabName = "first",
                    titlePanel("First Exploration Part"),
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput("state",
                                       "State:",
                                       choices = state_abb,
                                       selected = state_abb[1]),
                        radioButtons("adornment", "Find a Probability or Percentile?",
                                     choices = c("No", "Percentile", "Probability"),
                                     selected = "No"),
                        conditionalPanel("input.adornment == 'Percentile'",
                                         numericInput("percentile", "Percentile to find:", value = 0.5, min = 0.001, max = 0.999)),
                        conditionalPanel("input.adornment == 'Probability'",
                                         radioButtons("prob_type", "Which type of Probability to Find?",
                                                      choices = c("Less Than", "Between", "Greater Than"),
                                                      selected = "Less Than"),
                                         conditionalPanel("input.prob_type == 'Less Than'",
                                                          numericInput("less_than", "Upper Value", value = 0)),
                                         conditionalPanel("input.prob_type == 'Between'",
                                                          numericInput("between1", "Lower Value", value = 0),
                                                          numericInput("between2", "Upper Value", value = 1)),
                                         conditionalPanel("input.prob_type == 'Greater Than'",
                                                          numericInput("greater_than", "Lower Value", value = 1))
                        )
                      ),
                      # Show a plot of the generated distribution
                      mainPanel(
                        fluidRow(
                          box(title = "Normal Approximation",
                              collapsible = FALSE, 
                              width = 12,
                              plotOutput("norm_approx")
                              )
                        ),
                        fluidRow(
                          box(
                            
                            title = uiOutput("computation_value"),
                            conditionalPanel("input.adornment == 'Percentile'",
                                           uiOutput("percentile_calc")),
                            conditionalPanel("input.adornment == 'Probability'",
                                           uiOutput("probability_calc")),
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width = 12
                          )
                        )
                      )
                    )
            )
          )
        )
        )

# Define server logic required to draw a histogram
server <- function(session, input, output) {

    #data for 1st tab
    filtered_data <- reactive({
      state <- input$state
      rent <- "DP04_0142PE"
      rent_data <- get_acs(variables = rent, 
                           geography = "tract", #geometry = T returns the polygon data and allows for maps easily
                           geometry = TRUE,
                           survey = "acs5",
                           show_call = TRUE, 
                           state = state,
                           key = my_key)
      #drop 0 values
      rent_data[rent_data$estimate!=0, ]
    })
    #update names on the adornment selection 1st tab
    observeEvent(c(input$adornment, input$state), {
      state <- input$state
      #separate data
      rent_data <- filtered_data()
      
      #means and sds
      mean <- round(mean(rent_data$estimate, na.rm = TRUE), 2)
      sd <- round(sd(rent_data$estimate, na.rm = TRUE), 2)
      quants <- round(qnorm(c(0.25, 0.75), mean = mean, sd = sd), 2)

      updateNumericInput(session,
                         "percentile",
                         label = paste0("Percentile to find for ", state, ":"), value = 0.5)
      updateNumericInput(session,
                         "less_than",
                         label = paste0("Upper value for ", state, ":"), value = mean)
      updateNumericInput(session,
                         "between1",
                         label = paste0("Lower value for ", state, ":"), value = quants[1])
      updateNumericInput(session,
                         "between2",
                         label = paste0("Upper value for ", state, ":"), value = quants[2])
      updateNumericInput(session,
                         "greater_than",
                         label = paste0("Lower value for ", state, ":"), value = mean)
    })

 

    #create normal plot 1st tab
    output$norm_approx <- renderPlot({
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)

      lower <- mean-4*sd
      upper <- mean+4*sd
      x <- seq(lower, upper, length = 1000)
      
      temp_hist <- hist(filtered_data()$estimate, 
                        freq = FALSE)
      largest <- max(temp_hist$density)
      hist(filtered_data()$estimate, 
           freq = FALSE,            
           xlab= "",
           ylab = "Density",
           ylim = c(0, largest+0.1*largest),
           main = paste0("Rent Distribution with \nMean = ", round(mean), " and S.D. = ", round(sd))
      )
      lines(x,
           dnorm(x, mean = mean, sd = sd),
           type = "l"
      )
      axis(1, at = mean+(-4:4)*sd, labels = -4:4, line = 3)
      mtext("Rent and Corresponding Standard Normal Values", side =1, line = 2)
      
      if(input$adornment == "Percentile"){
        per <- qnorm(input$percentile, mean = mean, sd = sd)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      } else if(input$adornment == "Probability"){
        if (input$prob_type == "Less Than"){
          per <- input$less_than
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        } else if (input$prob_type == "Between"){
          per1 <- input$between1
          per2 <- input$between2
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        } else if (input$prob_type == "Greater Than"){
          per <- input$greater_than
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        }
      }


    })

    output$computation_value <- renderUI({
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)
      
      if(input$adornment == "Percentile"){
        #percentile
        paste0("Percentile value = ", round(qnorm(input$percentile)*sd(filtered_data()$estimate, na.rm = TRUE) + mean(filtered_data()$estimate, na.rm = TRUE), 2))
      } else if (input$adornment == "Probability"){
        #prob
        if (input$prob_type == "Less Than"){
        #less than
          paste0("Probability = ", round(pnorm(round((input$less_than - mean)/sd, 2)), 4))
        } else if (input$prob_type == "Between"){
        #between
          paste0("Probability = ", round(pnorm(round((input$between2 - mean)/sd, 2))- pnorm(round((input$between1 - mean)/sd, 2)), 4))
        } else if(input$prob_type == "Greater Than"){
          #greater than
          paste0("Probability = ", round(1- pnorm(round((input$greater_than - mean)/sd, 2)), 4))
        }
      } else {
        "No computation selected."
      }
      
    })
    
    
    #Calculate percentile 1st tab
    output$percentile_calc <- renderUI({
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)

      tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              input$percentile,
              " value from the standard normal is ",
              round(qnorm(input$percentile), 2)
            )),
            tags$li(
              "Converted to the distribution of loan debt we use:",
              tags$br(),
              tags$code("loan value = mean + standard deviation * quantile"),
              tags$br(),
              tags$code(
                paste0(
                  round(qnorm(input$percentile)*sd(filtered_data()$estimate, na.rm = TRUE) + mean(filtered_data()$estimate, na.rm = TRUE), 2),
                  " = ",
                  round(mean, 2), " + ", round(sd, 2), " * ", round(qnorm(input$percentile), 2)
                )
              )
            )
          )
      )
    })

    #calculate probability 1st tab
    output$probability_calc <- renderUI({
      mean <- mean(filtered_data()$estimate, na.rm = TRUE)
      sd <- sd(filtered_data()$estimate, na.rm = TRUE)

      if(input$prob_type == "Less Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of loan debt of ",
              input$less_than,
              " can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$less_than - mean)/sd, 2),
                  " = (",
                  input$less_than, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are less than this value is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y < ", input$less_than, ") = P(Z < ", round((input$less_than - mean)/sd, 2), ") = ", round(pnorm(round((input$less_than - mean)/sd, 2)), 4)))
            )
          )
        )

      } else if(input$prob_type == "Between"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The values of loan debt, ",
              input$between1, " and ", input$between2,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between1 - mean)/sd, 2),
                  " = (",
                  input$between1, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              ),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between2 - mean)/sd, 2),
                  " = (",
                  input$between2, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are between these two values is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(", input$between1, "< Y < ", input$between2, ") = P(Y < ", input$between2, ") - P(Y < ", input$between1, ") = P(Z < ", round((input$between2 - mean)/sd, 2), ") - P(Z < ", round((input$between1 - mean)/sd, 2), ")= ", round(pnorm(round((input$between2 - mean)/sd, 2))- pnorm(round((input$between1 - mean)/sd, 2)), 4)))
            )
          )
        )
      } else if (input$prob_type == "Greater Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of loan debt of ",
              input$greater_than,
              " can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$greater_than - mean)/sd, 2),
                  " = (",
                  input$greater_than, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are greater than this value is then found using the complement rule and the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y > ", input$greater_than, ") = 1-P(Y < ", input$greater_than, ") = 1 - P(Z < ", round((input$greater_than - mean)/sd, 2), ") = ", round(1- pnorm(round((input$greater_than - mean)/sd, 2)), 4)))
            )
          )
        )
      }
    })


}

# Run the application
shinyApp(ui = ui, server = server)
