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
                    titlePanel("Z-Score Exploration"),
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
                              plotOutput("norm_approx"),
                              leaflet::leafletOutput("map_plot")
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
            ),
            tabItem(tabName = "second",
                    titlePanel("Comparing Two Distributions"),
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput("state1",
                                       "First State:",
                                       choices = state_abb,
                                       selected = state_abb[1]),
                        selectizeInput("state2",
                                       "Second State:",
                                       choices = state_abb,
                                       selected = state_abb[2]),
                        fluidRow(
                          column(6,
                                 radioButtons("adornment_comp_1st", "Find a Probability or Percentile?",
                                              choices = c("No", "Percentile", "Probability"),
                                              selected = "No"),
                                 conditionalPanel("input.adornment_comp_1st == 'Percentile'",
                                                  numericInput("percentile_comp_1st", "Percentile to find:", value = 0.5, min = 0.001, max = 0.999)),
                                 conditionalPanel("input.adornment_comp_1st == 'Probability'",
                                                  radioButtons("prob_type_comp_1st", "Which type of Probability to Find?",
                                                               choices = c("Less Than", "Between", "Greater Than"),
                                                               selected = "Less Than"),
                                                  conditionalPanel("input.prob_type_comp_1st == 'Less Than'",
                                                                   numericInput("less_than_comp_1st", "Upper Value", value = 0)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_1st == 'Between'",
                                                                   numericInput("between1_comp_1st", "Lower Value", value = 0),
                                                                   numericInput("between2_comp_1st", "Upper Value", value = 1)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_1st == 'Greater Than'",
                                                                   numericInput("greater_than_comp_1st", "Lower Value", value = 1)
                                                  )
                                 )
                          ),
                          column(6,
                                 radioButtons("adornment_comp_2nd", "Find a Probability or Percentile?",
                                              choices = c("No", "Percentile", "Probability"),
                                              selected = "No"),
                                 conditionalPanel("input.adornment_comp_2nd == 'Percentile'",
                                                  numericInput("percentile_comp_2nd", "Percentile to find:", value = 0.5, min = 0.001, max = 0.999)),
                                 conditionalPanel("input.adornment_comp_2nd == 'Probability'",
                                                  radioButtons("prob_type_comp_2nd", "Which type of Probability to Find?",
                                                               choices = c("Less Than", "Between", "Greater Than"),
                                                               selected = "Less Than"),
                                                  conditionalPanel("input.prob_type_comp_2nd == 'Less Than'",
                                                                   numericInput("less_than_comp_2nd", "Upper Value", value = 0)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_2nd == 'Between'",
                                                                   numericInput("between1_comp_2nd", "Lower Value", value = 0),
                                                                   numericInput("between2_comp_2nd", "Upper Value", value = 1)
                                                  ),
                                                  conditionalPanel("input.prob_type_comp_2nd == 'Greater Than'",
                                                                   numericInput("greater_than_comp_2nd", "Lower Value", value = 1)
                                                  )
                                 )
                          )
                        )
                      ),
                      # Show a plot of the generated distribution
                      mainPanel(
                        fluidRow(
                          box(title = "Normal Approximation",
                              collapsible = FALSE, 
                              width = 6,
                              plotOutput("norm_approx_1st"),
                              box(
                                title = uiOutput("computation_value_1st"),
                                conditionalPanel("input.adornment_comp_1st == 'Percentile'",
                                                 uiOutput("percentile_calc_comp_1st")
                                ),
                                conditionalPanel("input.adornment_comp_1st == 'Probability'",
                                                 uiOutput("probability_calc_comp_1st")
                                ),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                width = 12
                              )
                          ),
                          box(title = "Normal Approximation",
                              collapsible = FALSE, 
                              width = 6,
                              plotOutput("norm_approx_2nd"),
                              box(
                                title = uiOutput("computation_value_2nd"),
                                conditionalPanel("input.adornment_comp_2nd == 'Percentile'",
                                                 uiOutput("percentile_calc_comp_2nd")
                                ),
                                conditionalPanel("input.adornment_comp_2nd == 'Probability'",
                                                 uiOutput("probability_calc_comp_2nd")
                                ),
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
        )

# Define server logic required to draw a histogram
server <- function(session, input, output) {

  
  
    #################################################################################
    #First tab part
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
           main = paste0("Rent Distribution with \nMean = ", round(mean, 2), " and S.D. = ", round(sd, 2))
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

    
    #create map on 1st tab
    output$map_plot <- leaflet::renderLeaflet({
      my_map <- filtered_data() |> mapview::mapview(zcol = "estimate", layer.name = "Median rent as a % of gross income")
      my_map@map
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
              "Converted to the distribution of rent we use:",
              tags$br(),
              tags$code("rent value = mean + standard deviation * quantile"),
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
              "The value of rent, ",
              input$less_than,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
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
              "The values of rent, ",
              input$between1, " and ", input$between2,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
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
              "The value of rent, ",
              input$greater_than,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
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

    #################################################################################
    #Second tab part
    
    #data for 2nd tab
    filtered_data_2nd <- reactive({
      state1 <- input$state1
      state2 <- input$state2

      rent <- "DP04_0142PE"
      rent_data1 <- get_acs(variables = rent, 
                           geography = "tract", #geometry = T returns the polygon data and allows for maps easily
                           #geometry = TRUE,
                           survey = "acs5",
                           show_call = TRUE, 
                           state = state1,
                           key = my_key)
      #drop 0 values
      rent_data1 <- rent_data1[rent_data1$estimate!=0, ]
      
      rent_data2 <- get_acs(variables = rent, 
                            geography = "tract", #geometry = T returns the polygon data and allows for maps easily
                            #geometry = TRUE,
                            survey = "acs5",
                            show_call = TRUE, 
                            state = state2,
                            key = my_key)
      #drop 0 values
      rent_data2 <- rent_data2[rent_data2$estimate!=0, ]
      
      #combine
      rent_data1$state <- state1
      rent_data2$state <- state2
      rbind(rent_data1, rent_data2)
    })  
    
    #update the choices for state on the 2nd tab
    observeEvent(c(input$state1, input$state2), {
      state1 <- input$state1
      state2 <- input$state2
      choices <- state_abb
      if (state1 == state2){
        choices <- choices[-which(choices == state1)]
        updateSelectizeInput(session,
                             "state2",
                             choices = choices)
      }
    })
    
    
    #update names on the adornment selectiosn 2nd tab
    observeEvent(c(input$state1, input$state2),{
      updateRadioButtons(session,
                         "adornment_comp_1st",
                         paste0("Find a Probability or Percentile for ", input$state1, "?"))
      updateRadioButtons(session,
                         "adornment_comp_2nd",
                         paste0("Find a Probability or Percentile for ", input$state2, "?"))
    })
    
    #update input names for 2nd tab
    observeEvent(c(input$adornment_comp_1st, input$adornment_comp_2nd, input$state1, input$state2), {
      state1 <- input$state1
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean1 <- round(mean(state1_data$estimate, na.rm = TRUE), 2)
      sd1 <- round(sd(state1_data$estimate, na.rm = TRUE), 2)
      mean2 <- round(mean(state2_data$estimate, na.rm = TRUE), 2)
      sd2 <- round(sd(state2_data$estimate, na.rm = TRUE), 2)
      quants1 <- round(qnorm(c(0.25, 0.75), mean = mean1, sd = sd1), 2)
      quants2 <- round(qnorm(c(0.25, 0.75), mean = mean2, sd = sd2), 2)
      
      updateNumericInput(session,
                         "percentile_comp_1st",
                         label = paste0("Percentile to find for ", state1, ":"), value = 0.5)
      updateNumericInput(session,
                         "percentile_comp_2nd",
                         label = paste0("Percentile to find for ", state2, ":"), value = 0.5)
      updateNumericInput(session,
                         "less_than_comp_1st",
                         label = paste0("Upper value for ", state1, ":"), value = mean1)
      updateNumericInput(session,
                         "less_than_comp_2nd",
                         label = paste0("Upper value for ", state2, ":"), value = mean2)
      updateNumericInput(session,
                         "between1_comp_1st",
                         label = paste0("Lower value for ", state1, ":"), value = quants1[1])
      updateNumericInput(session,
                         "between2_comp_1st",
                         label = paste0("Upper value for ", state1, ":"), value = quants1[2])
      updateNumericInput(session,
                         "between1_comp_2nd",
                         label = paste0("Lower value for ", state2, ":"), value = quants2[1])
      updateNumericInput(session,
                         "between2_comp_2nd",
                         label = paste0("Upper value for ", state2, ":"), value = quants2[2])
      updateNumericInput(session,
                         "greater_than_comp_1st",
                         label = paste0("Lower value for ", state1, ":"), value = mean1)
      updateNumericInput(session,
                         "greater_than_comp_2nd",
                         label = paste0("Lower value for ", state2, ":"), value = mean2)
    })
    

    #Create plot for 1st state, 2nd tab
    output$norm_approx_1st <- renderPlot({
      state1 <- input$state1
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean1 <- mean(state1_data$estimate, na.rm = TRUE)
      sd1 <- sd(state1_data$estimate, na.rm = TRUE)
      mean2 <- mean(state2_data$estimate, na.rm = TRUE)
      sd2 <- sd(state2_data$estimate, na.rm = TRUE)
      
      #plotting sequence
      lower <- min(mean1-4*sd1, mean2-4*sd2)
      upper <- max(mean1+4*sd1, mean2+4*sd2)
      x <- seq(lower, upper, length = 5000)
      
      #temp histogram to update max ylim
      temp_hist <- hist(state1_data$estimate, 
                        freq = FALSE)
      largest <- max(temp_hist$density)
      hist(state1_data$estimate, 
           freq = FALSE,            
           xlab= "",
           ylab = "Density",
           ylim = c(0, largest+0.1*largest),
           main = paste0("Rent Distribution for ", input$state1, "\nMean = ", round(mean1, 2), " and S.D. = ", round(sd1, 2))
      )
      lines(x,
            dnorm(x, mean = mean1, sd = sd1),
            type = "l"
      )
      axis(1, at = mean1+(-4:4)*sd1, labels = -4:4, line = 3)
      mtext("Rent and Corresponding Standard Normal Values", side =1, line = 2)
      
      mean <- mean1
      sd <- sd1
      if(input$adornment_comp_1st == "Percentile"){
        per <- qnorm(input$percentile_comp_1st, mean = mean, sd = sd)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      } else if(input$adornment_comp_1st == "Probability"){
        if (input$prob_type_comp_1st == "Less Than"){
          per <- input$less_than_comp_1st
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        } else if (input$prob_type_comp_1st == "Between"){
          per1 <- input$between1_comp_1st
          per2 <- input$between2_comp_1st
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        } else if (input$prob_type_comp_1st == "Greater Than"){
          per <- input$greater_than_comp_1st
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        }
      }
    })
    
    #create plot for 2nd school, 2nd tab
    output$norm_approx_2nd <- renderPlot({
      state1 <- input$state1
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean1 <- mean(state1_data$estimate, na.rm = TRUE)
      sd1 <- sd(state1_data$estimate, na.rm = TRUE)
      mean2 <- mean(state2_data$estimate, na.rm = TRUE)
      sd2 <- sd(state2_data$estimate, na.rm = TRUE)
      
      #plotting sequence
      lower <- min(mean1-4*sd1, mean2-4*sd2)
      upper <- max(mean1+4*sd1, mean2+4*sd2)
      x <- seq(lower, upper, length = 5000)
      
      #temp histogram to update max ylim
      temp_hist <- hist(state2_data$estimate, 
                        freq = FALSE)
      largest <- max(temp_hist$density)
      hist(state2_data$estimate, 
           freq = FALSE,            
           xlab= "",
           ylab = "Density",
           ylim = c(0, largest+0.1*largest),
           main = paste0("Rent Distribution for ", input$state2, "\nMean = ", round(mean2, 2), " and S.D. = ", round(sd2, 2))
      )
      lines(x,
            dnorm(x, mean = mean2, sd = sd2),
            type = "l"
      )
      axis(1, at = mean2+(-4:4)*sd2, labels = -4:4, line = 3)
      mtext("Rent and Corresponding Standard Normal Values", side =1, line = 2)
      
      mean <- mean2
      sd <- sd2
      if(input$adornment_comp_2nd == "Percentile"){
        per <- qnorm(input$percentile_comp_2nd, mean = mean, sd = sd)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
      } else if(input$adornment_comp_2nd == "Probability"){
        if (input$prob_type_comp_2nd == "Less Than"){
          per <- input$less_than_comp_2nd
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        } else if (input$prob_type_comp_2nd == "Between"){
          per1 <- input$between1_comp_2nd
          per2 <- input$between2_comp_2nd
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        } else if (input$prob_type_comp_2nd == "Greater Than"){
          per <- input$greater_than_comp_2nd
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = rgb(red = 0.34, blue = 0.139, green = 0.34, alpha = 0.5))
        }
      }
    })
    
    #calculate percentile for 1st plot, 2nd tab
    output$percentile_calc_comp_1st <- renderUI({
      state1 <- input$state1
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      
      #means and sds
      mean <- mean(state1_data$estimate, na.rm = TRUE)
      sd <- sd(state1_data$estimate, na.rm = TRUE)
      
      tags$div(
        "Calculation:",
        tags$br(),
        tags$ul(
          tags$li(paste0(
            input$percentile_comp_1st,
            " value from the standard normal is ",
            round(qnorm(input$percentile_comp_1st), 2)
          )),
          tags$li(
            "Converted to the distribution of rent we use:",
            tags$br(),
            tags$code("rent value = mean + standard deviation * quantile"),
            tags$br(),
            tags$code(
              paste0(
                round(qnorm(input$percentile_comp_1st)*sd + mean, 2),
                " = ",
                round(mean, 2), " + ", round(sd, 2), " * ", round(qnorm(input$percentile_comp_1st), 2)
              )
            )
          )
        )
      )
    })
    
    #calculate percentile for 2nd plot, 2nd tab
    output$percentile_calc_comp_2nd <- renderUI({
      state2 <- input$state2
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean <- mean(state1_data$estimate, na.rm = TRUE)
      sd <- sd(state1_data$estimate, na.rm = TRUE)
      
      tags$div(
        "Calculation:",
        tags$br(),
        tags$ul(
          tags$li(paste0(
            input$percentile_comp_2nd,
            " value from the standard normal is ",
            round(qnorm(input$percentile_comp_2nd), 2)
          )),
          tags$li(
            "Converted to the distribution of rent we use:",
            tags$br(),
            tags$code("rent value = mean + standard deviation * quantile"),
            tags$br(),
            tags$code(
              paste0(
                round(qnorm(input$percentile_comp_2nd)*sd + mean, 2),
                " = ",
                round(mean, 2), " + ", round(sd, 2), " * ", round(qnorm(input$percentile_comp_2nd), 2)
              )
            )
          )
        )
      )
    })
    
    #calculate probability for 1st plot, 2nd tab
    output$probability_calc_comp_1st <- renderUI({
      state1 <- input$state1
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      
      #means and sds
      mean <- mean(state1_data$estimate, na.rm = TRUE)
      sd <- sd(state1_data$estimate, na.rm = TRUE)
      
      if(input$prob_type_comp_1st == "Less Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$less_than_comp_1st,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$less_than_comp_1st - mean)/sd, 2),
                  " = (",
                  input$less_than_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are less than this value is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y < ", input$less_than_comp_1st, ") = P(Z < ", round((input$less_than_comp_1st - mean)/sd, 2), ") = ", round(pnorm(round((input$less_than_comp_1st - mean)/sd, 2)), 4)))
            )
          )
        )
        
      } else if(input$prob_type_comp_1st == "Between"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The values of rent, ",
              input$between1_comp_1st, " and ", input$between2_comp_1st,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between1_comp_1st - mean)/sd, 2),
                  " = (",
                  input$between1_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              ),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between2_comp_1st - mean)/sd, 2),
                  " = (",
                  input$between2_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are between these two values is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(", input$between1_comp_1st, "< Y < ", input$between2_comp_1st, ") = P(Y < ", input$between2_comp_1st, ") - P(Y < ", input$between1_comp_1st, ") = P(Z < ", round((input$between2_comp_1st - mean)/sd, 2), ") - P(Z < ", round((input$between1_comp_1st - mean)/sd, 2), ")= ", round(pnorm(round((input$between2_comp_1st - mean)/sd, 2))- pnorm(round((input$between1_comp_1st - mean)/sd, 2)), 4)))
            )
          )
        )
      } else if (input$prob_type_comp_1st == "Greater Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$greater_than_comp_1st,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$greater_than_comp_1st - mean)/sd, 2),
                  " = (",
                  input$greater_than_comp_1st, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are greater than this value is then found using the complement rule and the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y > ", input$greater_than_comp_1st, ") = 1-P(Y < ", input$greater_than_comp_1st, ") = 1 - P(Z < ", round((input$greater_than_comp_1st - mean)/sd, 2), ") = ", round(1- pnorm(round((input$greater_than_comp_1st - mean)/sd, 2)), 4)))
            )
          )
        )
      }
    })
    
    #calculate probability for 2nd plot, 2nd tab
    output$probability_calc_comp_2nd <- renderUI({
      state2 <- input$state2
      #separate data
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      #means and sds
      mean <- mean(state2_data$estimate, na.rm = TRUE)
      sd <- sd(state2_data$estimate, na.rm = TRUE)
      
      if(input$prob_type_comp_2nd == "Less Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$less_than_comp_2nd,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$less_than_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$less_than_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are less than this value is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y < ", input$less_than_comp_2nd, ") = P(Z < ", round((input$less_than_comp_2nd - mean)/sd, 2), ") = ", round(pnorm(round((input$less_than_comp_2nd - mean)/sd, 2)), 4)))
            )
          )
        )
        
      } else if(input$prob_type_comp_2nd == "Between"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The values of rent, ",
              input$between1_comp_2nd, " and ", input$between2_comp_2nd,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between1_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$between1_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              ),
              tags$br(),
              tags$code(
                paste0(
                  round((input$between2_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$between2_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are between these two values is then found using the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(", input$between1_comp_2nd, "< Y < ", input$between2_comp_2nd, ") = P(Y < ", input$between2_comp_2nd, ") - P(Y < ", input$between1_comp_2nd, ") = P(Z < ", round((input$between2_comp_2nd - mean)/sd, 2), ") - P(Z < ", round((input$between1_comp_2nd - mean)/sd, 2), ")= ", round(pnorm(round((input$between2_comp_2nd - mean)/sd, 2))- pnorm(round((input$between1_comp_2nd - mean)/sd, 2)), 4)))
            )
          )
        )
      } else if (input$prob_type_comp_2nd == "Greater Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of rent, ",
              input$greater_than_comp_2nd,
              ", can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (rent - mean)/(standard deviation"),
              tags$br(),
              tags$code(
                paste0(
                  round((input$greater_than_comp_2nd - mean)/sd, 2),
                  " = (",
                  input$greater_than_comp_2nd, " - ", round(mean, 2), ")/(", round(sd, 2), ")"
                )
              )
            ),
            tags$li(
              "The probability we are greater than this value is then found using the complement rule and the standard normal distribution:",
              tags$br(),
              tags$code(paste0("P(Y > ", input$greater_than_comp_2nd, ") = 1-P(Y < ", input$greater_than_comp_2nd, ") = 1 - P(Z < ", round((input$greater_than_comp_2nd - mean)/sd, 2), ") = ", round(1- pnorm(round((input$greater_than_comp_2nd - mean)/sd, 2)), 4)))
            )
          )
        )
      }
    })
    
    output$computation_value_1st <- renderUI({
      state1 <- input$state1
      #separate data
      state1_data <- filtered_data_2nd() %>%
        filter(state == state1)
      
      mean <- mean(state1_data$estimate, na.rm = TRUE)
      sd <- sd(state1_data$estimate, na.rm = TRUE)
      
      if(input$adornment_comp_1st == "Percentile"){
        #percentile
        paste0("Percentile value = ", round(qnorm(input$percentile_comp_1st)*sd + mean, 2))
      } else if (input$adornment_comp_1st == "Probability"){
        #prob
        if (input$prob_type_comp_1st == "Less Than"){
          #less than
          paste0("Probability = ", round(pnorm(round((input$less_than_comp_1st - mean)/sd, 2)), 4))
        } else if (input$prob_type_comp_1st == "Between"){
          #between
          paste0("Probability = ", round(pnorm(round((input$between2_comp_1st - mean)/sd, 2))- pnorm(round((input$between1_comp_1st - mean)/sd, 2)), 4))
        } else if(input$prob_type_comp_1st == "Greater Than"){
          #greater than
          paste0("Probability = ", round(1- pnorm(round((input$greater_than_comp_1st - mean)/sd, 2)), 4))
        }
      } else {
        "No computation selected."
      }
      
    })
    
    output$computation_value_2nd <- renderUI({
      state2 <- input$state2
      #separate data
      state2_data <- filtered_data_2nd() %>%
        filter(state == state2)
      
      mean <- mean(state2_data$estimate, na.rm = TRUE)
      sd <- sd(state2_data$estimate, na.rm = TRUE)
      
      if(input$adornment_comp_2nd == "Percentile"){
        #percentile
        paste0("Percentile value = ", round(qnorm(input$percentile_comp_2nd)*sd + mean, 2))
      } else if (input$adornment_comp_2nd == "Probability"){
        #prob
        if (input$prob_type_comp_2nd == "Less Than"){
          #less than
          paste0("Probability = ", round(pnorm(round((input$less_than_comp_2nd - mean)/sd, 2)), 4))
        } else if (input$prob_type_comp_2nd == "Between"){
          #between
          paste0("Probability = ", round(pnorm(round((input$between2_comp_2nd - mean)/sd, 2))- pnorm(round((input$between1_comp_2nd - mean)/sd, 2)), 4))
        } else if(input$prob_type_comp_2nd == "Greater Than"){
          #greater than
          paste0("Probability = ", round(1- pnorm(round((input$greater_than_comp_2nd - mean)/sd, 2)), 4))
        }
      } else {
        "No computation selected."
      }
      
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
