library(shiny)
library(tidyverse)

load("read_data.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
  withMathJax(),
  tabsetPanel(
    tabPanel("First Exploration Part",
             titlePanel("First Exploration Part"),
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("school",
                                "Institution:",
                                choices = unique(cohorts_institutions$INSTNM),
                                selected = unique(cohorts_institutions$INSTNM)[1]),
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
                   column(6,
                          plotOutput("norm_approx"),
                          conditionalPanel("input.adornment == 'Percentile'",
                                           uiOutput("percentile_calc")
                          ),
                          conditionalPanel("input.adornment == 'Probability'",
                                           uiOutput("probability_calc")
                          )
                   ),
                   column(6,
                          plotOutput("standard_norm_approx"),
                   )
                 )
               )
             )
    ),
    tabPanel("Second Comparison Part",
             titlePanel("Second Comparison Part"),
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("school1",
                                "First Institution:",
                                choices = unique(cohorts_institutions$INSTNM),
                                selected = unique(cohorts_institutions$INSTNM)[1]),
                 selectizeInput("school2",
                                "Second Institution:",
                                choices = unique(cohorts_institutions$INSTNM),
                                 selected = unique(cohorts_institutions$INSTNM)[2]),
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
                   column(6,
                          plotOutput("norm_approx_1st"),
                          conditionalPanel("input.adornment_comp_1st == 'Percentile'",
                                           uiOutput("percentile_calc_comp_1st")
                          ),
                          conditionalPanel("input.adornment_comp_1st == 'Probability'",
                                           uiOutput("probability_calc_comp_1st")
                          )
                   ),
                   column(6,
                          plotOutput("norm_approx_2nd"),
                          conditionalPanel("input.adornment_comp_2nd == 'Percentile'",
                                           uiOutput("percentile_calc_comp_2nd")
                          ),
                          conditionalPanel("input.adornment_comp_2nd == 'Probability'",
                                           uiOutput("probability_calc_comp_2nd")
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
      name_n <- "DEBT_ALL_STGP_ANY_N"
      name_mean <- "DEBT_ALL_STGP_ANY_MEAN"

      cohorts_institutions %>%
        select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, DEBT_ALL_STGP_ANY_N, DEBT_ALL_STGP_ANY_MEAN) %>%
        filter(INSTNM == input$school) %>%
        mutate(num_debt_n = ifelse(get(name_n) == "PrivacySuppressed", NA, as.numeric(get(name_n))),
             num_debt_mean = ifelse(get(name_mean) == "PrivacySuppressed", NA, as.numeric(get(name_mean)))
        )
    })

    #data for 2nd tab
    filtered_data_2nd <- reactive({
      name_n <- "DEBT_ALL_STGP_ANY_N"
      name_mean <- "DEBT_ALL_STGP_ANY_MEAN"

      cohorts_institutions %>%
        select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, DEBT_ALL_STGP_ANY_N, DEBT_ALL_STGP_ANY_MEAN) %>%
        filter(INSTNM %in% c(input$school1, input$school2)) %>%
        mutate(num_debt_n = ifelse(get(name_n) == "PrivacySuppressed", NA, as.numeric(get(name_n))),
               num_debt_mean = ifelse(get(name_mean) == "PrivacySuppressed", NA, as.numeric(get(name_mean)))
        )
    })

    #update the choices for school on the 2nd tab
    observeEvent(input$school1, {
      school1 <- input$school1
      choices <- unique(cohorts_institutions$INSTNM)
      choices <- choices[-which(choices == school1)]
      updateSelectizeInput(session,
                           "school2",
                           choices = choices)
    })

    #update names on the adornment selection 1st tab
    observeEvent(c(input$adornment, input$school), {
      school <- input$school
      #separate data
      school_data <- filtered_data()


      #means and sds
      mean <- round(mean(school_data$num_debt_mean, na.rm = TRUE), 2)
      sd <- round(sd(school_data$num_debt_mean, na.rm = TRUE), 2)
      quants <- round(qnorm(c(0.25, 0.75), mean = mean, sd = sd), 2)

      updateNumericInput(session,
                         "percentile",
                         label = paste0("Percentile to find for ", school, ":"), value = 0.5)
      updateNumericInput(session,
                         "less_than",
                         label = paste0("Upper value for ", school, ":"), value = mean)
      updateNumericInput(session,
                         "between1",
                         label = paste0("Lower value for ", school, ":"), value = quants[1])
      updateNumericInput(session,
                         "between2",
                         label = paste0("Upper value for ", school, ":"), value = quants[2])
      updateNumericInput(session,
                         "greater_than",
                         label = paste0("Lower value for ", school, ":"), value = mean)
    })

    #update names on the adornment selectiosn 2nd tab
    observeEvent(c(input$school1, input$school2),{
      updateRadioButtons(session,
                         "adornment_comp_1st",
                         paste0("Find a Probability or Percentile for ", input$school1, "?"))
      updateRadioButtons(session,
                         "adornment_comp_2nd",
                         paste0("Find a Probability or Percentile for ", input$school2, "?"))
    })

    #update input names for 2nd tab
    observeEvent(c(input$adornment_comp_1st, input$adornment_comp_2nd, input$school1, input$school2), {
      school1 <- input$school1
      school2 <- input$school2
      #separate data
      school1_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school1)
      school2_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school2)

      #means and sds
      mean1 <- round(mean(school1_data$num_debt_mean, na.rm = TRUE), 2)
      sd1 <- round(sd(school1_data$num_debt_mean, na.rm = TRUE), 2)
      mean2 <- round(mean(school2_data$num_debt_mean, na.rm = TRUE), 2)
      sd2 <- round(sd(school2_data$num_debt_mean, na.rm = TRUE), 2)
      quants1 <- round(qnorm(c(0.25, 0.75), mean = mean1, sd = sd1), 2)
      quants2 <- round(qnorm(c(0.25, 0.75), mean = mean2, sd = sd2), 2)

      updateNumericInput(session,
                         "percentile_comp_1st",
                         label = paste0("Percentile to find for ", school1, ":"), value = 0.5)
      updateNumericInput(session,
                         "percentile_comp_2nd",
                         label = paste0("Percentile to find for ", school2, ":"), value = 0.5)
      updateNumericInput(session,
                         "less_than_comp_1st",
                         label = paste0("Upper value for ", school1, ":"), value = mean1)
      updateNumericInput(session,
                         "less_than_comp_2nd",
                         label = paste0("Upper value for ", school2, ":"), value = mean2)
      updateNumericInput(session,
                         "between1_comp_1st",
                         label = paste0("Lower value for ", school1, ":"), value = quants1[1])
      updateNumericInput(session,
                         "between2_comp_1st",
                         label = paste0("Upper value for ", school1, ":"), value = quants1[2])
      updateNumericInput(session,
                         "between1_comp_2nd",
                         label = paste0("Lower value for ", school2, ":"), value = quants2[1])
      updateNumericInput(session,
                         "between2_comp_2nd",
                         label = paste0("Upper value for ", school2, ":"), value = quants2[2])
      updateNumericInput(session,
                         "greater_than_comp_1st",
                         label = paste0("Lower value for ", school1, ":"), value = mean1)
      updateNumericInput(session,
                         "greater_than_comp_2nd",
                         label = paste0("Lower value for ", school2, ":"), value = mean2)
    })

    #create normal plot 1st tab
    output$norm_approx <- renderPlot({
      mean <- mean(filtered_data()$num_debt_mean, na.rm = TRUE)
      sd <- sd(filtered_data()$num_debt_mean, na.rm = TRUE)

      lower <- mean-4*sd
      upper <- mean+4*sd
      x <- seq(lower, upper, length = 1000)

      plot(x,
           dnorm(x, mean = mean, sd = sd),
           xlab= "Loan Debt at Graduation",
           ylab = "Density",
           type = "l",
           main = paste0("Loan Distribution with \nMean = ", round(mean), " and S.D. = ", round(sd))
      )

      if(input$adornment == "Percentile"){
        per <- qnorm(input$percentile, mean = mean, sd = sd)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
      } else if(input$adornment == "Probability"){
        if (input$prob_type == "Less Than"){
          per <- input$less_than
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type == "Between"){
          per1 <- input$between1
          per2 <- input$between2
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type == "Greater Than"){
          per <- input$greater_than
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        }
      }


    })

    #create standard normal plot 1st tab
    output$standard_norm_approx <- renderPlot({

      mean <- mean(filtered_data()$num_debt_mean, na.rm = TRUE)
      sd <- sd(filtered_data()$num_debt_mean, na.rm = TRUE)

      lower <- -4
      upper <- 4
      x <- seq(lower, upper, length = 1000)

      plot(x,
           dnorm(x),
           xlab= "z",
           ylab = "Density",
           type = "l",
           main = "Plot of Standard Normal Distribution")

      if(input$adornment == "Percentile"){
        per <- qnorm(input$percentile)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq), rep(0, length(xseq))), col = "Grey")
      } else if(input$adornment == "Probability"){
        if (input$prob_type == "Less Than"){
          per <- (input$less_than-mean)/sd
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type == "Between"){
          per1 <- (input$between1-mean)/sd
          per2 <- (input$between2-mean)/sd
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type == "Greater Than"){
          per <- (input$greater_than-mean)/sd
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq), rep(0, length(xseq))), col = "Grey")
        }
      }

    })

    #Calculate percentile 1st tab
    output$percentile_calc <- renderUI({
      mean <- mean(filtered_data()$num_debt_mean, na.rm = TRUE)
      sd <- sd(filtered_data()$num_debt_mean, na.rm = TRUE)

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
                  round(qnorm(input$percentile)*sd(filtered_data()$num_debt_mean, na.rm = TRUE) + mean(filtered_data()$num_debt_mean, na.rm = TRUE), 2),
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
      mean <- mean(filtered_data()$num_debt_mean, na.rm = TRUE)
      sd <- sd(filtered_data()$num_debt_mean, na.rm = TRUE)

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

    #Create plot for 1st school, 2nd tab
    output$norm_approx_1st <- renderPlot({
      #separate data
      school1_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school1)
      school2_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school2)

      #means and sds
      mean1 <- mean(school1_data$num_debt_mean, na.rm = TRUE)
      sd1 <- sd(school1_data$num_debt_mean, na.rm = TRUE)
      mean2 <- mean(school2_data$num_debt_mean, na.rm = TRUE)
      sd2 <- sd(school2_data$num_debt_mean, na.rm = TRUE)

      #plotting sequence
      lower <- min(mean1-4*sd1, mean2-4*sd2)
      upper <- max(mean1+4*sd1, mean2+4*sd2)
      x <- seq(lower, upper, length = 5000)

      #base plot
      plot(x,
           dnorm(x, mean = mean1, sd = sd1),
           xlab= "Loan Debt at Graduation",
           ylab = "Density",
           type = "l",
           main = paste0("Loan Distribution for ", input$school1, "\nMean = ", round(mean1), " and S.D. = ", round(sd1))
      )

      mean <- mean1
      sd <- sd1
      if(input$adornment_comp_1st == "Percentile"){
        per <- qnorm(input$percentile_comp_1st, mean = mean, sd = sd)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
      } else if(input$adornment_comp_1st == "Probability"){
        if (input$prob_type_comp_1st == "Less Than"){
          per <- input$less_than_comp_1st
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type_comp_1st == "Between"){
          per1 <- input$between1_comp_1st
          per2 <- input$between2_comp_1st
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type_comp_1st == "Greater Than"){
          per <- input$greater_than_comp_1st
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        }
      }


    })

    #create plot for 2nd school, 2nd tab
    output$norm_approx_2nd <- renderPlot({
      #separate data
      school1_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school1)
      school2_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school2)

      #means and sds
      mean1 <- mean(school1_data$num_debt_mean, na.rm = TRUE)
      sd1 <- sd(school1_data$num_debt_mean, na.rm = TRUE)
      mean2 <- mean(school2_data$num_debt_mean, na.rm = TRUE)
      sd2 <- sd(school2_data$num_debt_mean, na.rm = TRUE)

      #plotting sequence
      lower <- min(mean1-4*sd1, mean2-4*sd2)
      upper <- max(mean1+4*sd1, mean2+4*sd2)
      x <- seq(lower, upper, length = 5000)

      #base plot
      plot(x,
           dnorm(x, mean = mean2, sd = sd2),
           xlab= "Loan Debt at Graduation",
           ylab = "Density",
           type = "l",
           main = paste0("Loan Distribution for ", input$school2, "\nMean = ", round(mean2), " and S.D. = ", round(sd2))
      )

      mean <- mean2
      sd <- sd2
      if(input$adornment_comp_2nd == "Percentile"){
        per <- qnorm(input$percentile_comp_2nd, mean = mean, sd = sd)
        segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
        xseq <- seq(lower, per, length = 1000)
        polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
      } else if(input$adornment_comp_2nd == "Probability"){
        if (input$prob_type_comp_2nd == "Less Than"){
          per <- input$less_than_comp_2nd
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(lower, per, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type_comp_2nd == "Between"){
          per1 <- input$between1_comp_2nd
          per2 <- input$between2_comp_2nd
          segments(x0 = per1, x1 = per1, y0 = 0, y1 = dnorm(per1, mean = mean, sd = sd), lwd = 2)
          segments(x0 = per2, x1 = per2, y0 = 0, y1 = dnorm(per2, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per1, per2, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        } else if (input$prob_type_comp_2nd == "Greater Than"){
          per <- input$greater_than_comp_2nd
          segments(x0 = per, x1 = per, y0 = 0, y1 = dnorm(per, mean = mean, sd = sd), lwd = 2)
          xseq <- seq(per, upper, length = 1000)
          polygon(x = c(xseq, rev(xseq)), y = c(dnorm(xseq, mean = mean, sd = sd), rep(0, length(xseq))), col = "Grey")
        }
      }


    })

    #calculate percentile for 1st plot, 2nd tab
    output$percentile_calc_comp_1st <- renderUI({
      school_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school1)

      #means and sds
      mean <- mean(school_data$num_debt_mean, na.rm = TRUE)
      sd <- sd(school_data$num_debt_mean, na.rm = TRUE)

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
            "Converted to the distribution of loan debt we use:",
            tags$br(),
            tags$code("loan value = mean + standard deviation * quantile"),
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
      school_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school2)

      #means and sds
      mean <- mean(school_data$num_debt_mean, na.rm = TRUE)
      sd <- sd(school_data$num_debt_mean, na.rm = TRUE)

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
            "Converted to the distribution of loan debt we use:",
            tags$br(),
            tags$code("loan value = mean + standard deviation * quantile"),
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
      school_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school1)

      #means and sds
      mean <- mean(school_data$num_debt_mean, na.rm = TRUE)
      sd <- sd(school_data$num_debt_mean, na.rm = TRUE)

      if(input$prob_type_comp_1st == "Less Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of loan debt of ",
              input$less_than_comp_1st,
              " can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
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
              "The values of loan debt, ",
              input$between1_comp_1st, " and ", input$between2_comp_1st,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
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
              "The value of loan debt of ",
              input$greater_than_comp_1st,
              " can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
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
      school_data <- filtered_data_2nd() %>%
        filter(INSTNM == input$school2)

      #means and sds
      mean <- mean(school_data$num_debt_mean, na.rm = TRUE)
      sd <- sd(school_data$num_debt_mean, na.rm = TRUE)

      if(input$prob_type_comp_2nd == "Less Than"){
        tags$div(
          "Calculation:",
          tags$br(),
          tags$ul(
            tags$li(paste0(
              "The value of loan debt of ",
              input$less_than_comp_2nd,
              " can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
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
              "The values of loan debt, ",
              input$between1_comp_2nd, " and ", input$between2_comp_2nd,
              " can each be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
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
              "The value of loan debt of ",
              input$greater_than_comp_2nd,
              " can be standardized."
            )),
            tags$li(
              "Converted to the standard normal distribution we use:",
              tags$br(),
              tags$code("standardized value = (loan debt - mean)/(standard deviation"),
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

}

# Run the application
shinyApp(ui = ui, server = server)
