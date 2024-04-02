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

# cohorts_field_of_study <- read_csv("Most-Recent-Cohorts-Field-of-Study.csv")
# institutions <- cohorts_field_of_study %>%
#   mutate(num_ind = ifelse(DEBT_ALL_PP_ANY_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_ANY_MEAN))) %>% 
#   filter(CREDLEV == 3) %>% 
#   group_by(INSTNM) %>%
#   summarize(count = sum(!is.na(num_ind))) %>%
#   filter(count >= 10)
# 
# majors <- cohorts_field_of_study %>%
#   mutate(num_ind = ifelse(DEBT_ALL_PP_ANY_MEAN == "PrivacySuppressed", NA, as.numeric(DEBT_ALL_PP_ANY_MEAN))) %>%
#   filter(CREDLEV == 3) %>%
#   group_by(CIPDESC) %>%
#   summarize(count = sum(!is.na(num_ind))) %>%
#   filter(count >= 10)
# 
# cohorts_institutions <- cohorts_field_of_study %>%
#   select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, starts_with("DEBT")) %>%
#   filter(INSTNM %in% institutions$INSTNM, CREDLEV == 3)
# 
# cohorts_majors <- cohorts_field_of_study %>%
#   select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, starts_with("DEBT")) %>%
#   filter(CIPDESC %in% majors$CIPDESC, CREDLEV == 3)

# variables_to_study <- c(
#   "Parent PLUS Loan Debt (all)" = "ALL_PP_ANY",
#   "Parent PLUS Loan Debt Males (all)"= "MALE_PP_ANY",
#   "Parent PLUS Loan Debt Not Males (all)" = "NOTMALE_PP_ANY",
#   "Parent PLUS Loan Debt to Pell Recipients (all)"= "PELL_PP_ANY",
#   "Parent PLUS Loan Debt to Non Pell Recipients (all)"= "NOPELL_PP_ANY",
#   "Parent PLUS Loan Debt (this)"= "ALL_PP_EVAL",
#   "Parent PLUS Loan Debt Males (this)" = "MALE_PP_EVAL",
#   "Parent PLUS Loan Debt Not Males (this)"= "NOTMALE_PP_EVAL",
#   "Parent PLUS Loan Debt to Pell Recipients (this)"= "PELL_PP_EVAL",
#   "Parent PLUS Loan Debt to Non Pell Recipients (this)"= "NOPELL_PP_EVAL",
#   "Stafford and Grad PLUS Loan Debt (all)" = "ALL_STGP_ANY",
#   "Stafford and Grad PLUS Loan Debt Males (all)"= "MALE_STGP_ANY",
#   "Stafford and Grad PLUS Loan Debt Not Males (all)"= "NOTMALE_STGP_ANY",
#   "Stafford and Grad PLUS Loan Debt to Pell Recipients (all)"= "PELL_STGP_ANY",
#   "Stafford and Grad PLUS Loan Debt to Non Pell Recipients (all)"= "NOPELL_STGP_ANY",
#   "Stafford and Grad PLUS Loan Debt (this)"= "ALL_STGP_EVAL",
#   "Stafford and Grad PLUS Loan Debt Males (this)" = "MALE_STGP_EVAL",
#   "Stafford and Grad PLUS Loan Debt Not Males (this)" = "NOTMALE_STGP_EVAL",
#   "Stafford and Grad PLUS Loan Debt to Pell Recipients (this)" = "PELL_STGP_EVAL",
#   "Stafford and Grad PLUS Loan Debt to Non Pell Recipients (this)" = "NOPELL_STGP_EVAL"
# )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Focus Lesson Draft App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectizeInput("school",
                         "Institution:",
                         choices = unique(cohorts_institutions$INSTNM),
                         selected = unique(cohorts_institutions$INSTNM)[1]),
            selectizeInput("measure",
                           "Variable to Study",
                           choices = variables_to_study, 
                           selected = variables_to_study[1])
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hist_mean"),
           plotOutput("hist_median")
          )
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput("major",
                       "Institution:",
                       choices = unique(cohorts_majors$CIPDESC),
                       selected = unique(cohorts_majors$CIPDESC)[1]),
        selectizeInput("measure2",
                       "Variable to Study",
                       choices = variables_to_study, 
                       selected = variables_to_study[1])
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("hist_major_mean"),
        plotOutput("hist_major_median")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {

    filtered_data <- reactive({
      #n, mean, median names
      name_n <- paste("DEBT_", input$measure, "_N", sep = "")
      name_mean <- paste("DEBT_", input$measure, "_MEAN", sep = "")
      name_median <- paste("DEBT_", input$measure, "_MDN", sep = "")
      cohorts_institutions %>%
        select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, contains(input$measure)) %>%
        filter(INSTNM == input$school) %>%
        mutate(num_debt_n = if_else(get(name_n) == "PrivacySuppressed", NA, as.numeric(get(name_n))),
             num_debt_mean = ifelse(get(name_mean) == "PrivacySuppressed", NA, as.numeric(get(name_mean))),
             num_debt_median = ifelse(get(name_median) == "PrivacySuppressed", NA, as.numeric(get(name_median))))
    })
    
    output$hist_mean <- renderPlot({

      filtered_data() %>%
        pull(num_debt_mean) %>%
        hist(breaks = 15, main = "Hist of Means")

      #hist(runif(10))

    })

    output$hist_median <- renderPlot({
      
      filtered_data() %>%
        pull(num_debt_median) %>%
        hist(breaks = 15, main = "Hist of Medians")

    })
    
    
    filtered_data_major <- reactive({
      #n, mean, median names
      name_n <- paste("DEBT_", input$measure, "_N", sep = "")
      name_mean <- paste("DEBT_", input$measure, "_MEAN", sep = "")
      name_median <- paste("DEBT_", input$measure, "_MDN", sep = "")
      cohorts_majors %>%
        select(INSTNM, CONTROL, CIPCODE, CIPDESC, CREDLEV, CREDDESC, contains(input$measure)) %>%
        filter(CIPDESC == input$major) %>%
        mutate(num_debt_n = if_else(get(name_n) == "PrivacySuppressed", NA, as.numeric(get(name_n))),
               num_debt_mean = ifelse(get(name_mean) == "PrivacySuppressed", NA, as.numeric(get(name_mean))),
               num_debt_median = ifelse(get(name_median) == "PrivacySuppressed", NA, as.numeric(get(name_median))))
    })
    
    output$hist_major_mean <- renderPlot({
      
      filtered_data_major() %>%
        pull(num_debt_mean) %>%
        hist(breaks = 15, main = "Hist of Means")
      
      #hist(runif(10))
      
    })
    
    output$hist_major_median <- renderPlot({
      
      filtered_data_major() %>%
        pull(num_debt_median) %>%
        hist(breaks = 15, main = "Hist of Medians")
      
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
