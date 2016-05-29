
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(

  navbarPage(title = "Milliman",
    tabPanel(title = "Overview",
      h1("GLWB Lapse Model Explorer"),
      h2("Description"),
      p("This website presents a ",strong("Generalized Linear Model"),
        "for predicting the probability that a policyholder of a", 
        em("Variable Annuity"), " contract with a ",
        em("Guaranteed Lifetime Withdrawal Benefit (GLWB)"),
        " will surrender (lapse) their policy."),
      h2("Data"),
      p("The dataset used for the Model Explorer is a hypothetical but 
        realistic set of about 100,000 records of annuity contracts"),
      tableOutput("values"),
      br(),
      HTML("Created by G. Yollin and E. Burns. ","&#169;", "2016 Milliman. All rights reserved.")
      ),
    tabPanel(title = "Explore Results",
      tabsetPanel(type = "tabs", 
        tabPanel("A/E Analysis", 
          sidebarLayout(
            sidebarPanel(
              radioButtons("AEvar", "Variables:",varList,selected="q"),
              radioButtons("AEmodel", "Models:",ModelList),
              checkboxInput('AEcheck', 'Actual and Expected')
              ),
              mainPanel(
                plotOutput("AEPlot")
              )
            )
          ), 
        tabPanel("Lift Plots", 
          sidebarLayout(
            sidebarPanel(
              h4("side bar")
            ),
            mainPanel(
              h4("main panel")
            )
          )
        ) 
      )
    ),
    tabPanel(title = "Explore Model",
      navlistPanel(
        "Model Hierarchy",
        tabPanel("Best model (includes interactions)",
          h4("Best GLM Model"),
          verbatimTextOutput("BestModSum")
        ),
        tabPanel("Best model without interactions",
          h4("Best GLM Model without interactions"),
          verbatimTextOutput("BetterModSum")
        ),
        tabPanel("Baseline model",
          h4("Baseline Model"),
          verbatimTextOutput("BaselineModSum")
        )
      )
    ),
    tabPanel(title = "Explore Data",
      tabsetPanel(type = "tabs", 
        tabPanel("Distribution Plots", 
          sidebarLayout(
            sidebarPanel(
              radioButtons("var", "Variables:",varListMinS),
              checkboxInput('kde', 'Show Density')
            ),
            
            # Show a tabset that includes a plot, summary, and table view
            # of the generated distribution
            mainPanel(
              h4("Summary"),
              verbatimTextOutput("DataSum"),
              h4("Histogram"),
              plotOutput("DataPlot")
            )
          )
        ), 
        tabPanel("Scatter Plots", 
          sidebarLayout(
            sidebarPanel(
              selectInput("var1", "X-axis:",varListMinS),
              selectInput("var2", "Y-axis:",varListMinS,selected="ITM")
            ),
            
            # Show a tabset that includes a plot, summary, and table view
            # of the generated distribution
            mainPanel(
              h4("Scatter Plot"),
              plotOutput("ScatterPlot")
            )
          )
        ) 
      )
    )
  )
  
)
