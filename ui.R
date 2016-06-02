
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(

  navbarPage(title = strong("Milliman"),
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
    tabPanel(title = "Explore Data",
      tabsetPanel(type = "tabs", 
        tabPanel("Distribution Plots", 
          sidebarLayout(
            sidebarPanel(
              radioButtons("var", "Variables:",varListMinS),
              uiOutput("show_density")
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
    ),
    tabPanel(title = "Explore Models",
      navlistPanel(
        "Model Hierarchy",
        tabPanel("Best model (includes interactions)",
          plotOutput("BestTornado"),
          DT::dataTableOutput("BestModSum")
        ),
        tabPanel("Best model without interactions",
          plotOutput("BetterTornado"),
          DT::dataTableOutput("BetterModSum")
        ),
        tabPanel("Baseline model",
          plotOutput("BaseTornado"),
          DT::dataTableOutput("BaseModSum")
        )
      )
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
              radioButtons("LiftModell", "Model 1:",ModelList),
              radioButtons("LiftModel2", "Model 2:",ModelList,selected="Baseline Model")
            ),
            mainPanel(
              plotOutput("LiftPlot")
            )
          )
        ) 
      )
    ),
    tabPanel(title = "Predict",
      sidebarLayout(
        sidebarPanel(
          radioButtons("PredSCPeriod", "Surrender charge period:",
            c("4 years"=4, "7 years"=7),selected="7"),
          radioButtons("PredRiderCode", "Rider code:",
            c("A", "B","C"),selected="C"),
          numericInput(inputId="PredAge", label="Policy owner age:", value=65, min = 18, max = 75),
          numericInput(inputId="Predq", label="Policy duration in quarters:", value=14, min = 1, max = 48),
          numericInput(inputId="PredBB", label="Benefit base:", 
            min = 1000, max = 10000000, value = 100000, step = 1000),
          numericInput(inputId="PredAV", label="Account value:", 
            min = 1000, max = 4000000, value = 100000, step = 1000)
        ),
        mainPanel(
          tableOutput("PredDf"),
          plotOutput("PredBarplot")
        )
      )
    )
  )
  
)
