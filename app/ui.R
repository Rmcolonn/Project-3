## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(knitr)
library(DT)

projectData <- read_csv("Project 3 Data.csv")
clusterData <- dplyr::select(projectData, RelSpeed:SpinAxis, RelHeight:ZoneSpeed)

ui <- dashboardPage(
  dashboardHeader(title = "Project 3"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "Info", icon = icon("file-alt")),
      menuItem("Data Exploration", tabName = "Exploration", icon = icon("th")),
      menuItem("Clustering", tabName = "Cluster", icon = icon("th")),
      menuItem("Modeling", tabName = "Modeling", icon = icon("th")),
      menuItem("Data", tabName = "Data", icon = icon("th"))
    )
  ),
  #Body Content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Info",
              h1("App Information"), 
              p("The purpose of this app is to go through the methodology of analyzing a new dataset. The data being used for analysis is baseball data from various ACC teams in 2019. This app has many functionalities with each tab serving a different function.  The features of each tab is explained on that page. This page is to give you a summary of what is going on in the following tabs:", strong("Data Exploration, Clustering, Modeling, and Data")),
              
              br(),
              strong("Data Exploration:"),
              p("The data exploration tab allows you to select a team and pitcher and shows you some plots and some summary statistics for that specific person/team."), 
              
              br(),
              strong("Clustering:"),
              p("The clustering tab goes allows you to select variables to cluster and some cluster specifications. If you would like to learn more about clustering go to", a("this webpage", href = "https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/")),
              
              br(),
              strong("Modeling:"),
              p("The modeling tab has a few aspects to it. You have the option to select a type of model to run analysis on and then you can select the predictor variables to use. The models you can choose from are linear regression or a kmeans model. The form of the regression mode is: ", withMathJax(helpText( " $$y = \\beta_0\\ + x_1 * \\beta_1\\ + ... + x_p * \\beta_p\\  $$")), p(" With both models, the data being used is specific to the team selected. There is a test tab which allows you to test the model you created. You will select the percentage of data in the train/test data set and then select the test button to test your selected model with the test dataset. ")), 
              br(),
              strong("Data:"),
              p("The last tab is a data tab. It allows you to select a player on a specifc team and you have to option to download that data for that player.")


      ),
      
      # Second tab content
      tabItem(tabName = "Exploration",
              h2("Let's explore the data..."),
              selectizeInput("team", "Team", selected = "NOR_TAR",
                             choices = levels(as.factor(projectData$PitcherTeam))),
              selectizeInput("pitcher", "Pitcher", selected = "Bergner, Austin", 
                             choices = levels(as.factor(projectData$Pitcher))),
              br(),
              
              mainPanel( 
                tabsetPanel(
                  tabPanel("Pitcher Plot", plotOutput("plot1a")), 
                  tabPanel("Summary", verbatimTextOutput("summary1")),
                  tabPanel("Team Plot", 
                           p("Click the download button at the bottom if you would like to donwload the plot"),
                           plotOutput("plot1b"),
                           fluidPage(downloadButton('Download'))))
                
                
      )),
      
      # Third tab content
      tabItem(tabName = "Cluster",
              h2("K-Means Clustering"),
              p("Select the X and Y vairables and select the pitcher you would like to see the cluster. Click and drag on the plot to get some info about the points, or hover over to see the point information."),
              sidebarPanel(
                selectInput('xcol', 'X Variable', names(clusterData)),
                selectInput('ycol', 'Y Variable', names(clusterData),
                            selected=names(clusterData)[[2]]),
                selectizeInput("pitcher3", "Pitcher", selected = "Bergner, Austin", 
                               choices = levels(as.factor(projectData$Pitcher))),
                numericInput('clusters', 'Cluster count', 3,
                             min = 1, max = 9, step = 1),
                numericInput('iteration', '# of Iterations of Algorithm', value = 1, min = 1, max = 20, step = 1)
              ),
              mainPanel(
                plotOutput('plotClust', hover = "plot_hover", brush = "plot_brush"),
                verbatimTextOutput("info")
              )
      ),
      
      # Fourth tab content
      tabItem(tabName = "Modeling",
              h2("Select your Model Type"),
              selectizeInput("model", "Model Type", selected = "Linear Regression", 
                             choices = c("Linear Regression", "K Nearest Neighbors")),
              selectizeInput("team2", "Team", selected = "NOR_TAR",
                             choices = levels(as.factor(projectData$PitcherTeam))),
              selectizeInput("dataSize", "Select Percentage of data for training model", selected = .9,
                             choices = c(.7, .75, .8, .9)),
             
             
               div(
                 tabsetPanel(
                 tabPanel("Models" ,
             
      conditionalPanel(
        condition = "input.model == 'Linear Regression'",
        h3("Regression Analysis"), 
        p("This data used for analysis is dependent on the team selected, and you need to select at least one independent variable to see the model"),
        sidebarPanel(
          p("Select the inputs for the Dependent Variable"),
          selectInput(inputId = "DepVar", label = "Dependent Variables", choices = list("SpinRate")),
          p("Select the inputs for the Independent Variable"),
          selectInput(inputId = "IndVar", label = "Independent Variables", multiple = TRUE, 
                      choices = list( "RelSpeed", "RelHeight", "RelSide", "TruePitch", "Pitcher", "BatterSide"))
        ),
        mainPanel(
          verbatimTextOutput(outputId = "RegSum"),
          verbatimTextOutput(outputId = "IndPrint"),
          verbatimTextOutput(outputId = "DepPrint")
        )
      ),
      
      conditionalPanel(
        condition = "input.model == 'K Nearest Neighbors'",
        h3("K Nearest Neighbors"), 
        p("This data used for analysis is dependent on the team selected, and you need to select at least one independent variable to see the model. It takes a minute or two for this model to run."),
        sidebarPanel(
          p("Select the inputs for the Dependent Variable"),
          selectInput(inputId = "DepVar2", label = "Dependent Variables", choices = list("TruePitch")),
          p("Select the inputs for the Independent Variable"),
          selectInput(inputId = "IndVar2", label = "Independent Variables", multiple = TRUE, 
                      choices = list( "RelSpeed", "RelHeight", "RelSide", "SpinRate", "Pitcher", "BatterSide")),
          selectizeInput("number", "Select Number for Repeated cv", selected = 10, 
                         choices = c(1,5,10,15)),
          selectizeInput("repeats", "Select repeats for Repeated cv", selected = 3, 
                         choices = c(1,2,3,4)),
        mainPanel(
      
      verbatimTextOutput(outputId = "RegSum2"),
      verbatimTextOutput(outputId = "IndPrint2"),
      verbatimTextOutput(outputId = "DepPrint2"), width = 20
      )))), 
      
      tabPanel("Test",
                 mainPanel(
                   verbatimTextOutput(outputId = "TestModel2"),
                   verbatimTextOutput(outputId = "TestModel3")
                 ))),
      class = "span7")),
        

      # Fifth tab content
      tabItem(tabName = "Data",
              h2("Here's the data"),
              
              sidebarPanel(
                # Button
                downloadButton("downloadData", "Download"),
                selectizeInput("team5", "Team", selected = "NOR_TAR",
                             choices = levels(as.factor(projectData$PitcherTeam))),
                selectizeInput("pitcher5", "Pitcher", selected = "Bergner, Austin", 
                             choices = levels(as.factor(projectData$Pitcher))),
              br()
              ),
              
              mainPanel(
                DT::dataTableOutput("data")
              )
              
      )
      
    )
  )
)




