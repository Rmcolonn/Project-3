server <- function(input, output, session) {
  projectData <- read_csv("Project 3 Data.csv")
  pitcherData <- reactive({projectData %>% filter(Pitcher == input$pitcher)})
  teamData <- reactive({projectData %>% filter(PitcherTeam == input$team)})
  
  observe({
    updateSelectizeInput(session, "pitcher", 
                         choices = as.factor(teamData()$Pitcher))
  })
  
  #create plot for exploration tab
  output$plot1a <- renderPlot({
    ggplot(data = pitcherData(), aes( x = Date, y = SpinRate)) +
      geom_point()
    
  })
  
  ##create table for exploration tab
  tableData <- reactive({projectData %>% filter(Pitcher == input$pitcher & PitcherTeam == input$team) %>% 
      dplyr::select(SpinRate,Outs, Balls, Strikes)
                                                })
  output$summary1 <- renderPrint({
    (summary(tableData()))
  })
  
  output$plot1b <- renderPlot({
    ggplot(data = teamData(), aes(x = SpinRate)) +
      geom_histogram(bins = 20) +
      facet_wrap(~Pitcher, scales = "free") +
      ggtitle(input$team)
  })
  individualGraph <- reactive({
    ggplot(data = teamData(), aes(x = SpinRate)) +
      geom_histogram(bins = 20) +
      facet_wrap(~Pitcher, scales = "free") +
      ggtitle(input$team)
  })
  
  output$individualGraph <- renderPlot({
    req(individualGraph())
    individualGraph()
  })
  
  output$Download <- downloadHandler(
    filename = function(){
      paste('test', '.png', sep = '')
    },
    content = function(file){
      req(individualGraph())
      ggsave(file, plot = individualGraph(), device = 'png')
    }
  )

  ##Clustering Stuff 
  selectedData <- reactive({
    projectData %>% filter(Pitcher == input$pitcher3) %>% select(input$xcol, input$ycol) %>% na.omit() 
  })
  
  clusters <- reactive({
    set.seed(123)
    kmeans(selectedData(), input$clusters, iter.max = input$iteration, algorithm = "MacQueen")
  })
  
  output$plotClust <- renderPlot({
    palette(c("red", "orange", "yellow", "green", "blue", "purple", "pink", "black", "gray"))
    

    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 8, cex = 5, lwd = 1)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
  paste0(
    "hover: ", xy_str(input$plot_hover),
    "brush: ", xy_range_str(input$plot_brush))
  })
  
  
  
  ##Modeling stuff
  #Test/Train Data
  

  
  set.seed(123)
  modelData <- reactive({ projectData %>% filter( PitcherTeam == input$team2)})
  baseballTrain <- reactive({modelData() %>% sample_n(nrow(modelData())*.8)})
  baseballTest <- reactive({modelData() %>% dplyr::setdiff(train)})

  
  
  #Regression
  
  
  lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = baseballTrain())})

  
  output$DepPrint <- renderPrint({input$DepVar})
  output$IndPrint <- renderPrint({input$IndVar})
  output$RegSum <- renderPrint({summary(lm1())})

  
  
  output$TestModel2 <- renderPrint({
    if(input$model == "Linear Regression"){
      predict(lm1(), data = baseballTest())
      
    }})


  #Classification Tree
  knn <- reactive({caret::train(reformulate(input$IndVar2, input$DepVar2), data = baseballTrain(), 
                                      method = "knn" , 
                                      trControl = (caret::trainControl(method = "repeatedcv", 
                                                               number = as.numeric(input$number),
                                                               repeats = as.numeric(input$repeats))), 
                                                               preProcess = c("center", "scale"),
                                tuneGrid = expand.grid(k = c(2:30)))
    })

  output$DepPrint2 <- renderPrint({input$DepVar2})
  output$IndPrint2 <- renderPrint({input$IndVar2})
  output$RegSum2 <- renderPrint({(knn())})
  
  
 
  output$TestModel3 <- renderPrint({
    if(input$model == "K Nearest Neighbors"){
      predict(knn(), data = baseballTest())

    }})
  
  
  
  #Data tab
  datasetInput <-  reactive({projectData %>% filter(Pitcher == input$pitcher5) %>% 
        filter(PitcherTeam == input$team5)})
 
  
  # Table of selected dataset ----
  output$data <- DT::renderDataTable({
    DT::datatable(datasetInput(), options = list(scrollY = '500px'))
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$pitcher5, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}

