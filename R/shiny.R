
load("PLSISE/R/functions.rdata")
source("~/GitHub/PLSISE/R/libraries.R", echo=TRUE)
setwd("~/GitHub/")

ui <- fluidPage(theme = shinytheme('united'),
                
                useShinyjs(),
                
                # Application title
                navbarPage(
                  "Partial Least Square Regression : ",
                  id = "Tabspanel",
                  
                  tabPanel(title = "Add files",
                           
                           sidebarLayout(
                             sidebarPanel(
                               #  Input: Select a file ----
                               fileInput("datafile", "Choose CSV File",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               # Input: Checkbox if file has header ----
                               checkboxInput("header", "Header", TRUE),
                               
                               # Input: Select separator ----
                               radioButtons("inSep", "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ","),
                               
                               fluidRow(
                                 column(3,
                                        actionButton(
                                          inputId = "submitFile",
                                          label = "Show file",
                                        ),
                                 ),
                                 column(1),
                                 
                               ),
                               br(),
                               
                               # Input: Select number of rows to display ----
                               radioButtons("disp", "Display",
                                            choices = c(Head = "head",
                                                        "100" = "100",
                                                        All = "all"),
                                            selected = "head"),
                               
                             ),
                             
                             mainPanel(
                               verbatimTextOutput("summary"),
                              
                             ) # Main Panel for summary
                             
                           ), # Sidebar Layout
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                             
                             # Output: Data file ----
                             dataTableOutput("contents")
                             
                           ) # Table Output Main                        
                  ), # Add Files Tab Panel layout
                  
                  
                  tabPanel(title = "Fit", value = "Fit",
                           
                           sidebarLayout(
                             
                             sidebarPanel(
                               # X variables checkbox
                               selectInput(inputId = "Xvar",
                                           label = "Select your X variables",
                                           choices = c(),
                                           multiple = TRUE),
                               
                               # Y variables checkbox
                               selectInput(inputId = "Yvar",
                                                  label = "Select your Y variables",
                                                  choices = c(),
                                           multiple = TRUE),
                               
                               actionButton(
                                 inputId = "VariablesRecommended",
                                 label = "Show recommended variables"),
                               
                               br(),
                               br(),
                              
                               # Input: Simple integer interval ----
                               sliderInput(inputId = "Ncomps", 
                                           label = "Select a number of components",
                                           min = 2, max = 3,
                                           value = 2, step = 1,
                                           ticks = FALSE),
                               
                               actionButton(
                                 inputId = "RunPLS",
                                 label = "Run PLS Regression"),
                               
                               br(),
                               br(),
                               
                               actionButton(
                                 inputId = "CrossValidation",
                                 label = "Run a cross validation"),
                             ), # SiderBarPanel
                             
                             mainPanel(
                               verbatimTextOutput("VarRecommended"),
                               verbatimTextOutput("fit"),
                               verbatimTextOutput("CrossVal"),
                               
                             ), # Main panel
                           ),
                  ),
                  
                  tabPanel(title = "Predict",
                           
                           sidebarLayout(
                             sidebarPanel(
                               #  Input: Select a file ----
                               fileInput("dataPred", "Choose CSV File",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               # Input: Checkbox if file has header ----
                               checkboxInput("PredHeader", "Header", TRUE),
                               
                               # Input: Select separator ----
                               radioButtons("PredinSep", "Separator",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ","),
                               
                               radioButtons("type", "Prediction value : ",
                                            choices = c(Value = "value",
                                                        Softmax = "posterior",
                                                        Class = "class"),
                                            selected = "class"),
                               
                               actionButton(
                                 inputId = "doPrediction",
                                 label = "Predict my file !",
                               ),
                               
                               br(),
                               br(),
                               
                               # Input: Select number of rows to display ----
                               radioButtons(inputId = "PredDisplay", label = "Display",
                                            choices = c(Head = "head",
                                                        "100" = "100",
                                                        All = "all"),
                                            selected = "head"),
                               
                               downloadButton("downloadData", "Download predictions")
                               
                             ), # SidebarPanel
                             
                             mainPanel(
                               verbatimTextOutput("PrintPredict")
                             ) # Main Panel for Prediction
                             
                           ), # Sidebar Layout
                           
                           mainPanel(
                             
                             # Output: Data Pred ----
                             DT::dataTableOutput("Predcontents")
                             
                           ) # Table Output Main                        
                  ), # Prediction Tab Panel layout
                  
                  navbarMenu("Graphics",
                             tabPanel("Scree Plot",
                                      
                                      radioButtons("Method", "Select a method : ",
                                                   choices = c(Kaiser = "kaiser",
                                                               "Broken Sticks" = "broken_sticks"),
                                                   selected = "kaiser"),
                                      
                                      plotlyOutput("scree_plot")
                                      
                             ), # Graphics Tab Panel layout
                             
                             tabPanel("Variables",
                                      
                                      selectInput("X_Variables", 
                                                  label = "Select a component in X", 
                                                  choices = c(), 
                                                  selected = 1),
                                      
                                      selectInput("Y_Variables", 
                                                  label = "Select a component in Y", 
                                                  choices = c(), 
                                                  selected = 2),
                                      
                                      plotlyOutput("pls_variables")
                                      
                             ),
                             
                             
                             
                             tabPanel("Individuals",
                                      
                                      selectInput("X_Individuals", 
                                                  label = "Select a component in X", 
                                                  choices = c(), 
                                                  selected = 1),
                                      
                                      selectInput("Y_Individuals", 
                                                  label = "Select a component in Y", 
                                                  choices = c(), 
                                                  selected = 2),
                                      
                                      plotlyOutput("pls_individuals")),
                             
                             tabPanel("Explanatory variables",
                                      
                                      selectInput("plot_X", 
                                                  label = "Select a column in X", 
                                                  choices = c(), 
                                                  selected = 1),
                                      
                                      selectInput("plot_Y", 
                                                  label = "Select a column in Y", 
                                                  choices = c(), 
                                                  selected = 2),
                                      
                                      plotlyOutput("x_plot")),
                             
                  ) # NavBar Menu 
                  
                ) # NavBar Page
) # Fluid Page

server <- function(input, output, session) {
  
  hideTab(inputId = "Tabspanel", target = "Fit")
  hideTab(inputId = "Tabspanel", target = "Predict")
  hideTab(inputId = "Tabspanel", target = "Graphics")
  hide("RunPLS")
  hide("downloadData")
  hide("CrossValidation")

  dataframe <- eventReactive(input$submitFile,{ 
    
    tryCatch(
      {
        data = read.csv(input$datafile$datapath,
                        header = input$header,
                        sep = input$inSep)
        
        # Must be below data !  
        showTab(inputId = "Tabspanel", target = "Fit")
        return(data)
        
      }, error = function(e){
        stop(safeError(e))
      }
    )
  })
  
  # Print the data loaded
  output$contents <- DT::renderDataTable({
    
    df <- dataframe()
    
    if(input$disp == "head") {
      return(head(df))
    }
    if(input$disp == "100") {
      return(head(df, 100))
    }
    if(input$disp == "all") {
      return(df)
    }
    
  }) # output$contents renderdataTable
  
  output$summary <- renderPrint(
    
    summary(dataframe())
  )
  
  # Setting Y columns choices with var
  YFitSelector <- reactive({
    
    choices <- colnames(dataframe())
    Xselected <- input$Xvar
    ToDropY <- choices[which(choices %in% Xselected)]
    
    Ychoices <- choices[!choices %in% ToDropY]
    return(Ychoices)
    
  })
  
  # Update each selector
  observeEvent(dataframe(),{

      updateSelectInput(session,
                        "Xvar",
                        choices = colnames(dataframe()))
  })
  
  # Set the number of Comp and allow user to run PLS
  NumberOfComps <- reactive({
    
    lIXvar <- length(input$Xvar)
    lIYvar <- length(input$Yvar)
    if( lIXvar > 1 & lIYvar >= 1){
      show("RunPLS")
    } else {
      hide("RunPLS")
    }
    number <- length(input$Xvar)
    
    return(number)
  })
  
  # Update the slider according to the dataset
  observeEvent(NumberOfComps(),{
    
    updateSliderInput(session, "Ncomps",
                      max = NumberOfComps())
  })
  
  #  Y variables to select according to X variables
  observeEvent(YFitSelector(),{
    
    updateSelectInput(session,
                      "Yvar",
                      choices = YFitSelector())
    
  })  
  
  VariablesRecommended <- eventReactive(input$VariablesRecommended,{ 
    
    tryCatch(
      {
        data = dataframe()
        selection <- select_variable(DF = data[,input$Xvar], 
                                     cible = data[,input$Yvar])
        
        return(selection)
        
      }, error = function(e){
        stop(safeError(e))
      }
    )
  })
  
  output$VarRecommended <- renderPrint({
    
    return(VariablesRecommended())
  })
  
  PLS <- eventReactive(input$RunPLS,{ 
    
    tryCatch(
      {
        df <- dataframe()
        
        XtoSubset <- input$Xvar
        df[,XtoSubset] <- lapply(df[,XtoSubset] , as.numeric)
        
        YtoFactor <- input$Yvar
        df[YtoFactor] <- lapply(df[YtoFactor] , factor)
        
        Subset = c(XtoSubset, YtoFactor)
        df <- df[, Subset]
        
        PlsFormula = as.formula(paste(YtoFactor, "~", ".", sep = ""))
        
        PLSDA <- fit(formula = PlsFormula, data = df, ncomp = input$Ncomps)
        
        # Must be below pls !
        show("CrossValidation")
        showTab(inputId = "Tabspanel", target = "Predict")
        showTab(inputId = "Tabspanel", target = "Graphics")
        
        return(PLSDA)
        
      }, error = function(e){
        stop(safeError(e))
      }
    )
  }) 
  
  output$fit <- renderPrint({
    
    return(PLS())
  })

  CrossValidation <- eventReactive(input$CrossValidation,{ 
    
    tryCatch(
      {
        df <- dataframe()
        
        XtoSubset <- input$Xvar
        df[,XtoSubset] <- lapply(df[,XtoSubset] , as.numeric)
        
        YtoFactor <- input$Yvar
        df[YtoFactor] <- lapply(df[YtoFactor] , factor)
        
        Subset = c(XtoSubset, YtoFactor)
        df <- df[, Subset]
        
        CrossValFormula = as.formula(paste(YtoFactor, "~", ".", sep = ""))
        
        CrossVal <- cross_validation(formula = CrossValFormula, data = df, 
                                     ncomp = input$Ncomps)

        return(CrossVal)
        
      }, error = function(e){
        stop(safeError(e))
      }
    )
  }) 
  
  output$CrossVal <- renderPrint({
    
    return(CrossValidation())
  })
  
  # Scree plot graphic
  output$scree_plot <- renderPlotly({
    
    scree_plot(PLS(), method = input$Method)
    
  })
  
  # Setting Y columns choices with var
  YCompsVariableSelector <- reactive({
    
    Comps <- c(1:PLS()$N_comp)
    Xselected <- input$X_Variables
    ToDropY <- Comps[which(Comps %in% Xselected)]
    
    Ycomps <- Comps[!Comps %in% ToDropY]
    return(Ycomps)
    
  })
  
  # X variables to select
  observeEvent(PLS()$N_comp,{
    
    V_Comps = c(1:PLS()$N_comp)
    
    updateSelectInput(session,
                      "X_Variables",
                      choices = V_Comps)
    
    updateSelectInput(session,
                      "X_Individuals",
                      choices = V_Comps)
  })
  
  #  Y variables to select according to X variables
  observeEvent(YCompsVariableSelector(),{
    
    updateSelectInput(session,
                      "Y_Variables",
                      choices = YCompsVariableSelector())
    
  })
  
  # Variable graphic
  output$pls_variables <- renderPlotly({
    
    pls_variables(PLS(), 
                  Axis_1 = as.numeric(input$X_Variables), 
                  Axis_2 = as.numeric(input$Y_Variables))
  })
  
  # Setting Y columns choices with var
  YCompsIndividualsSelector <- reactive({
    
    Comps <- c(1:PLS()$N_comp)
    Xselected <- input$X_Individuals
    ToDropY <- Comps[which(Comps %in% Xselected)]
    
    Ycomps <- Comps[!Comps %in% ToDropY]
    return(Ycomps)
    
  })
  
  observeEvent(YCompsIndividualsSelector(),{
    
    updateSelectInput(session,
                      "Y_Individuals",
                      choices = YCompsIndividualsSelector())
  })
  
  output$pls_individuals <- renderPlotly({
    
    pls_individuals(PLS(),
                    Axis_1 = as.numeric(input$X_Individuals),
                    Axis_2 = as.numeric(input$Y_Individuals))
    
  })
  
  YColumns <- reactive({
    
    columns <- colnames(dataframe())
    toClass = input$Yvar
    Xselected <- input$plot_X
    ToDropY <- columns[which(columns %in% Xselected)]
    ToDropY <- c(ToDropY, toClass)
    Ycolumns <- columns[!columns %in% ToDropY]
    return(Ycolumns)
    
  })
  
  observeEvent(YColumns(),{
    
    updateSelectInput(session,
                      "plot_Y",
                      choices = YColumns())
  })
  
  
  
  XColumns <- reactive({
    
    # Delete Yvar
    cols <- colnames(dataframe())
    DelClass <- input$Yvar
    ColWithoutYvar <- cols[!cols %in% DelClass]
    
  })
  
  observeEvent(XColumns(),{
    
    updateSelectInput(session,
                      "plot_X",
                      choices =  XColumns())
  })
  
  output$x_plot <- renderPlotly({
    
    toClass = input$Yvar
    x_plot(data = dataframe(), varX = input$plot_X, varY = input$plot_Y, class = toClass)
    
  })
  
  ################################ PREDICTION ####################################  
  
  PredDataframe <- eventReactive(input$doPrediction,{
    
    tryCatch(
      {
        
        pred = read.csv(input$dataPred$datapath,
                        header = input$PredHeader,
                        sep = input$PredinSep)
        show("downloadData")
        
        
        return(pred)
        
      }, error = function(e){
        stop(safeError(e))
      }
    )
  })
  
  Predict <- eventReactive(input$doPrediction,{ 
    
    tryCatch(
      {
        df <- PredDataframe()
        
        XFit <- input$Xvar
        df <- df[,XFit]
        df[,XFit] <- lapply(df[,XFit] , as.numeric)
        
        
        pred <- predict(PLS(), newdata = df, type = input$type)

        return(pred)
        
      }, error = "There is an error"
    )
  })
  
  DataWithPred <- reactive({
    
    df <- PredDataframe()
    colname <- ncol(df)
    prediction <- Predict()
    
    newdf <- bind_cols(df, prediction)
    if(ncol(newdf) == colname + 1){
      colnames(newdf)[colname+1] <- "Class"
    }
    return(newdf)
  })
  
  # Print the data loaded
  output$Predcontents <- DT::renderDataTable({
    
    if(input$PredDisplay == "head") {
      return(head(DataWithPred()))
    }
    if(input$PredDisplay == "100") {
      return(head(DataWithPred(), 100))
    }
    if(input$PredDisplay == "all") {
      return(DataWithPred())
    }
    
  }) # output$contents renderdataTable
  
  output$PrintPredict <- renderPrint(
    
    print(Predict())
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(substr(input$dataPred, 1, nchar(input$dataPred)-4), "_Predicted", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(DataWithPred(), file, row.names = FALSE)
    }
  )
  
} # Server

# Run the application 
shinyApp(ui = ui, server = server)