
load("PLSISE/R/functions.rdata")
source("~/GitHub/PLSISE/R/libraries.R", echo=TRUE)

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
                                verbatimTextOutput("summary")
                              ) # Main Panel for summary
                              
                            ), # Sidebar Layout
                            
                            # Main panel for displaying outputs ----
                            mainPanel(
                              
                              # Output: Data file ----
                              dataTableOutput("contents")
                              
                            ) # Table Output Main                        
                   ), # Add Files Tab Panel layout
                   
                   tabPanel(title = "Fit",
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                # X variables checkbox
                                checkboxGroupInput(inputId = "Xvar",
                                              label = "Select your X variables",
                                              choices = c()),
                                
                                # Y variables checkbox
                                checkboxGroupInput(inputId = "Yvar",
                                                   label = "Select your Y variables",
                                                   choices = c()),
                                
                                # Input: Simple integer interval ----
                                sliderInput(inputId = "Ncomps", 
                                            label = "Select a number of components",
                                            min = 2, max = 3,
                                            value = 2, step = 1,
                                            ticks = FALSE),
                                
                                actionButton(
                                  inputId = "RunPLS",
                                  label = "Run PLS Regression"),
                              ),
                              
                              mainPanel(

                                verbatimTextOutput("fit")
                              ),
                            ),
                   ),
                   
                   tabPanel("Predict"
                   ), # Predict Tab Panel layout
                   
                   tabPanel("Graphics"
                   ) # Graphics Tab Panel layout
                   
                 ) # NavBar Page
) # Fluid Page

server <- function(input, output, session) {
  
  hideTab(inputId = "Tabspanel", target = "Fit")
  hideTab(inputId = "Tabspanel", target = "Predict")
  hideTab(inputId = "Tabspanel", target = "Graphics")
  hide("RunPLS")
  
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

  # X variables to select
  observeEvent(dataframe(),{
    
    updateCheckboxGroupInput(session,
                             "Xvar",
                             choices = colnames(dataframe()))

  })
    #  Y variables to select according to X variables
    observeEvent(YFitSelector(),{
      
    updateCheckboxGroupInput(session,
                       "Yvar",
                       choices = YFitSelector())

  })  
    
    PLS <- eventReactive(input$RunPLS,{ 
      
      tryCatch(
        {
          df <- dataframe()
          
          XtoSubset <- input$Xvar
          XtoSubset <- c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm")
          
          df[,XtoSubset] <- lapply(df[,XtoSubset] , as.numeric)
          YtoFactor <- input$Yvar
          ySub = c("Species")
          
          df[YtoFactor] <- lapply(df[YtoFactor] , factor)
          
          Subset = c(XtoSubset, YtoFactor)
          df <- df[, Subset]
          
          PlsFormula = as.formula(paste(YtoFactor, "~", ".", sep = ""))
          
          pls <- fit(formula = PlsFormula, data = df, ncomp = input$Ncomps)

          # Must be below data !  
          showTab(inputId = "Tabspanel", target = "Predict")
          showTab(inputId = "Tabspanel", target = "Graphics")
          
          return(pls)

        }, error = function(e){
          stop(safeError(e))
        }
      )
    }) 
  
  output$fit <- renderPrint({
    
    return(PLS())

  })
  
} # Server

# Run the application 
shinyApp(ui = ui, server = server)