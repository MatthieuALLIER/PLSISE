
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
                                  
                                  # column(4,
                                  #        actionButton(
                                  #          inputId = "library",
                                  #          label = "Go to package",
                                  #          
                                  # ),
                                  # )
                                  
                                ),
                                br(),
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
                                checkboxGroupInput(inputId = "Xvar",
                                              label = "Select your X variables",
                                              choices = c()),
                                checkboxGroupInput(inputId = "Yvar",
                                                   label = "Select your Y variables",
                                                   choices = c())
                              ),
                              
                              mainPanel(
                                
                                # verbatimTextOutput("fit")
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
  
  dataframe <- eventReactive(input$submitFile,{ 
    
    tryCatch(
      {
        
        data = read.csv(input$datafile$datapath,
                        header = input$header,
                        sep = input$inSep)
        
        # Must be below data !  
        showTab(inputId = "Tabspanel", target = "Fit")
        showTab(inputId = "Tabspanel", target = "Predict")
        showTab(inputId = "Tabspanel", target = "Graphics")
        
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
  
  observeEvent(dataframe(),{
    updateCheckboxGroupInput(session,
                             "Xvar",
                             choices = colnames(dataframe()))
    
    updateCheckboxGroupInput(session,
                       "Yvar",
                       choices = colnames(dataframe()))

  })  
  # output$fit <- renderPrint({
  #   
  #   df <- dataframe()
  #   
  #   df$Yvar <- as.factor(df$Yvar)
  #   
  #   pls <- fit(formula = Yvar~., data = df, ncomp = 4)
  #   return(pls$Comps)
  #   
  # })
  
} # Server

# Run the application 
shinyApp(ui = ui, server = server)