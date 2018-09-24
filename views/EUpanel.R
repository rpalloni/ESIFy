EUpanel <-

  
  # Sidebar layout
  # page component for a page divided in a SIDEBAR (input) and a MAIN area (output)
  # in the background Shiny implements layout features available in Bootstrap 2.0 (css framework)
  # https://shiny.rstudio.com/articles/layout-guide.html
  sidebarLayout(
   
    ##################################################################
    ########################### sidebar area #########################
    # the sidebar panel contains interface input controls
    # Input fields (widgets)
    ##################################################################
    absolutePanel(id = "menuEUp", fixed = TRUE, draggable = TRUE, class = "menu",
                  top = 60, right = 30, bottom = "auto", left = "auto",
                  width = 330, height = "auto",
                  
                  
                  # Select variable for color
                  selectInput(inputId = "fundEUp", 
                              label = "ESI Fund:",
                              choices = unique(dfP$fund)),
                  
                  
                  # user buttons
                  # Text instructions
                  HTML(paste0("Check the box to show/hide data")),
                  
                  # show the data table
                  #checkboxInput(inputId = "showDataMsp",
                  #              label = "Show data",
                  #              value = T), # used in a conditional statement
                  
                  tags$p("Source:", tags$a(href = "https://cohesiondata.ec.europa.eu/", "ESIF Open Data")),
                  
                  tags$p("Update:", update)
                  
                  
                  # Numeric input for sample size
                  #numericInput(inputId = "n",
                  #             label = "Sample size:",
                  #             value = 30,
                  #             min = 1, max = n_total, 
                  #             step = 1)
    ),
   
   ###############################################################
   ########################### main area #########################
   # mainPanel creates the area to host server response (outputs)
   ###############################################################
   
   # output alternatives:
   # dataTableOutput
   # htmlOutput
   # imageOutput
   # plotOutput
   # tableOutput
   # textOutput
   # uiOutput
   mainPanel(
     tags$div(class = "plot",
               tags$p("Rate of project selection and expenditure declared of EU Member States (% of planned financing)"),
               withSpinner(plotOutput(outputId = "pbarEU",  width = "auto"))
     ),
     tags$div(class = "plot",
              tags$p("Rate of project selection and expenditure declared by TO (% of planned financing)"),
              withSpinner(plotOutput(outputId = "pbarTOeu",  width = "auto"))
     ),
     tags$div(class = "plot",
              tags$p("Project selection and expenditure declared by TO (EUR billion)"),
              withSpinner(plotOutput(outputId = "pbarTOeuro",  width = "auto"))
     ),
     tags$div(tags$br()) # 
   )
 )