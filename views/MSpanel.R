MSpanel <-
  
  
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
    absolutePanel(id = "menuMSp", fixed = TRUE, draggable = TRUE, class = "menu",
                  top = 60, right = 30, bottom = "auto", left = "auto",
                  width = 280, height = "auto",
      
      # Dropdown menu for selection of variable for y-axis
      selectInput(inputId = "msMSp", # id used to refer to input field
                  label = "Member State:",
                  choices = unique(dfP$ms), 
                  selected = "AT"),
      
      # Select variable for color
      selectInput(inputId = "fundMSp", 
                  label = "ESI Fund:",
                  choices = unique(dfP$fund), 
                  selected = unique(dfP$fund)[1]),
      
      
      # user buttons
      # Text instructions
      HTML(paste0("Check the box to show/hide data")),
      
      # show the data table
      # checkboxInput(inputId = "showDataMsp",
      #               label = "Show data",
      #               value = T), # used in a conditional statement
      
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
               tags$p("Rate of project selection and expenditure declared of MS Operational Programmes (% of planned financing)"),
               withSpinner(plotlyOutput(outputId = "pbarMS",  width = "auto"))),
      tags$div(class = "plot",
               tags$p("Rate of project selection and expenditure declared by TO (% of planned financing)"),
               withSpinner(plotlyOutput(outputId = "pbarTOms",  width = "auto"))),
      tags$div(class = "plot",
               tags$p("Rate of project selection and expenditure declared by TO (% of planned financing)"),
               withSpinner(plotlyOutput(outputId = "pTSlineMS",  width = "auto", height = '600')))
  )
)