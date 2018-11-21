OPpanel <-
  
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
    absolutePanel(id = "menuOPp", fixed = TRUE, draggable = TRUE, class = "menu",
                 top = 60, right = 30, bottom = "auto", left = "auto",
                 width = 330, height = "auto",
      
      # Dropdown menu for selection of variable for y-axis
      selectInput(inputId = "msOPp", # id used to refer to input field
                  label = "Member State:",
                  choices = unique(dfP$ms), 
                  selected = "AT"),
      
      # Select variable for color
      selectInput(inputId = "fundOPp", 
                  label = "ESI Fund:",
                  choices = unique(dfP$fund), 
                  selected = unique(dfP$fund)[1]),
      
      # Dropdown menu for selection of  variable for x-axis
      selectInput(inputId = "titleOPp", 
                  label = "Operational Programme:",
                  choices = unique(dfP$title), 
                  selected = unique(dfP$title)[1]),
      
      # user buttons
      # Text instructions
      HTML(paste0("Check the box to show/hide data")),
      
      # show the data table
      checkboxInput(inputId = "showDataOPp",
                    label = "Show data",
                    value = T), # used in a conditional statement
      
      tags$p("Source:", tags$a(href = "https://cohesiondata.ec.europa.eu/", "ESIF Open Data")),
      
      tags$p("Update:", update)
      
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
        tags$p("Rate of project selection and expenditure declared by Operational Program Priority Axis (% of planned financing)"),
        withSpinner(plotlyOutput(outputId = "pbarPA",  width = "auto")),
        tags$br()
        ),
      tags$div(class = "plot",
        tags$p("Rate of project selection and expenditure declared over time: OP (left) and PA (right) details (% of planned financing)"),
        withSpinner(plotOutput(outputId = "pbarTS",  width = "auto")),
        tags$br()
        ),
      tags$div(class = "plot",
        tags$p("Rate of project selection and expenditure declared by Thematic Objective (% of planned financing)"),
        withSpinner(plotlyOutput(outputId = "pbarTO",  width = "auto")),
        tags$br()
        ),
      dataTableOutput(outputId = "tData",  width = "auto")
    )
  )