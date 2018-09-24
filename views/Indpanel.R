Indpanel <-
  
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
    absolutePanel(id = "menuInd", fixed = TRUE, draggable = TRUE, class = "menu",
                  top = 60, right = 30, bottom = "auto", left = "auto",
                  width = 330, height = "auto",
                  
                  # Dropdown menu for selection of variable for y-axis
                  # selectInput(inputId = "msInd", # id used to refer to input field
                  #             label = "Member State:",
                  #             choices = unique(dfR$ms), 
                  #             selected = "AT"),
                  
                  # Select variable for color
                  selectInput(inputId = "fundInd", 
                              label = "ESI Fund:",
                              choices = c("ERDF", "CF", "ESF"), 
                              selected = "ERDF"),
                  
                  # Dropdown menu for selection of  variable for x-axis
                  selectInput(inputId = "ToInd", 
                              label = "TO:",
                              choices = unique(dfR$to)
                              #selected = "01"
                              ),
                  
                  # Dropdown menu for selection of  variable for x-axis
                  selectInput(inputId = "codeInd", 
                              label = "Indicator:",
                              choices = unique(dfR$ind_code)
                              #selected = "CO01"
                              ),
                  
                  # user buttons
                  # # Text instructions
                  # HTML(paste0("Check the box to show/hide data")),
                  # 
                  # # show the data table
                  # checkboxInput(inputId = "showDataOPp",
                  #               label = "Show data",
                  #               value = T), # used in a conditional statement
                  
                  tags$p("Source:", tags$a(href = "https://cohesiondata.ec.europa.eu/", "ESIF Open Data")),
                  
                  tags$p("Update:", update_ind)
                  
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
               tags$p("Project selection and expenditure efficiency (EUR million per indicator unit)"),
               withSpinner(plotOutput(outputId = "pbarEUind",  width = "auto")),
               tags$br()
      )
      # tags$div(class = "plot",
      #          tags$p("Rate of project selection and expenditure declared over time: OP (left) and PA (right) details (% of planned financing)"),
      #          withSpinner(plotOutput(outputId = "pbarTS",  width = "auto")),
      #          tags$br()
      # ),
      # tags$div(class = "plot",
      #          tags$p("Rate of project selection and expenditure declared by Thematic Objective (% of planned financing)"),
      #          withSpinner(plotOutput(outputId = "pbarTO",  width = "auto")),
      #          tags$br()
      # ),
      # dataTableOutput(outputId = "tData",  width = "auto")
    )
  )