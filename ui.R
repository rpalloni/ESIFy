#########################################################
##################### User Interface ####################
#########################################################

# https://shiny.rstudio.com/articles/modules.html
# https://shiny.rstudio.com/articles/scoping.html


source('functions/fetch/FetchData.R', local = FALSE)


# https://shiny.rstudio.com/articles/layout-guide.html

shinyUI(fluidPage(

  headerPanel('Visualize Cohesion Policy OpenData'),
  
  tags$div(class = "header",
           tags$p("Data on financing and achievements under the ESI Funds 2014-2020 as available at ", tags$a(href = "https://cohesiondata.ec.europa.eu/", "Open Data Portal for the ESIF")),
           tags$p("Data are available disaggregated by fund, programme, priority axis, thematic objective and category of regions."),
           tags$p("Visualization of project selection and expenditure declared as percentage of planned financing as reported by the Managing Authorities of the programmes (ERDF only)."),
           tags$hr()),

             column(4,
                  fluidRow(
                    column(4,
                      sidebarPanel(style='width: 400px; height: 200px',
                        selectInput('ms', 'Member State', unique(dfP$ms)),
                        selectInput('title', 'Operational Programme', unique(dfP$title))
                        )
                      )
                    ),
                  fixedRow(
                    column(4,
                      tags$div(class = "image", style="text-align: center;",
                      img(src='legend.png', height = '50px', width = '200px'))
                      )
                    )
                  ),
              column(8,
                     mainPanel(style='width: 1200px; height: 600px',
                     plotOutput('pbarMS'),
                     tags$hr(),
                     plotOutput('pbarOP'),
                     tags$hr(),
                     plotOutput('pbarTS'), # time series
                     tags$hr(),
                     plotOutput('pbarTO')
                   )
              ),
  
    tags$div(
          tags$p("Developed by:", tags$a(href = "https://www.linkedin.com/in/roberto-palloni-6b224031/", "Roberto Palloni")),
          tags$p("GitHub:", tags$a(href = "https://github.com/rpalloni/CohesionDataVisualization", "rpalloni")))
  )
)





