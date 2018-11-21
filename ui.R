# call the libraries and the data
source('functions/fetch/FetchData.R', local = F)

#################################################################################
################################### front-end ###################################
#################################################################################
# EACH input and output element MUST have a unique ID


source('views/info.R', local = F)
source('views/EUpanel.R', local = F)
source('views/MSpanel.R', local = F)
source('views/OPpanel.R', local = F)
source('views/Indpanel.R', local = F)


shinyUI(
  
  navbarPage(
        "ESIFy",
        tabPanel(icon("home"), info),
        tabPanel("EU level", EUpanel),
        tabPanel("MS level", MSpanel),
        tabPanel("OP level", OPpanel),
        navbarMenu("More",
                   tabPanel("Indicators",Indpanel),
                   tabPanel("Financial Instruments")
        ),
        
        includeCSS("www/style.css"),
        
        tags$div(class = "footer",
                 tags$p("Developed by:", tags$a(href = "https://www.linkedin.com/in/roberto-palloni-6b224031/", "Roberto Palloni")),
                 tags$p("GitHub:", tags$a(href = "https://github.com/rpalloni/ESIFy", "rpalloni"))))
)


# Create a Shiny app object
# shinyApp(ui = ui, server = server)

