info <-
  bootstrapPage(
    tags$div(class = "container",
      tags$div(class = "row justify-content-center",
        tags$div(class = "col-4 header",
                 tags$p("European Structural and Investment Funds opendata visualization")
        )
      ),
      tags$div(class = "row justify-content-center",
        tags$div(class = "col-6 text",
                  tags$p("ESIFy is the tool developed as side project of the thesis",
                  tags$b("Open Data Analytics - Advanced methods, tools and visualization for policy making (2018)."),
                  "This web tool allows to drill down and roll up on ESIF opendata to easily visualize level and performance of investments Europe wide.")
        )
      ),
      tags$div(class = "row justify-content-center",
        tags$div(class = "col-6 text",
                 tags$p("The objective of this dashboard is to allow wider and deeper analysis of ESIF open data as available in the ESIF opendata portal."),
                 tags$a(href = "https://cohesiondata.ec.europa.eu/", "ESIF Open Data")
        )
      )
    )
  )
