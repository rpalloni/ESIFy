info <-
  bootstrapPage(
    tags$div(class = "row",
      tags$div(class = "col-sm-2 header",
               tags$p("European Structural and Investment Funds opendata visualization")
      )
    ),
    tags$div(class = "row",
      tags$div(class = "col-sm-6 text",
                tags$p("ESIFy is the tool developed as side project of the thesis",
                tags$b("Open Data Management - Advanced methods, tools and visualization for policy making"),
                "This web tool allows to drill down and roll up on ESIF opendata to easily visualize level and performance of investments Europe wide.")
      )
    ),
    tags$div(class = "row",
      tags$div(class = "col-sm-6 text",
               tags$p("The objective of dashboard is to allow wider and deeper analysis of ESIF open data as made available in the ESIF opendata portal")
      )
    )
  )
