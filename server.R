#########################################################
################## Server instructions ##################
#########################################################

shinyServer(function(input, output, session){

  observeEvent(input$ms,{
    updateSelectInput(session, 'title', choices=unique(dfP$title[dfP$ms==input$ms & dfP$fund=="ERDF"]))
  })

  #input$selectedDataMS <- renderUI({
  #levels(dt[,(input$ms)])
  #})
  
  #input$selectedDataOP <- renderUI({
  #levels(dt[,(input$title)])
  #})
  
  source('functions/visualize/NestedBarPlotMS.R', local = T)
  source('functions/visualize/NestedBarPlotPA.R', local = T)
  source('functions/visualize/NestedBarPlotTS.R', local = T)
  source('functions/visualize/NestedBarPlotTO.R', local = T)
        

  }
)

