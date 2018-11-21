#################################################################################
################################### back-end ####################################
#################################################################################

# Define server functions required to create the output
# Input and output in the function() are LISTS of all the input objects defined in
# the user interface and all the output objects produced by the functions.
# EACH input and output element MUST have a unique ID

shinyServer(
  
  ################################## OP level ##########################################
  
  function(input, output, session){
    
      observeEvent(input$msOPp,{
        updateSelectInput(session, inputId='fundOPp',
                          choices=unique(dfP$fund[dfP$ms==input$msOPp]))
        observeEvent(input$fundOPp,{
          updateSelectInput(session, inputId='titleOPp',
                            choices=unique(dfP$title[dfP$ms==input$msOPp & dfP$fund==input$fundOPp]))
        })
      })
      
      source('functions/visualize/op_level/NestedBarPlotPA.R', local = T)
      source('functions/visualize/op_level/NestedBarPlotTS.R', local = T)
      source('functions/visualize/op_level/NestedBarPlotTO.R', local = T)
      source('functions/visualize/op_level/TableData.R', local = T)
      
  ################################## MS level ##########################################   
      observeEvent(input$msMSp,{
        updateSelectInput(session, 'fundMSp', choices=unique(dfP$fund[dfP$ms==input$msMSp]))
      })
      
      source('functions/visualize/ms_level/NestedBarPlotMS.R', local = T)
      source('functions/visualize/ms_level/NestedBarPlotTO.R', local = T)
      
  ################################## EU level ########################################## 
      
      source('functions/visualize/eu_level/NestedBarPlotEU.R', local = T)
      source('functions/visualize/eu_level/NestedBarPlotTO.R', local = T)
      source('functions/visualize/eu_level/NestedBarPlotTOeuro.R', local = T)
    
  ################################## Indicator ########################################## 

    observeEvent(input$fundInd,{
      updateSelectInput(session, inputId='ToInd',
                        choices=unique(dfR$to[dfR$fund==input$fundInd]))
      observeEvent(input$ToInd,{
        updateSelectInput(session, inputId='codeInd',
                          choices=unique(dfR$ind_code[dfR$fund==input$fundInd & dfR$to==input$ToInd]))
      })
    })
    
    
    source('functions/visualize/ind_level/NestedBarPlotInd.R', local = T)
    source('functions/visualize/ind_level/NestedBarPlotIndRate.R', local = T)
    source('functions/visualize/ind_level/NestedBarPlotEff.R', local = T)
      
  }
)

