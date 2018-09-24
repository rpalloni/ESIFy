# render a data table
output$tData <- renderDataTable({
  if(input$showDataOPp){
    datatable(data = dfI %>% filter(title == input$titleOPp) %>%
                select(title, 
                       total_eligible_cost,
                       total_eligible_expenditure),
              options = list(pageLength = 10),
              rownames = F)
  }
})
