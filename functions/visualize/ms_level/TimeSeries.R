output$pTSlineMS <- renderPlotly({
  
  setMS <- input$msOPp
  setFund <- input$fundOPp
  
  # EU
  
  EUI <- dfI %>%
    filter(fund==setFund) %>%
    group_by(year) %>%
    summarise(sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  EUP <- dfP %>%
    filter(fund==setFund) %>%
    summarise(sum(total_amount))
  
  # MS
  
  MSI <- dfI %>%
    filter(ms==setMS & fund==setFund) %>%
    group_by(year) %>%
    summarise(sum(total_eligible_cost, na.rm=T), 
              sum(total_eligible_expenditure, na.rm=T))
  
  MSP <- dfP %>%
    filter(ms==setMS & fund==setFund) %>%
    summarise(sum(total_amount))

  
  dtMS <- cbind('MS',MSI, MSP)
  colnames(dtMS) <- c("Geo","Year", "Selection", "Expenditure", "Planned")
  dtEU <- cbind('EU',EUI, EUP)
  colnames(dtEU) <- c("Geo","Year", "Selection", "Expenditure", "Planned")
  dt <- rbind(dtOP, dtMS, dtEU)
  
  
  dt$valueSelection <- rnd(dt$Selection/dt$Planned)
  dt$valueExpenditure <- rnd(dt$Expenditure/dt$Planned)
  
  ## plot function
  sel <- 
    ggplotly(
      ggplot() +
        # selection
        geom_point(data = dt,
                   aes(x=Year,
                       y=valueSelection,
                       group = Geo,
                       color = Geo,
                       shape = Geo),
                   size=2) +
        geom_line(data = dt,
                  aes(x=Year,
                      y=valueSelection,
                      group = Geo,
                      color = Geo,
                      linetype = Geo),
                  size=1.2) +
        
        scale_color_manual(values=c("#ADD8E6", "orange", "#3C6478")) +
        scale_linetype_manual(values=c("solid", "dashed", "dotted"))+ # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
        scale_shape_manual(values=c(15,16,17)) + 
        
        theme_classic() +
        
        coord_cartesian() +
        
        expand_limits(y = 0) +
        
        scale_y_continuous(labels = percent_format()) +
        
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey')
        ),
      
      tooltip=c("y")
    ) %>%
    layout(
      showlegend = T,
      legend = list(
        orientation = "h", 
        x = 0.3, 
        y = -0.1
      )
    )
  
  exp <- 
    ggplotly(
      ggplot() +
        # expenditure
        geom_point(data = dt,
                   aes(x=Year,
                       y=valueExpenditure,
                       group = Geo,
                       color = Geo,
                       shape=Geo),
                   size=2) +
        geom_line(data = dt,
                  aes(x=Year,
                      y=valueExpenditure,
                      group = Geo,
                      color = Geo,
                      linetype = Geo),
                  size=1.2) +
        
        scale_color_manual(values=c("orange", "#3C6478")) +
        scale_linetype_manual(values=c("dashed", "dotted"))+ # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
        scale_shape_manual(values=c(16,17)) + 
        
        theme_classic() +
        
        coord_cartesian() +
        
        expand_limits(y = 0) +
        
        scale_y_continuous(labels = percent_format()) +
        
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5, colour = 'lightgrey')
        ),
      
      
      tooltip=c("y")
    ) %>%
    layout(
      showlegend = T,
      legend = list(
        orientation = "h", 
        x = 0.3, 
        y = -0.1
      )
    )
  
  subplot(sel, exp, nrows=2, margin = 0.05, heights = c(0.5, 0.5))
  
})