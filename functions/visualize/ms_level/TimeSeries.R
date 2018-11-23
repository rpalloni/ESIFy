output$pTSlineMS <- renderPlotly({
  
  setMS <- input$msMSp
  setFund <- input$fundMSp
  
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

  
  # current values
  dtMS <- cbind(setMS,MSI, MSP)
  colnames(dtMS) <- c("Geo","Year", "Selection", "Expenditure", "Planned")
  
  dtEU <- cbind('EU',EUI, EUP)
  colnames(dtEU) <- c("Geo","Year", "Selection", "Expenditure", "Planned")
  
  dt <- rbind(dtMS, dtEU)
  
  dt$valueSelection <- rnd(dt$Selection/dt$Planned)
  dt$valueExpenditure <- rnd(dt$Expenditure/dt$Planned)
  yr <- as.numeric(max(dt$Year))
  
  # future values
  dfs <- utilsPred(setMS, 'Selection', dt, yr)
  dfe <- utilsPred(setMS, 'Expenditure', dt, yr)
  
  
  ## plot function
  
  # selection
  sel <- 
    ggplotly(
      ggplot() + 
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
        geom_line(data = dfs,
                  aes(x=Year,
                      y=valueEstimate,
                      group = Geo,
                      color = Geo),
                  size=1) +
        geom_ribbon(data = dfs,
                   aes(x=Year, 
                       ymin = valueLower, 
                       ymax = valueUpper, 
                       group = Geo,
                       color = Geo), alpha = .25) +
        
        scale_color_manual(values=c( "orange", "#3C6478")) +
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
    ) 
  
  # expenditure
  exp <- 
    ggplotly(
      ggplot() +
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
        geom_line(data = dfe,
                  aes(x=Year,
                      y=valueEstimate,
                      group = Geo,
                      color = Geo),
                  size=1) +
        geom_ribbon(data = dfe,
                    aes(x=Year, 
                        ymin = valueLower, 
                        ymax = valueUpper, 
                        group = Geo,
                        color = Geo), alpha = .25) +
        
        scale_color_manual(values=c( "orange", "#3C6478")) +
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
      annotations = list(
        list(x = 0.5,
             y = 2.2,
             text = 'Rate of selection (% of planned)',
             showarrow = F,
             xref='paper',
             yref='paper'),
        list(x = 0.5,
             y = 0.8,
             text = 'Rate of expenditure (% of planned)',
             showarrow = F,
             xref='paper',
             yref='paper')
      ),
      showlegend = T,
      legend = list(
        orientation = "h",
        x = 0.3,
        y = -0.1
      )
    )
  
  subplot(sel, exp, nrows=2, margin = 0.05, heights = c(0.5, 0.5))
  
})