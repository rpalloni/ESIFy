# organize data for prediction geom_ribbon

utilsPred <- function(geo, var, impl, yr){
  
  currentEstimateMS <- impl %>% filter(Geo == geo & Year == yr) %>% select(var) %>% as.numeric()
  currentUpperMS <- currentEstimateMS
  currentLowerMS <- currentEstimateMS
  
  currentEstimateEU <- impl %>% filter(Geo == 'EU' & Year == yr) %>% select(var) %>% as.numeric()
  currentUpperEU <- currentEstimateEU
  currentLowerEU <- currentEstimateEU
  
  tsMS <- impl %>% filter(Geo == geo) %>% select(var) %>% pull()
  futureEstimateMS <- predict(tsMS,1)$mean[1]
  futureUpperMS <- predict(tsMS,1)$upper[2]
  futureLowerMS <- predict(tsMS,1)$lower[2]
  
  tsEU <- impl %>% filter(Geo == 'EU') %>% select(var) %>% pull()
  futureEstimateEU <- predict(tsEU,1)$mean[1]
  futureUpperEU <- predict(tsEU,1)$upper[2]
  futureLowerEU <- predict(tsEU,1)$lower[2]
  
  planMS <- impl %>% filter(Geo == geo & Year == yr) %>% select(Planned) %>% distinct() %>% as.numeric()
  planEU <- impl %>% filter(Geo == 'EU' & Year == yr) %>% select(Planned) %>% distinct() %>% as.numeric()
  
  cd <- data.frame('Geo' = c(geo,'EU'),
                   'Year' = yr, 
                   'Estimate' = c(currentEstimateMS,currentEstimateEU),
                   'Upper' = c(currentUpperMS, currentUpperEU), 
                   'Lower' = c(currentLowerMS, currentLowerEU),
                   'Planned' = c(planMS, planEU))
  fd <- data.frame('Geo' = c(geo,'EU'),
                   'Year' = yr+1,
                   'Estimate' = c(futureEstimateMS,futureEstimateEU),
                   'Upper' = c(futureUpperMS, futureUpperEU), 
                   'Lower' = c(futureLowerMS, futureLowerEU),
                   'Planned' = c(planMS, planEU))
  v <- rbind(cd, fd)
  v$valueEstimate <- v$Estimate/v$Planned
  v$valueUpper <- v$Upper/v$Planned
  v$valueLower <- v$Lower/v$Planned
  v$Year <- as.character(v$Year)
  
  return(v)
}

# utilsPred('Selection', dt, yr)