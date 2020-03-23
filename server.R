library(shiny)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(stringr)


coordinates <- read.csv("LTM_Sample_Coordinates.csv")
coordinates <- coordinates[complete.cases(coordinates),]
colnames(coordinates) <- c("lat", "lng", "popup", "ALSC")
lakedata <- read.csv("LakeData.csv")
colnames(lakedata)[1] <- "confirmedDate"
lakedata$ALSC <- str_sub(lakedata$ALSC,2, str_length(lakedata$ALSC))
lakedata$confirmedDate <- as.Date(lakedata$confirmedDate, "%Y.%m.%d")
dateValues <- base::unique(lakedata$confirmedDate)
measurementValues <- colnames(lakedata)[8:ncol(lakedata)]
coordinates_and_data <- merge(coordinates, lakedata, by = "ALSC")
library(data.table)
coord_data_dt <- data.table(coordinates_and_data)

shinyServer(function(input, output) {
  
  
  
  
  output$map1 <- renderLeaflet({
    
    
    
    lakeDataSelected <- coordinates_and_data[coordinates_and_data$confirmedDate == input$dateInput,]
    require(RColorBrewer)
    colors <- rev(brewer.pal(9, 'YlOrRd'))
    selectedVariable <- lakeDataSelected[,colnames(lakeDataSelected)
                                         == input$variableInput]
    
    colors <- cut(selectedVariable, breaks = 9, labels = colors)
    cutLabels <- cut(selectedVariable, breaks = 9)
    cutLabels <- paste(input$variableInput, "between", levels(cutLabels))
    filteredData <- coordinates_and_data[coordinates_and_data$confirmedDate == input$dateInput,]
    
    require(leaflet)
    latlon <- filteredData[,2:3] 
    labels <- paste0(filteredData$popup, ", ", 
                     input$variableInput, " = ", selectedVariable, 
                     ", Date ", filteredData$confirmedDate)
    
    adkLakes <- leaflet() 
    latlon %>%
      leaflet() %>%
      addTiles() %>%
      addCircleMarkers(popup=labels, color = colors, opacity = 1, clusterOptions = markerClusterOptions()) %>%
      addLegend(labels = cutLabels, 
                colors = rev(brewer.pal(9, 'YlOrRd')))
    
  })
  output$timeSeries1 <- renderPlot({
    
    
    tsDT <- data.frame(coord_data_dt[, mean(eval(parse(text = input$variableInput))), by = confirmedDate])
    ggplot(tsDT, aes(confirmedDate, V1)) + geom_line() +
      ylab("Average of All Measurements") + 
      geom_smooth(method='lm', na.rm=TRUE)
    
  })
  
  
  
  output$timeSeries2 <- renderPlot({
    
    tsDF2 <- subset(coord_data_dt, 
                    popup == input$locationInput & 
                      confirmedDate >= input$startDateInput &
                      confirmedDate <= input$endDateInput)
    
    ggplot(tsDF2, aes(confirmedDate, eval(parse(text = input$variableInput2)))) + geom_line() +
      ylab(input$variableInput2) + 
      geom_smooth(method='lm', na.rm=TRUE)
    
  })
  
  
  
  output$textOutput1 <- renderText({
    
    tsDF2 <- subset(coord_data_dt, 
                    popup == input$locationInput & 
                      confirmedDate >= input$startDateInput &
                      confirmedDate <= input$endDateInput)
    
    fit <- lm(eval(parse(text = input$variableInput2)) ~ confirmedDate, tsDF2)
    changePerYear <- fit$coefficients[2] * 365.25
    paste0("For this time period for this location, the selected metric is trending a ", 
           format(round(changePerYear, 4)), " change per year. The p-value 
           for the linear model fitting the variable to time is ", 
           format(round(summary(fit)[4][[1]][2,4], 10)), ".")
  })
  
  
  
})
© 2020 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
Pricing
API
Training
Blog
About
