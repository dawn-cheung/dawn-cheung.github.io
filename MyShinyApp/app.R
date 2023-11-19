library(tidyverse)
library(maps)
library(dplyr)
library(countrycode)
library(shiny)
library(ggpubr)
library(plotly)

# Import the data with coordinates
world_map <- map_data("world")
world_map$region <- countrycode(world_map$region, "country.name", "iso3c")
#ALL DATASETS CHANGED TO ISO3
access_dataset <- read.csv("accesstointernet.csv")
speed_dataset <- read.csv("worldwideInternetSpeed.csv")
price_dataset <- read.csv("worldwideMobileDataPricing2019to2023.csv")

price_dataset$values <- gsub('[$,]', '', price_dataset$values) 

#group = group connects the points in the correct order
mapplotting <- function(wee, dayear, colourscheme) {
  weep <- wee %>% filter(Dyear == paste0("D", dayear))
  weep$values <- as.numeric(weep$values)
  mapdataset <- left_join(world_map, weep, by = "region")
  ggplot(mapdataset, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = values), color = "white", size = 0.04) +
    scale_fill_viridis_c(option = colourscheme) +
    theme(plot.margin = margin(0, 0, 0, 0, "cm"))
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Comparing Variations in Access, Speed, and Price of Internet, Over Time"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("ACCESS: Individuals using the Internet (% of population)", "SPEED: Mean download speed (Mbps)", "PRICE: Average price of 1GB (in USD)")),
      
      sliderInput(inputId = "year",
                  label = "Choose the year:",
                  min = 1990, max = 2022, value = 2020, sep = ""),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      plotlyOutput("view", width = "80%")
    )
  )
)
server <- function(input, output, session) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "ACCESS: Individuals using the Internet (% of population)" = access_dataset,
           "PRICE: Average price of 1GB (in USD)" = price_dataset, #2019-2023
           "SPEED: Mean download speed (Mbps)" = speed_dataset) #2017 to 2023
  })
  observe({
    if (identical(datasetInput(),access_dataset)) {
      minnie = 1990L
      maxxie = 2022L
    } else if (identical(datasetInput(),price_dataset)) {
      minnie = 2019L
      maxxie = 2023L
    } else if (identical(datasetInput(),speed_dataset)){
      minnie = 2017L
      maxxie = 2023L
    }
    updateSliderInput(
      session = session,
      inputId = "year",
      min = minnie,
      max = maxxie
    )
  })
  output$view <- renderPlotly({
    ggplotly(mapplotting(datasetInput(), as.character(input$year), "B"))
  }) %>% bindCache(datasetInput(), input$year)
  
  shinyOptions(cache = cachem::cache_disk("./cache"))
}
shinyApp(ui, server)
