library(tidyverse)
library(maps)
library(sf)
library(dplyr)
library(countrycode)
library(shiny)
library(ggpubr)

# Import the data with coordinates
world_map <- map_data("world")
world_map$region <- countrycode(world_map$region, "country.name", "iso3c")
accesstonet <- read.csv("accesstointernet.csv")
netspeed <- read.csv("worldwideInternetSpeed.csv") #ISO2 code used in dataset!
netprice <- read.csv("worldwideMobileDataPricing2019to2023.csv") #ISO2 code used in dataset!

# Data cleaning functions
clean_data <- function(dataset, year_col, value_cols) {
  dataset %>%
    pivot_longer(
      cols = value_cols,
      names_to = year_col,
      values_to = "values"
    ) %>%
    rename(region = Country.Code)
}

# Clean access data
access_dataset <- clean_data(accesstonet, "Xyear", "X1990:X2022")

# Clean speed data
speed_dataset <- clean_data(netspeed, "Dyear", "D2017:D2023")

# Clean price data
price_dataset <- clean_data(netprice, "Dyear", "D2019:D2023")
price_dataset$values <- as.numeric(gsub('[$,]', '', price_dataset$values))

# Shiny app
ui <- fluidPage(
  titlePanel("Worldwide Internet Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("yearInput", "Select Year", choices = unique(access_dataset$Xyear))
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

server <- function(input, output) {
  observe({
    accessyear <- access_dataset %>% filter(Xyear == input$yearInput)
    sostupid_map <- left_join(world_map, accessyear, by = "region")
    
    speedyear <- speed_dataset %>% filter(Dyear == input$yearInput)
    sohungry_map <- left_join(world_map, speedyear, by = "region")
    
    priceyear <- price_dataset %>% filter(Dyear == input$yearInput)
    sotired_map <- left_join(world_map, priceyear, by = "region")
    
    sostupid_map$values <- as.numeric(sostupid_map$values)
    sohungry_map$values <- as.numeric(sohungry_map$values)
    sotired_map$values <- as.numeric(sotired_map$values)
    
    output$mapPlot <- renderPlot({
      par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))
      mapplotting(sostupid_map, sostupid_map$values, "B")
      mapplotting(sohungry_map, sohungry_map$values, "C")
      mapplotting(sotired_map, sotired_map$values, "C")
    })
  })
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
