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
ISOCODESAREGGFKINGKILLME <- c("iso3") #keeps track of what iso code we're on and im so sorry i couldnt be bothered to figure out how to tag it onto word_map itself
accesstonet <- read.csv("accesstointernet.csv")
netspeed <- read.csv("worldwideInternetSpeed.csv") #ISO2 code used in dataset!
netprice <- read.csv("worldwideMobileDataPricing2019to2023.csv") #ISO2 code used in dataset!

#data cleaning for access to net
access_dataset <- accesstonet %>%
  pivot_longer(
    cols = D1990:D2022,
    names_to = "Dyear",
    values_to = "values") %>%
  rename(region = Country.Code)

#data cleaning for speed of net
speed_dataset <- netspeed %>%
  pivot_longer(
    cols = D2017:D2023,
    names_to = "Dyear",
    values_to = "values") %>%
  rename(region = Country.Code)

#data cleaning for price of net
price_dataset <- netprice %>% 
  pivot_longer(
    cols = D2019:D2023,
    names_to = "Dyear",
    values_to = "values") %>%
  rename(region = Country.Code)

price_dataset$values <- gsub('[$,]', '', price_dataset$values) 
world_map$region <- countrycode(world_map$region, "iso3c", "iso2c") #RESET ISO CODES
ISOCODESAREGGFKINGKILLME <- "iso2c"

#group = group connects the points in the correct order
mapplotting <- function(wee, dayear, colourscheme) {
  if(identical(wee, access_dataset)) { 
    if (ISOCODESAREGGFKINGKILLME == "iso2c") { #its a global variable so its fine
      world_map$region <- countrycode(world_map$region, "iso2c", "iso3c")
      ISOCODESAREGGFKINGKILLME == "iso3c"
    } else {
      ISOCODESAREGGFKINGKILLME == "iso3c"
    }
  } else {
    if (ISOCODESAREGGFKINGKILLME == "iso3c") {
      world_map$region <- countrycode(world_map$region, "iso3c", "iso2c")
      ISOCODESAREGGFKINGKILLME == "iso2c"
    } else {
      ISOCODESAREGGFKINGKILLME == "iso3c"
    }
  }
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
