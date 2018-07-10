# Load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(scales)
library(fiftystater) #might not use
library(usmap)
library(ggthemes)
library(shinythemes)
library(rsconnect)

# Load in data

honey <- read_csv(file = "/Users/John/Documents/Honey Production Dash Shiny/honeyproduction.csv")

# Inspect data

head(honey)
summary(honey)

# Retrieve FIPS codes for each state for plotting regions 
honey$fips <- apply(honey[,c("state")], 1, function(x) fips(x))
states <- unique(honey$state)

# new dataframe with clean titles for display purposes 
cleanColHoney <- honey
colnames(cleanColHoney) <- c("State", "Number of Colonies", "Yield per Colony", "Total Production", "Stocks", "Pricer per Pound", "Value of Total Production", "Year", "Fips")

cleanColHoney <- cleanColHoney[, 1:8]

# add national average price to honey 
avgNatPrice <- honey %>% # get average national price for each year 
  group_by(year) %>%
  summarise(
    avgPrice = mean(priceperlb)
  )

honey <- merge(honey, avgNatPrice, by= "year") # merging dataframes together to get national average in honey


ui <- fluidPage(theme = shinytheme("journal"),
  titlePanel("Save the Bees!"), #add title panel 
  
  h4("Bee Population and Honey Production from 1998 to 2012"), #add secondary heading 
  
  br(), br(),
  
  sidebarLayout(
    sidebarPanel( #define sidebar panel for inserting buttons, dropdowns, sliders, etc. 
      
      sliderInput("yearInput", "Year", min = 1998, max = 2012, value = c(1998), sep = ""),
      
      selectInput("stateInput", "State:", states),
      
      br(), br(), br(), br()
      
    ),
    
    mainPanel( #define main panel portion of page 
      tabsetPanel(
        tabPanel("Honey Production", plotOutput("yearlyProdPlot")),
        tabPanel("Map of Production", plotOutput("productionByState"), tableOutput("stateDetails")),
        tabPanel("Price of Honey", plotOutput("honeyPrice")),
        tabPanel("Yield per Colony", plotOutput("yieldColony"), br(), dataTableOutput("yieldColTable"))
        )
    )
  )
)

server <- function(input, output) {
# U.S. Total Production Chart 
  output$yearlyProdPlot <- renderPlot({ 
    yearlyTotalProduction <- honey %>% #filtering data 
      group_by(year) %>%
      filter(year >= input$yearInput[1]) %>%
      summarise(
        yearly_prod = sum(totalprod, na.rm = TRUE)) 
    # create actual point and line graphs
    ggplot(data = yearlyTotalProduction, mapping = aes(x = year, y = yearly_prod)) +
      geom_point(aes(size = yearly_prod, color = yearly_prod)) +
      geom_line(aes(size = 2)) +
      scale_x_continuous(breaks = round(seq(min(yearlyTotalProduction$year), max(yearlyTotalProduction$year), by = 1.0), 1)) + 
      scale_y_continuous(labels = comma) +
      xlab("Year") +
      ylab("Yearly Production (lbs)") +
      ggtitle("U.S. Total Yearly Production") +
      theme_economist() +
      theme(legend.position = "none") + 
      scale_color_gradient(low="blue", high="red")
    })
  
  # create output for map of us with production of each state 
  output$productionByState <- renderPlot({
    filteredYear <- honey %>%
      filter(year == input$yearInput[1])
    plot_usmap(data = filteredYear, values = "totalprod", lines = "white") +
      scale_fill_continuous(name = "Honey Production by State (lbs)", label = comma) + 
      scale_fill_gradient(low="blue", high="yellow", label = comma) +
      theme(legend.position = "right") +
      guides(fill=guide_legend(title="Production Level"))
  })
  # create table output with input state 
  output$stateDetails <- renderTable({
    cleanColHoney %>%
      filter(
        Year == input$yearInput[1], State == input$stateInput[1]
      )
  })
  # create plot output of honey price by state
  output$honeyPrice <- renderPlot({
    honey %>%
      filter(
        state == input$stateInput[1]
      ) %>%
    ggplot() +
      geom_line(mapping = aes(x = year, y = priceperlb, col = "red")) +
      geom_line(mapping = aes(x=year, y = avgPrice, col = "darkgreen")) +
      scale_x_continuous(breaks = round(seq(min(honey$year), max(honey$year), by = 1.0), 1)) + 
      scale_y_continuous(labels = comma) +
      xlab("Year") +
      ylab("Price per Pound ($)") +
      ggtitle(paste0("Price change for ", input$stateInput[1], "vs. United States Average")) +
      scale_color_manual(labels = c(input$stateInput[1], "Nationally"), values = c("red", "darkgreen")) +
      theme_economist() +
      guides(color = guide_legend(""))
      
  })
  
   # create plot of yield per colony by state
   output$yieldColony <- renderPlot({
     filteredYear <- honey %>%
     filter(year == input$yearInput[1])
   plot_usmap(data = filteredYear, values = "yieldpercol", lines = "white") +
     scale_fill_gradient(name = "Yield per Colony (lbs)", low="blue", high="yellow", label = comma, breaks = c(0,20,40,60,80,100,120)) +
     theme(legend.position = "right") 
   })
   
   #create corresponding table of all states for that particular year
   output$yieldColTable <- renderDataTable({
     cleanColHoney[, c(1,3,8)] %>%
       filter(Year == input$yearInput[1])
   })
  
    
}


shinyApp(ui = ui, server = server)



