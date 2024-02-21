#install.packages('shiny')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('readr')
#install.packages("leaflet", type = "binary")
#install.packages("leaflet.extra", type = "binary")
#install.packages('plotly')

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(plotly)


# Load data for Disaster Declarations
data_og <- read.csv("https://raw.githubusercontent.com/yrcho2k/stat-projects/main/updated_DisasterDeclarationsSummaries.csv")
data_df <- read.csv("https://raw.githubusercontent.com/yrcho2k/stat-projects/main/updated_cleaned_data.csv")

# Define UI
ui <- fluidPage(
  navbarPage(
    title = "COVID-19 Cases Comparison",
    
    # Page 1: Heatmap of COVID-19 Cases
    tabPanel("COVID-19 Cases Heatmap",
             titlePanel("COVID-19 Cases Heatmap"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("stateSelect", 
                             "Select State(s):", 
                             choices = unique(data_df$state), 
                             selected = NULL, 
                             multiple = TRUE),
                 checkboxInput("weekend", "Compare by Weekends", value = FALSE)
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    ),
    
    # Page 2: Comparison of COVID Cases Over Time
    tabPanel("COVID Cases Over Time",
             titlePanel("COVID Cases Over Time by State"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("stateSelectTime", 
                             "Select State(s):", 
                             choices = unique(data_df$state), 
                             selected = NULL, 
                             multiple = TRUE),
                 checkboxInput("weekendTime", "Compare by Weekends", value = FALSE)
               ),
               mainPanel(
                 plotOutput("timeComparisonPlot")
               )
             )
    ),
    
    # Page 3: Comparison of Total COVID Cases by State
    tabPanel("Total Cases by State",
             titlePanel("Total COVID Cases by State"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("stateSelectTotal", 
                             "Select State(s):", 
                             choices = unique(data_df$state), 
                             selected = NULL, 
                             multiple = TRUE)
               ),
               mainPanel(
                 plotOutput("stateComparisonPlot")
               )
             )
    ),
    
    # Page 4: Dominant Disaster Type by State
    tabPanel("Dominant Disaster Type",
             titlePanel("Dominant Disaster Type by State"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("incidentTypeSelect", 
                             "Select Incident Type:", 
                             choices = unique(data_og$incidentType), 
                             selected = "Biological")
               ),
               mainPanel(
                 leafletOutput("mapDisaster")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Filtered data reactive expression for COVID-19 cases
  filteredData <- reactive({
    data <- data_df
    if(input$weekend) {
      # Filter for weekends
      data <- data %>% mutate(weekday = weekdays(DeclarationDate)) %>%
        filter(weekday %in% c('Saturday', 'Sunday'))
    }
    
    if(!is.null(input$stateSelect)) {
      # Filter for selected states
      data <- data %>% filter(state %in% input$stateSelect)
    }
    
    data
  })
  
  # Heatmap render for COVID-19 cases
  output$map <- renderLeaflet({
    req(filteredData())  # Ensure filteredData is available before rendering
    
    # Plotly heatmap for COVID-19 cases
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~cases,
                 blur = 20, max = 0.05, radius = 15) %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4) # Center of the USA
  })
  
  # Plot for comparing COVID-19 cases over time
  output$timeComparisonPlot <- renderPlot({
    data <- filteredData()
    ggplot(data, aes(x = DeclarationDate, y = cases, group = state, color = state)) +
      geom_line() +
      labs(title = "COVID Cases Over Time by State", x = "Date", y = "Number of Cases") +
      theme_minimal()
  })
  
  # Plot for comparing total COVID-19 cases by state
  output$stateComparisonPlot <- renderPlot({
    data <- filteredData() %>%
      group_by(state) %>%
      summarize(totalCases = sum(cases))
    ggplot(data, aes(x = reorder(state, totalCases), y = totalCases, fill = state)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Total COVID Cases by State", x = "State", y = "Total Cases") +
      theme_minimal()
  })
  
  # Filter data for selected incident type
  filtered_data <- reactive({
    data_og %>%
      filter(incidentType == input$incidentTypeSelect)  
  })
  
  # Aggregate data to find dominant disaster type for each state
  dominant_disaster <- reactive({
    filtered_data() %>%
      group_by(state, incidentType) %>%
      summarise(count = n()) %>%
      arrange(state, desc(count)) %>%
      slice(1)  # Get the dominant disaster type for each state
  })
  
  # Create geographic visualization for dominant disaster type
  output$mapDisaster <- renderLeaflet({
    leaflet(data = dominant_disaster()) %>%
      addTiles() %>%
      addMarkers(data = dominant_disaster(), 
                 lng = data_df$longitude, 
                 lat = data_df$latitude, 
                 popup = ~paste(state, "<br>", incidentType, "<br>", count))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
