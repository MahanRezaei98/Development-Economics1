
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# Load the shapefile and rename the province column
iran_map <- st_read("irn_admbnda_adm1_unhcr_20190514.shp")
TC_province1 <- read.csv("TC_province1.csv")
relative_line <- read.csv("relative_line.csv")
hybrid_poverty_line <- read.csv("hybrid_poverty_line.csv")
income_poverty_line <- read.csv("income_poverty_line.csv")
poverty_gap_index <- read.csv("poverty_gap_index.csv")
MDP_index_province <- read.csv("MDP_index_province.csv")
Gini_province <- read.csv("Gini_province.csv")

iran_map <- rename(iran_map, province = ADM1_EN)

# Function to generate the Leaflet map
generateLeafletMap <- function(data, metric, metric_label) {
  # Set labels for map
  labels <- sprintf("<strong>%s</strong><br/>%s: %s",
                    data$province,
                    metric_label,
                    format(data[[metric]], big.mark = ",", scientific = FALSE))
  
  # Create the color palette
  pal <- colorNumeric(palette = "PuBu", domain = data[[metric]])
  
  # Generate the map
  leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(data[[metric]]),
      fillOpacity = 1,
      weight = 0.3,
      smoothFactor = 0.3,
      label = lapply(labels, htmltools::HTML),
      labelOptions = labelOptions(
        style = list("font-family" = "serif", "font-weight" = "bold", "padding" = "3px 8px",
                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                     "font-size" = "13px", "border-color" = "rgba(0,0,0,0.5)"),
        direction = "auto"
      ),
      highlightOptions = highlightOptions(color = "darkred", weight = 3, bringToFront = TRUE)
    ) %>%
    addLegend(
      pal = pal,
      values = data[[metric]],
      position = "topright",
      title = metric_label)}

# UI
ui <- fluidPage(
  titlePanel("Atlas Poverty and Inequality of Iran"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedProvince", "Choose a Province",
                  choices = c("Iran" = "All", unique(iran_map$province))),
      selectInput("selectedDataset", "Choose a Measure:",
                  choices = c(
                    "Absolute Poverty Line" = "TC_province1",
                    "Relative Poverty Line (Traditional)" = "relative_line",
                    "Relative Poverty Line (Hybrid)" = "hybrid_poverty_line",
                    "Income Poverty Line" = "income_poverty_line",
                    "Poverty Gap Index" = "poverty_gap_index",
                    "Multidimensional Poverty Index" = "MDP_index_province",
                    "Gini Coefficient" = "Gini_province"
                  )),
      selectInput("selectedMetric", "Choose an Approach:", choices = NULL),
      selectInput("urbanRural", "Choose an Area Type:",
                  choices = c("All", "Urban", "Rural"),
                  selected = "All"),
      actionButton("resetMap", "Reset View"),
      width = 3
    ),
    mainPanel(
      leafletOutput("mapDisplay", height = "800px"),
      width = 9)))

# Server
server <- function(input, output, session) {
  # Update the 'Choose an Approach' dropdown dynamically based on selected dataset
  observe({
    selected_dataset <- input$selectedDataset
    
    metric_choices <- switch(selected_dataset,
                             "TC_province1" = c("Method 1" = "First_Method", "Method 2" = "Second_Method"),
                             "relative_line" = c("50 % of Median" = "Half_Median"),
                             "hybrid_poverty_line" = c("Cost of bottom 20%" = "cost_new_bundle"),
                             "income_poverty_line" = c("Method 1" = "income_poverty_line1", "Method 2" = "income_poverty_line2"),
                             "poverty_gap_index" = c("Method 1" = "PGI_1", "Method 2" = "PGI_2"),
                             "MDP_index_province" = c("Equally Weighted MPI" = "weighted_average_MPI"),
                             "Gini_province" = c("Gini (HEIS Data)" = "Gini_Coef_HEIS", "Gini (Welfare Data)" = "Gini_Coef_Welfare"))
    
    updateSelectInput(session, "selectedMetric", choices = metric_choices)
    
    # Update the 'Choose an Area Type' dropdown based on selected dataset
    if (selected_dataset %in% c("MDP_index_province", "Gini_province")) {
      updateSelectInput(session, "urbanRural", choices = c("All"), selected = "All")
    } else {
      updateSelectInput(session, "urbanRural", choices = c("All", "Urban", "Rural"), selected = "All")}})
  
  # Render the Leaflet map
  output$mapDisplay <- renderLeaflet({
    req(input$selectedMetric)
    
    selected_dataset <- input$selectedDataset
    metric <- input$selectedMetric
    metric_label <- names(which(sapply(switch(selected_dataset,
                                              "TC_province1" = c("First Method" = "First_Method", "Second Method" = "Second_Method"),
                                              "relative_line" = c("Half of Median Income" = "Half_Median"),
                                              "hybrid_poverty_line" = c("Cost of New Bundle" = "cost_new_bundle"),
                                              "income_poverty_line" = c("Method 1" = "income_poverty_line1", "Method 2" = "income_poverty_line2"),
                                              "poverty_gap_index" = c("PGI Method 1" = "PGI_1", "PGI Method 2" = "PGI_2"),
                                              "MDP_index_province" = c("Weighted Average MPI" = "weighted_average_MPI"),
                                              "Gini_province" = c("Gini (HEIS Data)" = "Gini_Coef_HEIS", "Gini (Welfare Data)" = "Gini_Coef_Welfare")
    ), identical, metric)))
    
    # Filter dataset
    data <- switch(selected_dataset,
                   "TC_province1" = TC_province1,
                   "relative_line" = relative_line,
                   "hybrid_poverty_line" = hybrid_poverty_line,
                   "income_poverty_line" = income_poverty_line,
                   "poverty_gap_index" = poverty_gap_index,
                   "MDP_index_province" = MDP_index_province,
                   "Gini_province" = Gini_province)
    
    # Merge with shapefile
    filtered_data <- merge(iran_map, data, by = "province")
    
    # Apply filters for area type and province
    if (input$urbanRural != "All" && !(selected_dataset %in% c("MDP_index_province", "Gini_province")))
    {
      filtered_data <- filtered_data[filtered_data$Urban_Rural == input$urbanRural, ]
    }
    if (input$selectedProvince != "All") {
      filtered_data <- filtered_data[filtered_data$province == input$selectedProvince, ]}
    
    # Generate the map
    generateLeafletMap(filtered_data, metric, metric_label)})
  
  # Reset map view
  observeEvent(input$resetMap, {
    updateSelectInput(session, "selectedProvince", selected = "All")
    updateSelectInput(session, "urbanRural", selected = "All")
    leafletProxy("mapDisplay", session) %>%
      setView(lng = 110, lat = 32, zoom = 4)})}


# Run the app
shinyApp(ui = ui, server = server)