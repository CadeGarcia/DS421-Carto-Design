---
  title: "Final_project"
author: "Cade Garcia"
date: "2025-08-05"
output: html_document
---

library(shiny)
library(mapgl)
library(mapboxapi)
library(sf)
library(tidycensus)

### Loading Data


# Get population for Hawaii census tracts
oahu_pop <- get_acs(
  geography = "tract",
  state = "HI",
  variables = "B01003_001", # total population
  geometry = TRUE,
  year = 2023
)



#Download from github "final_data" folder
evac_zones <- st_read(here("final_data/Tsunami_Evacuation_All_Zones.geojson"))


#Download from github "final_data" folder
traffic <- st_read(here("final_data/Traffic_Analysis_Zones_Oahu.geojson"))



glimpse(oahu_pop)


### Shiny App Code


# Choices for the dropdown
available_views <- c("Population Density", "Evacuation Zones", "Density and Evac Zones", "Traffic Data")

ui <- page_sidebar(
  title = "Oahu Tsunami Risk Map",
  sidebar = sidebar(
    selectInput("view_choice", "Choose Data to Display:", choices = available_views)
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map", height = "100vh")
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    list(
      pop = oahu_pop,
      zones = evac_zones,
      traffic = traffic
    )
  })
  
  output$map <- renderMaplibre({
    data_list <- filtered_data()
    pop_data <- data_list$pop
    zone_data <- data_list$zones
    traffic_data <- data_list$traffic
    
    map <- maplibre(style = carto_style("positron"))
    
    map <- map |> fit_bounds(pop_data, animate = FALSE)
    
    # Evacuation zone layer
    if (input$view_choice %in% c("Evacuation Zones", "Density and Evac Zones")) {
      map <- map |>
        add_fill_layer(
          id = "evac_zones_layer",
          source = zone_data,
          fill_color = list(
            "match",
            get_column("zone_code"),
            1, "#D96B6B",  # Red for zone 1
            2, "#E5D88F",  # Yellow for zone 2
            3, "#8CCB87",  # Green for zone 3
            "#808080"      # Gray fallback color if none match
          ),
          fill_opacity = 0.7,
          popup = "zone_desc",
          tooltip = "zone_type",
          hover_options = list(
            fill_opacity = 0.5
          )
        )
    }
    
    
    # Population Density layer
    if (input$view_choice %in% c("Population Density", "Density and Evac Zones")) {
      min_val <- min(pop_data$estimate, na.rm = TRUE)
      max_val <- max(pop_data$estimate, na.rm = TRUE)
      
      map <- map |>
        add_fill_layer(
          id = "population_layer",
          source = pop_data,
          fill_color = interpolate(
            column = "estimate",
            values = c(min_val, max_val),
            stops = c("#edf8fb", "#3182bd"),  #6a51a3
            na_color = "lightgrey"
          ),
          fill_opacity = 0.6,
          popup = "popup",
          tooltip = "estimate",
          hover_options = list(
            fill_color = "yellow",
            fill_opacity = 1
          )
        )
    }
    
    if (input$view_choice %in% c("Traffic Data", "All")) {
      # Get min and max of the numeric column you want to color by
      min_val <- min(traffic_data$hawaiistgrdsownertaz_oaharea, na.rm = TRUE)
      max_val <- max(traffic_data$hawaiistgrdsownertaz_oaharea, na.rm = TRUE)
      
      map <- map |>
        add_fill_layer(
          id = "traffic_layer",
          source = traffic_data,
          fill_color = interpolate(
            column = "hawaiistgrdsownertaz_oaharea",
            values = c(min_val, max_val),
            stops = c("#fee5d9", "#de2d26"),  # light pink to red
            na_color = "lightgrey"
          ),
          fill_opacity = 0.5,
          tooltip = "hawaiistgrdsownertaz_oaharea",
          hover_options = list(
            fill_opacity = 0.8
          )
        )
    }
    
    map
  })
}

shinyApp(ui, server)
```


# Choices for the dropdown
available_views <- c("Population Density", "Evacuation Zones", "Density and Evac Zones", "Traffic Data")

ui <- page_sidebar(
  title = "Oahu Tsunami Risk Map",
  sidebar = sidebar(
    selectInput("view_choice", "Choose Data to Display:", choices = available_views)
  ),
  card(
    full_screen = TRUE,
    maplibreOutput("map", height = "100vh"),
    uiOutput("legend_ui")
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    list(
      pop = oahu_pop,
      zones = evac_zones,
      traffic = traffic
    )
  })
  
  output$map <- renderMaplibre({
    data_list <- filtered_data()
    pop_data <- data_list$pop
    zone_data <- data_list$zones
    traffic_data <- data_list$traffic
    
    map <- maplibre(style = carto_style("positron"))
    map <- map |> fit_bounds(pop_data, animate = FALSE)
    
    # Evacuation zone layer
    if (input$view_choice %in% c("Evacuation Zones", "Density and Evac Zones")) {
      map <- map |>
        add_fill_layer(
          id = "evac_zones_layer",
          source = zone_data,
          fill_color = list(
            "match",
            get_column("zone_code"),
            1, "#D96B6B",  # Red for zone 1
            2, "#E5D88F",  # Yellow for zone 2
            3, "#8CCB87",  # Green for zone 3
            "#808080"      # Gray fallback
          ),
          fill_opacity = 0.7,
          popup = "zone_desc",
          tooltip = "zone_type",
          hover_options = list(
            fill_opacity = 0.5
          )
        )
    }
    
    # Population Density layer
    if (input$view_choice %in% c("Population Density", "Density and Evac Zones")) {
      min_val <- min(pop_data$estimate, na.rm = TRUE)
      max_val <- max(pop_data$estimate, na.rm = TRUE)
      
      map <- map |>
        add_fill_layer(
          id = "population_layer",
          source = pop_data,
          fill_color = interpolate(
            column = "estimate",
            values = c(min_val, max_val),
            stops = c("#edf8fb", "#3182bd"),
            na_color = "lightgrey"
          ),
          fill_opacity = 0.6,
          popup = "popup",
          tooltip = "estimate",
          hover_options = list(
            fill_color = "yellow",
            fill_opacity = 1
          )
        )
    }
    
    # Traffic data layer
    if (input$view_choice %in% c("Traffic Data", "All")) {
      min_val <- min(traffic_data$hawaiistgrdsownertaz_oaharea, na.rm = TRUE)
      max_val <- max(traffic_data$hawaiistgrdsownertaz_oaharea, na.rm = TRUE)
      
      map <- map |>
        add_fill_layer(
          id = "traffic_layer",
          source = traffic_data,
          fill_color = interpolate(
            column = "hawaiistgrdsownertaz_oaharea",
            values = c(min_val, max_val),
            stops = c("#fee5d9", "#de2d26"),
            na_color = "lightgrey"
          ),
          fill_opacity = 0.5,
          tooltip = "hawaiistgrdsownertaz_oaharea",
          hover_options = list(
            fill_opacity = 0.8
          )
        )
    }
    
    map
  })
  
  # Dynamic legend
  output$legend_ui <- renderUI({
    if (input$view_choice == "Population Density") {
      tags$div(style = "position:absolute; bottom:10px; left:10px; background:white; padding:8px; border-radius:5px;",
               tags$b("Population Density"),
               tags$div(style = "background:#edf8fb; height:10px; width:60px; display:inline-block; margin-right:5px;"), "Low",
               tags$div(style = "background:#3182bd; height:10px; width:60px; display:inline-block; margin-left:10px;"), "High"
      )
    } else if (input$view_choice == "Evacuation Zones") {
      tags$div(style = "position:absolute; bottom:10px; left:10px; background:white; padding:8px; border-radius:5px;",
               tags$b("Evacuation Zones"), tags$br(),
               tags$div(style = "background:#D96B6B; height:10px; width:20px; display:inline-block;"), " Zone 1", tags$br(),
               tags$div(style = "background:#E5D88F; height:10px; width:20px; display:inline-block;"), " Zone 2", tags$br(),
               tags$div(style = "background:#8CCB87; height:10px; width:20px; display:inline-block;"), " Zone 3"
      )
    } else if (input$view_choice == "Traffic Data") {
      tags$div(style = "position:absolute; bottom:10px; left:10px; background:white; padding:8px; border-radius:5px;",
               tags$b("Traffic Area Size"),
               tags$div(style = "background:#fee5d9; height:10px; width:60px; display:inline-block; margin-right:5px;"), "Small",
               tags$div(style = "background:#de2d26; height:10px; width:60px; display:inline-block; margin-left:10px;"), "Large"
      )
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)

