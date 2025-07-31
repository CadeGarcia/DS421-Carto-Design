library(shiny)
library(mapgl)
library(mapboxapi)
library(sf)

# Property coordinates
property_coords <- c(12.492303960986625, 41.890537124009725)

# Create sf object for the property
property_sf <- st_as_sf(data.frame(
  id = "apt1",
  name = "New Class A Apartment",
  lon = property_coords[1],
  lat = property_coords[2]
), coords = c("lon", "lat"), crs = 4326)

# Generate isochrone polygon using Mapbox API
isochrone <- mb_isochrone(property_coords, profile = "driving", time = 20)

# UI
ui <- fluidPage(
  tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
  story_map(
    map_id = "map",
    font_family = "Poppins",
    sections = list(
      "intro" = story_section(
        title = "One of the 7 Wonders of the World",
        content = list(
          p("Rome's one and only Colosseum"),
          img(src = "colo.jpeg", width = "300px")
        ),
        position = "center"
      ),
      "marker" = story_section(
        title = "History still standing",
        content = list(
          p("Located in the center of the city, it surely is a sight to behold.")
        )
      ),
      "isochrone" = story_section(
        title = "Explore Rome!",
        content = list(
          p("You don't go to Rome just for the Colosseum, check out its surroundings too!")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$map <- renderMapboxgl({
    mapboxgl(
      scrollZoom = FALSE,
      center = c(12.492303960986625, 41.890537124009725),
      zoom = 12
    )
  })
  
  on_section("map", "intro", {
    mapboxgl_proxy("map") |>
      clear_layer("property_layer") |>
      clear_layer("isochrone") |>
      fly_to(
        center = c(12.492303960986625, 41.890537124009725),
        zoom = 12,
        pitch = 0,
        bearing = 0
      )
  })
  
  on_section("map", "marker", {
    proxy <- mapboxgl_proxy("map")
    
    proxy |>
      clear_layer("isochrone") |>
      add_source(id = "property", data = property_sf) |>
      add_circle_layer(
        id = "property_layer",
        source = "property",
        circle_color = "#CC5500",
        circle_radius = 10,
        circle_opacity = 0.8,
        popup = "name"
      ) |>
      fly_to(
        center = property_coords,
        zoom = 17,
        pitch = 45,
        bearing = -90
      )
    
  })
  
  on_section("map", "isochrone", {
    mapboxgl_proxy("map") |>
      add_fill_layer(
        id = "isochrone",
        source = isochrone,
        fill_color = "#CC5500",
        fill_opacity = 0.5
      ) |>
      fit_bounds(
        isochrone,
        animate = TRUE,
        duration = 8000,
        pitch = 40
      )
  })
}

# Run the app
shinyApp(ui, server)

