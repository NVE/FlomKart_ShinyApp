# Function for mapping all stations or groups of stations

# MAP OF NORWAY WITH COLOR CHANGING ACCORDING TO THE NUMBER OF DATA.
norway_map4server <- function(selected.station) {
  library(leaflet)
  library(magrittr)
  
  st.index <- which(station$number == selected.station)
  st.name <- station$name[st.index]
  st.long <- station$long[st.index]
  st.lat <- station$lat[st.index]
  st.length_rec <- station$length_rec[st.index]
  
  #   pal <- colorNumeric(
  #     palette = heat.colors(5),
  #     domain = c(0,30,60,90,120,150))
  # qpal <- colorQuantile("RdYlBu", length.bins, n = 5)
  
  my.colors <- c("black", "red", "orange", "green", "blue")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0,30,60,90,120,150)
    color <- my.colors[trunc(x2plot/30)+1]
    invisible(color)
  }
  
  
  map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) 
  
  addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(station$name), "Number:", station$number,
                                 "Length of record:", station$length_rec, sep = " "), radius = 5, 
                   color = ~my.color.func(station$length_rec, my.colors), stroke = FALSE, fillOpacity = 0.5,
                   layerId = station$number) %>%
    
    addPopups(st.long, st.lat, paste("Name:", as.character(st.name), "Number:", selected.station,
                                     "Length of record:", st.length_rec, sep = " "),
              options = popupOptions(closeButton = FALSE, maxWidth = 100)) %>%
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
              title = "Length of flood record (years)",
              opacity = 1)
  
}


norway_map4groups <- function(group.index) {
  library(leaflet)
  library(magrittr)
  
  print(group.index)
  
  group.name <- station$name[group.index]
  group.nve_nb <- station$nve_nb[group.index]
  group.long <- station$long[group.index]
  group.lat <- station$lat[group.index]
  group.length_rec <- station$length_rec[group.index]
  
  my.colors <- c("black", "red", "orange", "green", "blue")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0,30,60,90,120,150)
    color <- my.colors[trunc(x2plot/30)+1]
    invisible(color)
  }
  
  
  map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) 
  
  addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(station$name), "Number:", station$nve_nb,
                                 "Length of record:", station$length_rec, sep = " "),
                   radius = 5, 
                   color = ~my.color.func(station$length_rec, my.colors), stroke = FALSE, fillOpacity = 0.5) %>%
    
    addMarkers(group.long, group.lat, popup = paste("Name:", as.character(group.name), "Number:", group.nve_nb,
                                                    "Length of record:", group.length_rec, sep = " ")) %>%
    
    #     addPopups(group.long, group.lat, 
    #                paste("gev"),
    #               options = popupOptions(closeButton = FALSE, maxWidth = 1)) 
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
              title = "Length of flood record (years)",
              opacity = 1)
  
}