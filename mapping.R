# Function for mapping all stations or groups of stations

# MAP OF NORWAY WITH COLOR CHANGING ACCORDING TO THE NUMBER OF DATA.
norway_map4server <- function(selected.station) {

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

  print("in norway_map4groups function")
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
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-30", "30-60", "60-90", "90-120", "120-150"),
              title = "Length of flood record (years)",
              opacity = 1)
  
}

# Function to map the fitted parameter values as color scale for each station
norway_map4param_values <- function(distr, method, param) {

  distr.index <- which(distr.name == distr)
  method.index <- which(method.name == method)
  param <- as.numeric(param)

  param.vector <- c()
  for (i in station$index) {
    param.vector <- c(param.vector, var.get.nc(nc, "param.estimate", 
                               start = c(i, distr.index, method.index, param, 30, 1), 
                               count = c(1, 1, 1, 1, 1, 1))) 
  }
  
  if (length(param.vector) > 0) {
  
  my.colors <- c("black", "gray", "brown", "red", "orange", "purple", "blue",  "cyan", "green", "pink")
  # my.colors <- heat.colors(10)
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- seq(min(x2plot), max(x2plot), length.out = 11)
     color.vector <- my.colors[trunc(( x2plot - min(x2plot) ) / (color.bins[2] - color.bins[1])) + 1]
    invisible(color.vector)
  }
  color.bins <- seq(min(na.omit(param.vector)), max(na.omit(param.vector)), length.out = 11)
  
  
  map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) 
  
  addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
                   popup = paste("Name:", as.character(station$name), "Number:", station$number,
                                 "Param value:", param.vector, sep = " "), radius = 5, 
                   color = ~my.color.func(na.omit(param.vector), my.colors), stroke = FALSE, fillOpacity = 0.5,
                   layerId = station$number) %>%
    
    addLegend(position = "bottomright", colors = my.colors, labels = c("0-0.1", "0.1-0.2", "0.2-0.3", 
                                                                       "0.3-0.4", "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"),
              title = "Parameter values, relative to min/max interval",
              opacity = 1)
  } else {map <- leaflet() %>% addTiles()
  setView(map, 13, 64, zoom = 5) }
  
  
}

# Histogram to go with the parameter mapping
histo4param_values <- function(distr, method, param) {
  
  distr.index <- which(distr.name == distr)
  method.index <- which(method.name == method)
  param <- as.numeric(param)
  
  param.vector <- c()
  for (i in station$index) {
    param.vector <- c(param.vector, var.get.nc(nc, "param.estimate", 
                                               start = c(i, distr.index, method.index, param, 30, 1), 
                                               count = c(1, 1, 1, 1, 1, 1))) 
  }
  if (length(param.vector) > 0) {
    hist(param.vector)
  }

}


######################## quick hack to optimise later


norway_map4groups_tab1 <- function(group.index) {
  
  print("in norway_map4groups function")
  print(group.index)
  
  group.name <- station$name[group.index]
  group.nve_nb <- station$nve_nb[group.index]
  group.long <- station$long[group.index]
  group.lat <- station$lat[group.index]
  group.length_rec <- station$length_rec[group.index]
  
  my.colors <- c("black", "red", "orange", "green", "blue")
  
  my.color.func <- function(x2plot, my.colors) {
    color.bins <- c(0,500,1000,1500,2000,2500)
    color <- my.colors[trunc(x2plot/500)+1]
    invisible(color)
  }

# #   # experiment with shape file FAILED  
#   catchments_shape <- raster::shapefile("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Lavvann/GISDATA/norge.shp")
#   
#      catchments_shape <- raster::shapefile("//nve/fil/h/HM/Interne Prosjekter/Flomkart/Lavvann/GISDATA/Hydrologi_TotalNedborfeltMalestasjon.shp")
#   map <- leaflet(data = catchments_shape) %>%  addTiles() %>%
#     setView(13, 64, zoom = 5)  %>%  
#     addPolygons(fill = FALSE, stroke = TRUE, color = "#03F")
  
  
# WORKS, BUT AM I STEALING A LINK?   
  map <-leaflet() %>% setView(13, 64, zoom = 5)  %>%  addWMSTiles(
    "http://wms.geonorge.no/skwms1/wms.topo2",
    layers = "topo2_WMS",
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    tileOptions(tms = TRUE),
    attribution = "Kartverket")
 
# # DOESNT WORK  
#     map <-leaflet() %>% setView(13, 64, zoom = 5)  %>%  addWMSTiles(
#     "http://wms.skogoglandskap.no/cgi-bin/clc?language=nor&",
#     layers = "0",
#     options = WMSTileOptions(format = "image/png", transparent = TRUE),
#     tileOptions(tms = TRUE),
#     attribution = "Kartverket")
  
#   map <- leaflet() %>% addTiles()
#   setView(map, 13, 64, zoom = 5) 
#   
#   addCircleMarkers(map, data = station, lng = ~ long, lat = ~ lat, 
#                    popup = paste("Name:", as.character(station$name), "Number:", station$nve_nb,
#                                  "Length of record:", station$length_rec, sep = " "),
#                    radius = 5, 
#                    color = ~my.color.func(station$catchment.size, my.colors), stroke = FALSE, fillOpacity = 0.5) %>%
#     
#     addMarkers(group.long, group.lat, popup = paste("Name:", as.character(group.name), "Number:", group.nve_nb,
#                                                     "Length of record:", group.length_rec, sep = " ")) %>%
#     
#     addLegend(position = "bottomright", colors = my.colors, labels = c("0-500", "500-1000", "1000-1500", "1500-2000", "2000-2500"),
#               title = "Length of flood record (years)",
#               opacity = 1)
  
}
