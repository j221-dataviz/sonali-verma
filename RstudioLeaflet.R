# install and load leaflet and rdgal
install.packages("leaflet")
install.packages("rgdal")
library(leaflet)
library(rgdal)

# make leaflet map centered on Berkeley
leaflet() %>% 
  setView(lng = -84.270020	, lat = 37.8393, zoom = 7) %>%
  addTiles()

# make leaflet map centered on Berkeley with Carto tiles
leaflet() %>%
  setView(lng = -84.270020, lat = 37.8393, zoom = 8) %>%
  addProviderTiles("CartoDB.Positron")

# load seismic risk shapefile
seismic <- readOGR("seismic", "seismic")

# load quakes data from USGS earthquakes API
quakes <- read_csv("https://earthquake.usgs.gov/fdsnws/event/1/query?starttime=1960-01-01T00:00:00&minmagnitude=6&format=csv&latitude=39.828175&longitude=-98.5795&maxradiuskm=6000&orderby=magnitude")

# view summary of seismic_risk data
summary(seismic)


# load required package
library(forcats)

# load required package
library(forcats)

# convert to factor/categorical variable
seismic$ValueRange <- as.factor(seismic$ValueRange)

# reorder the levels of the factor
seismic$ValueRange <- fct_relevel(seismic$ValueRange, c("< 1","1 - 2","2 - 5","5 - 10", "10 - 12"))

# view summary of seismic_risk data
summary(seismic)


# load the seismic risk data into a leaflet map
seismic_map <- leaflet(data=seismic)

# set color palette
pal <- colorFactor("Reds", seismic$ValueRange)

# plot map
seismic_map %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7,
    smoothFactor = 0.1,
    color = ~pal(ValueRange)
  ) %>%
  # add historical earthquakes
  addCircles(
    data = quakes, 
    radius = sqrt(10^quakes$mag)*30, 
    color = "#000000",
    weight = 0.2,
    fillColor ="#ffffff",
    fillOpacity = 0.5,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y"))
  )


# set color palette
pal <- colorFactor("Reds", seismic$ValueRange)

# plot map
seismic_map %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7,
    smoothFactor = 0.1,
    color = ~pal(ValueRange)
  ) %>%
  # add historical earthquakes
  addCircles(
    data = quakes, 
    #ScalingCirclesToMachData
    radius = sqrt(10^quakes$mag)*30, 
    color = "#000000",
    weight = 0.2,
    fillColor ="#ffffff",
    fillOpacity = 0.5,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y"))
  )


# make multi-layered leaflet map with layer-switching control
seismic_final <- seismic_map %>%
  setView(lng = -98.5795, lat = 39.828175, zoom = 4) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # make choropleth map of seismic hazards
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.7,
    smoothFactor = 0.1,
    color = ~pal(ValueRange)
  ) %>%
  # add historical earthquakes
  addCircles(
    data = quakes, 
    radius = sqrt(10^quakes$mag)*30, 
    color = "#000000",
    weight = 0.2,
    fillColor ="#ffffff",
    fillOpacity = 0.3,
    popup = paste0("<strong>Magnitude: </strong>", quakes$mag, "</br>",
                   "<strong>Date: </strong>", format(as.Date(quakes$time), "%b %d, %Y"))
  )
addLegend(
  "bottomright", pal = pal, values = ~ValueRange,
  title = "Seismic risk",
  opacity = 0.7
) %>%
  # add layers control
  addLayersControl(
    baseGroups = c("CartoDB", "Toner"),
    overlayGroups = "Quakes",
    options = layersControlOptions(collapsed = FALSE)
  )

# plot map
print(seismic_final)

# save the map
saveWidget(seismic_final, "seismic.html", selfcontained = TRUE, libdir = NULL, background = "white")