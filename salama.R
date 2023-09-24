library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)

ua <- httr::user_agent("https://tuijasonkkila.fi")
base_url <- "http://opendata.fmi.fi/wfs?service=WFS&version=2.0.0&request=getFeature&storedquery_id="
query_id <- "fmi::observations::lightning::simple&"

# Helsinki
query_parms <- "bbox=24.49234,60.07597,25.16979,60.27435&starttime=2023-09-22T00:00:00Z&endtime=2023-09-24T00:00:00Z"

# Get data
url <- httr::modify_url(paste0(base_url, query_id, query_parms))
resp <- httr::GET(url, ua)

# Parse content
content <- xml2::read_xml(resp$content)
xml <- XML::xmlParse(content)
XML::xmlRoot(xml) # to view

# Extract information
# Note that time is given in Z ("zulu time"), local time is +3 hours
time <- XML::xpathSApply(xml, '//BsWfs:BsWfsElement[ends-with(@gml:id, "1")]/BsWfs:Time', XML::xmlValue)
location <- XML::xpathSApply(xml, '//BsWfs:BsWfsElement[ends-with(@gml:id, "1")]/BsWfs:Location/gml:Point/gml:pos', XML::xmlValue)
peak_current <- XML::xpathSApply(xml, '//BsWfs:BsWfsElement[ends-with(@gml:id, "2")]/BsWfs:ParameterValue', XML::xmlValue)

# Values to a data frame
data <- data.frame(time, location, peak_current)

# Separate location data
lightnings <- data %>% 
  mutate(lat = str_extract(location, "^[^\\s]+"),
         lon = str_extract(location, "\\s.+")) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon),
         peak_current = as.numeric(peak_current)) %>% 
  select(-location)

# Map
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 23px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Neljä salamaa Helsingissä 23.9.2023 klo 10:28:23")
)  

leaflet(lightnings) %>% 
  addTiles(attribution = 'FMI open data | Tuija Sonkkila') %>%
  addTiles() %>% 
  addCircleMarkers(
    radius = ~scales::rescale(peak_current, c(5,40)),    
    stroke = TRUE, weight = 4, color = "black", fillOpacity = 0.5, fillColor = "orange",
    label = ~paste0(peak_current, " kA")
  ) %>% 
  addControl(title, position = "topleft", className="map-title")
