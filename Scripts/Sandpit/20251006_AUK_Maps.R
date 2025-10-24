# Mapping auks guilles with leaflet

scriptpath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Scripts/Sandpit/"
datpath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data/Tracks_processed/AUK/"
savepath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Outputs/Sandpit_outputs/20251006_AUKS/"

library(lubridate)
library(leaflet)
library(leaflet.extras2)
library(dplyr)
library(sp)
library(sf)
library(stringr)
require(htmlwidgets)
library(stringr)
library(data.table)
library(ggplot2)


GPS1 <- read.csv(paste(datpath, 'RAZ_FHH_2025.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
GPS2 <- read.csv(paste(datpath, 'RAZ_ORK_2025.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)

GPS1 <- GPS1[,1:17]
GPS2 <- GPS2[,1:17]

GPS <- rbind(GPS1, GPS2)

GPS$longitude <- GPS$Long
GPS$latitude <- GPS$Lat

GPS <- subset(GPS, latitude > 0)

GPS$DateTime <- as_datetime(paste(GPS$Year, '/' ,GPS$Month, '/', GPS$Day, '/', GPS$Hour, ':', GPS$Minute, ':', GPS$Second))

GPS <- GPS[order(GPS$DateTime),]

ifelse(!dir.exists(paste(savepath, 'Leaflet_maps', sep = '')), dir.create(paste(savepath, '/Leaflet_maps', sep = '')), "Folder exists already")

# Lef map of all
lefplot <- GPS
# lefplot <- subset(lefplot, Logger == unique(lefplot$Logger)[16])
class_names <- unique(lefplot$TagID)

pal2 <- colorFactor(palette = c("green","blue","red","purple","orange","magenta","yellow", "navy","black"),domain = class_names)

split_data = lapply(unique(lefplot$TagID), function(x) {
  df = as.matrix(lefplot[lefplot$TagID == x, c("longitude", "latitude")])
  lns = Lines(Line(df), ID = x)
  return(lns)
})

data_lines = SpatialLines(split_data)



lef <- leaflet(lefplot) %>%
  addTiles() %>%
  addPolylines(data = data_lines, fillOpacity = 0.6, color = "gray") %>%
  addCircles(data = lefplot, lat = ~ latitude, lng = ~ longitude, color = ~pal2(TagID), fillOpacity = 0.6, popup = paste(" Timestamp = ",lefplot$date_time," individual =", lefplot$TagID))


lef
saveWidget(lef, file=paste(savepath, 'Leaflet_maps/RAZ_FHH_ORK_2025',".html", sep = ''))
