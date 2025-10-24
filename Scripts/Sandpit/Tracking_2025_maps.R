# R map 

library(ggplot2)
library(sf)
library(sp)
library(raster)
library(geodata)
library(maps)
library(mapdata)
library(WorldMapR)
library(data.table)


datpath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data"

EUR <- st_read("C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data/Environmental_data/EEA_shapefiles/Europe_coastline.shp")
EUR <- st_transform(EUR, crs = "EPSG:4326")
EUR <- st_as_sf(EUR)

# UK <- st_crop(EUR, ymin = 48.67, xmin = -15.17, ymax = 62.99,  xmax = 3.38)
map <- st_crop(EUR, ymin = 47.42, xmin = -19.22, ymax = 67,  xmax = 11.0)

ggplot() +
  geom_sf(data = map, fill = "#69b3a2", color = "black")+theme_void()

rm(EUR)

load("C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/ConSci/Data/2025_Storm_Petrel_colonies.Rdata")

load("C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/ConSci/Data/All_2025_Storm_Petrel_GPS_data.Rdata")

datpath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data"
DUNKITTI <- read.csv(paste(datpath, '/Tracks_processed/KITTI/DUN_KITTI_2025_Tracks.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
FHHKITTI <- read.csv(paste(datpath, '/Tracks_processed/KITTI/FHH_KITTI_2025_Tracks.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
SAHKITTI <- read.csv(paste(datpath, '/Tracks_processed/KITTI/SAH_KITTI_2025_Tracks.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)

DUNKITTI <- DUNKITTI %>% dplyr::select("latitude" = Lat, "longitude" = Long, "id" = Tag_ID, "species" = Species, "site" = Site)
FHHKITTI <- FHHKITTI %>% dplyr::select("latitude" = Lat, "longitude" = Long, "id" = Tag_ID, "species" = Species, "site" = Site)
SAHKITTI <- SAHKITTI %>% dplyr::select("latitude" = Lat, "longitude" = Long, "id" = Tag_ID, "species" = Species, "site" = Site)

KITTI <- rbind(DUNKITTI, FHHKITTI, SAHKITTI)
KITTI$id <- as.character((KITTI$id))

rm(DUNKITTI, FHHKITTI, SAHKITTI)

FHHAUK <- read.csv(paste(datpath, '/Tracks_processed/AUK/FHH_AUK_2025_Tracks.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
RAZAUK <- read.csv(paste(datpath, '/Tracks_processed/AUK/RAZ_ORK_2025.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)

FHHAUK <- FHHAUK %>% dplyr::select("latitude" = Latitude, "longitude" = Longitude, "id" = TagID, "species" = Species, "site" = Site)
RAZAUK <- RAZAUK %>% dplyr::select("latitude" = Latitude, "longitude" = Longitude, "id" = TagID, "species" = Species, "site" = Site)

AUK <- rbind(FHHAUK, RAZAUK)
AUK$id <- as.character(AUK$id)

rm(FHHAUK, RAZAUK)


files = list.files(
  path = paste(datpath, '/Tracks_processed/GAN/2025_GAN_Troup_Head', sep = ''),
  pattern = ".*csv$",
  ignore.case = T,
  full.names = T
)

GANTH <- lapply(files, fread)
GANTH <- do.call(rbind.data.frame, GANTH)
GANTH <- GANTH %>% dplyr::select("latitude" = Latitude, "longitude" = Longitude, "id" = Logger_ID, "species" = Species, "site" = Site)
GANTH$species <- rep('GANNE', nrow(GANTH))
GANTH$id <- as.character(GANTH$id)

files = list.files(
  path = paste(datpath, '/Tracks_processed/GAN/2025_GAN_Bass_incubation', sep = ''),
  pattern = ".*csv$",
  ignore.case = T,
  full.names = T
)

GANBR <- lapply(files, fread)
GANBR <- do.call(rbind.data.frame, GANBR)
GANBR <- GANBR %>% dplyr::select("latitude" = Latitude, "longitude" = Longitude, "id" = Logger_ID, "species" = Species, "site" = Site)
GANBR$species <- rep('GANNE', nrow(GANBR))
GANBR$id <- as.character(GANBR$id)

GANFFC <- read.csv('C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/ConSci/Data/GANNEtracksFFC1025.csv', header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
GANFFC <- GANFFC %>% dplyr::select("latitude" = Latitude.decimal, "longitude" = Longitude.decimal, "id" = Device.ID, "species" = Species, "site" = Site)
GANFFC$id <- as.character(GANFFC$id)

bigbird <- rbind(All_Storm_petrels_2025, AUK, KITTI, GANTH, GANFFC, GANBR)
bigbird <- subset(bigbird, latitude > 0)
bigbird$species <- as.factor(bigbird$species)

rm(AUK, GANFFC, GANTH, KITTI, GANBR, All_Storm_petrels_2025)


cols <- c("Gannet"="royalblue4","Kittiwake"="olivedrab3","Storm Petrel"="maroon2","Leach's Storm Petrel"="purple4","Razorbill"="orange","Guillemot"="turquoise3")

p <- ggplot(data = bigbird) +
  geom_sf(data = map, fill = "#69b3a2", color = "black", alpha = 0.5)+theme_void()+
  geom_point(data = subset(bigbird, species == 'GANNE'), aes(x = longitude, y = latitude), color = "royalblue4", size = 0.01, alpha = 0.1)+
  geom_point(data = subset(bigbird, species == 'KITTI'), aes(x = longitude, y = latitude), color = "olivedrab3", size = 0.01, alpha = 0.1)+
  geom_point(data = subset(bigbird, species == 'STOPE'), aes(x = longitude, y = latitude), color = "maroon2", size = 0.01, alpha = 0.4)+
  geom_point(data = subset(bigbird, species == 'LEAPE'), aes(x = longitude, y = latitude), color = "purple4", size = 0.01, alpha = 0.4)+
  geom_point(data = subset(bigbird, species == "RAZOR"), aes(x = longitude, y = latitude), color = "orange", size = 0.01, alpha = 0.3)+
  geom_point(data = subset(bigbird, species == "GUILL"), aes(x = longitude, y = latitude), color = "turquoise3", size = 0.01, alpha = 0.3)+
  scale_colour_manual(name="Species",values=c("Gannet"="royalblue4","Kittiwake"="olivedrab3","Storm Petrel"="maroon2","Leach's Storm Petrel"="purple4","Razorbill"="orange","Guillemot"="turquoise3"))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 14))

p <- ggplot(data = bigbird) +
  geom_sf(data = map, color = "black", alpha = 0.5)+theme_void()+
  geom_point(data = subset(bigbird, species == 'GANNE'), aes(x = longitude, y = latitude, color = "Gannet"), size = 0.01, alpha = 0.1)+
  geom_point(data = subset(bigbird, species == 'KITTI'), aes(x = longitude, y = latitude, color = "Kittiwake"), size = 0.01, alpha = 0.1)+
  geom_point(data = subset(bigbird, species == 'STOPE'), aes(x = longitude, y = latitude, color = "Storm Petrel"), size = 0.01, alpha = 0.4)+
  geom_point(data = subset(bigbird, species == 'LEAPE'), aes(x = longitude, y = latitude, color = "Leach's Storm Petrel"), size = 0.01, alpha = 0.4)+
  geom_point(data = subset(bigbird, species == "RAZOR"), aes(x = longitude, y = latitude, color = "Razorbill"), size = 0.01, alpha = 0.3)+
  geom_point(data = subset(bigbird, species == "GUILL"), aes(x = longitude, y = latitude, color = "Guillemot"), size = 0.01, alpha = 0.3)+
  scale_colour_manual(name="Species",values=cols)+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 14),legend.position = 'inside', legend.position.inside = c(0.9,0.15))+
  guides(colour = guide_legend(override.aes = list(size=10, alpha = 1)))

title <-"C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/ConSci/seabirdtracks_2025_2.png" 
ggsave(title,plot = p, width = 15, height = 15, dpi = 150, units = "in", bg = "white", device='png')
