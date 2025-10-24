library(lubridate)
library(hms)
library(dplyr)

datpath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data"
savepath <- "C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data/Data_external_sharing/"

# Site options: DUN, SAH, FHH
SITE <- 'FHH'
# Species options: KITTI, COGU, RAZO
SPECIES <- 'KITTI'
YEAR <- 2025
Metapath <- paste("C:/Users/ElspethSage/OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS/Documents/BRAIDS/WORKING/Data/Tracks_unprocessed/2025_KITTI_FHH/", SITE, "_", YEAR, "_Metadata.csv", sep = '')


GPS <- read.csv(paste(datpath, '/Tracks_processed/KITTI/FHH_KITTI_2025_Tracks_BRAIDS.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
MetaData <- fread(Metapath)
names(MetaData) <- make.names(names(MetaData))

GPS$date_time <- as_datetime(paste(GPS$Year, '/' ,GPS$Month, '/', GPS$Day, '/', GPS$Hour, ':', GPS$Minute, ':', GPS$Second))

GPS$Date <- as_date(GPS$date_time)
GPS$Time_UTC <- as_hms(GPS$date_time)

Metasub <- MetaData %>% dplyr::select(Tag_ID = GPS.tag.ID,BTO.metal.ring)
Metasub$Tag_ID <- as.integer(Metasub$Tag_ID)

GPS <- inner_join(GPS, Metasub)

GPS <- subset(GPS, select=-c(Day,Month,Hour,Minute,Second, Second_of_day))

GPSout <- data.frame("Species" = rep('Kittiwake', nrow(GPS)),
                       "Site" = rep('Fowlsheugh', nrow(GPS)),
                       "Year" = GPS$Year,
                       "Bird_ID" = vector(mode="character", length=nrow(GPS)),
                       "BTO_Ring" = GPS$BTO.metal.ring,
                       "Logger_ID" = GPS$Tag_ID,
                       "Age" = rep('Adult', nrow(GPS)),
                       "Sex" = rep('unknown', nrow(GPS)),
                       "BreedingStage" =  rep('Chick-rearing', nrow(GPS)),
                       "Date" = GPS$Date,
                       "Time_UTC" = GPS$Time_UTC,
                       "GPS" = rep(1, nrow(GPS)),
                       "GPS_logger_make" = rep('PathTrack', nrow(GPS)),
                       "Latitude" = GPS$Lat,
                       "Longitude" = GPS$Long,
                       "GPS_altitude"  =GPS$Altitude_GPS,
                       "Satellites" = GPS$Satellites,
                       "Speed_ms" = rep(NA, nrow(GPS)),
                       "Accuracy" = GPS$Accuracy,
                       "Interpolated" = rep(0, nrow(GPS)),
                       "TDR" = rep(0, nrow(GPS)),
                       "Barometer" = rep(1, nrow(GPS)),
                       "Accelerometer" = rep(1, nrow(GPS)),
                     "Data_Owner" = rep('RSPB', nrow(GPS)))

GPSout <- subset(GPSout, Satellites >=4)

# GPSout$Accelerometer[which(GPSout$Logger_ID %in% c(35112, 35174))] <- 0
# GPSout$Barometer[which(GPSout$Logger_ID %in% c(35112, 35174))] <- 0

write.csv(GPSout, file = paste(savepath, 'Kittiwake_GPS_Finescale_2025_Fowlsheugh.csv', sep = ''), row.names=FALSE)


# Accel
Accel <- read.csv(paste(datpath, '/Accelerometry_processed/KITTI/FHH_KITTI_2025_Accel.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)

Metasub <- MetaData %>% dplyr::select(TagID = GPS.tag.ID,BTO.metal.ring)
Metasub$TagID <- as.integer(Metasub$TagID)
Accel <- inner_join(Accel, Metasub)
Accel$date_time <- as_datetime(paste(Accel$Year, '/' ,Accel$Month, '/', Accel$Day, '/', Accel$Hour, ':', Accel$Minute, ':', Accel$Second))

Accelout <- data.frame("Species" = rep('Kittiwake', nrow(Accel)),
                     "Site" = rep('Fowlsheugh', nrow(Accel)),
                     "Year" = Accel$Year,
                     "Bird_ID" = vector(mode="character", length=nrow(Accel)),
                     "BTO_Ring" = Accel$BTO.metal.ring,
                     "Logger_ID" = Accel$TagID,
                     "Age" = rep('Adult', nrow(Accel)),
                     "Sex" = rep('unknown', nrow(Accel)),
                     "BreedingStage" =  rep('Chick-rearing', nrow(Accel)),
                     "Date" = as_date(Accel$date_time),
                     "Time_UTC" = as_hms(Accel$date_time),
                     "Accelerometer" = rep(1, nrow(Accel)),
                     "Accelerometer_Sampling_Rate" = rep('1 Hz', nrow(Accel)),
                     "Accelerometer_Sampling_Info" = rep('
Recorded accelerometery constantly throughout the deployment', nrow(Accel)),
                     "Accelerometer_logger_make" = rep('Pathtrack Picofix', nrow(Accel)),
                     "x_acceleration" = Accel$Xaccel,
                     "y_acceleration" = Accel$Yaccel,
                     "z_acceleration" = Accel$Zaccel,
                     "ThreeD_acceleration" = Accel$X3Daccel,
                     "TDR" = rep(0, nrow(Accel)),
                     "Barometer" = rep(1, nrow(Accel)),
                     "Data_Owner" = rep('RSPB', nrow(Accel))
                     )

write.csv(Accelout, file = paste(savepath, 'Kittiwake_Fowlsheugh_Accel_2025_Finescale.csv', sep = ''), row.names=FALSE)


Press <- read.csv(paste(datpath, '/Pressure_processed/KITTI/FHH_KITTI_2025_Pressure.csv', sep = ''), header = TRUE, sep = ",", stringsAsFactors=FALSE, row.names = NULL)
Press <- inner_join(Press, Metasub)
Press$date_time <- as_datetime(paste(Press$Year, '/' ,Press$Month, '/', Press$Day, '/', Press$Hour, ':', Press$Minute, ':', Press$Second))

Pressout <- data.frame("Species" = rep('Kittiwake', nrow(Press)),
                       "Site" = rep('Fowlsheugh', nrow(Press)),
                       "Year" = Press$Year,
                       "Bird_ID" = vector(mode="character", length=nrow(Press)),
                       "BTO_Ring" = Press$BTO.metal.ring,
                       "Logger_ID" = Press$TagID,
                       "Age" = rep('Adult', nrow(Press)),
                       "Sex" = rep('unknown', nrow(Press)),
                       "BreedingStage" =  rep('Chick-rearing', nrow(Press)),
                       "Date" = as_date(Press$date_time),
                       "Time_UTC" = as_hms(Press$date_time),
                       "Barometer" = rep(1, nrow(Press)),
                       "Barometer_Sampling_Rate" = rep('1 Hz', nrow(Press)),
                       "Barometer_Sampling_Info" = rep('
Recorded barometry constantly throughout the deployment', nrow(Press)),
                       "Barometer_logger_make" = rep('Pathtrack Picofix', nrow(Press)),
                       "Temperature" = Press$Temperature, 
                       "Pressure" = Press$Pressure,
                       "Temperature_Units" = rep('Celcius', nrow(Press)),
                       "Pressure_Units" = rep('hPa', nrow(Press)),
                       "TDR" = rep(0, nrow(Press)),
                       "Accelerometer" = rep(1, nrow(Press)),
                       "Data_Owner" = rep('RSPB', nrow(Press)))

                       
                       
write.csv(Pressout, file = paste(savepath, 'Kittiwake_Fowlsheugh_BaromPress_2025_Finescale.csv', sep = ''), row.names=FALSE)
