
# Similar process to reading in GPS data
# Opening logger pressure data:
LGpress <- data.frame()
# Run through every directory 
directoriesN <- directoriesTags

for(n in 1:length(directoriesN)){
  
  #Identify folder with loggername
  # folder <- paste(datpath, "Tag", loggernames[n], "/", sep = "") 
  
  # list all .pos file types in that folder
  files = list.files(
    path = directoriesN[n],
    pattern = ".*press.txt$",
    ignore.case = T,
    full.names = T
  )
  
  if(length(files) > 0) {
    
    # Read in all files in folder and bind into one dataframe
    dataN = rbindlist(lapply(files, fread))
    
    colnames(dataN) <- c('year','month','day','hour','minute','second','temperature','pressure','height_calib')
    
    # # Include logger name and filename as columns - doing this twice to match with basestation data which includes loggername whereas loggername doesnt, 
    # # this means we can do a matching excersise to find duplciates later
    filenameN <- str_split(directoriesN[n], 'Tag', simplify = TRUE)[2]
    dataN$tagID <- rep(filenameN, nrow(dataN))
    # 
    LGpress <- rbind(LGpress, dataN)
  }
  
}

press <- LGpress
press$date_time <- as_datetime(paste(press$year, '/' ,press$month, '/', press$day, '/', press$hour, ':', press$minute, ':', press$second))
press <- press %>% sort_by(press$tagID, press$date_time)

# Look for data rows that are duplciated across logger files
press$dup <- duplicated(press[,1:10])
pressFilt <- subset(press, dup == FALSE)

# Remove highly anomolous pressures: 
pressFilt <- subset(pressFilt, pressure < 9000)

SummPress <- pressFilt %>% group_by(tagID) %>% summarise(
  n = length(tagID),
  mintime = min(date_time),
  maxtime= max(date_time),
  nrefs = length(unique(tagID))
)


Accel <- data.frame()
# Run through every directory 
directoriesN <- directoriesTags

for(n in 1:length(directoriesN)){
  
  #Identify folder with loggername
  # folder <- paste(datpath, "Tag", loggernames[n], "/", sep = "") 
  
  # list all .pos file types in that folder
  files = list.files(
    path = directoriesN[n],
    pattern = ".*Accel.txt$",
    ignore.case = T,
    full.names = T
  )
  
  if(length(files) > 0) {
    
    # Read in all files in folder and bind into one dataframe
    dataN = rbindlist(lapply(files, fread))
    
    colnames(dataN) <- c('year','month','day','hour','minute','second','xacc','yacc','zacc', '3Dacc')
    
    
    
    # # Include logger name and filename as columns - doing this twice to match with basestation data which includes loggername whereas loggername doesnt, 
    # # this means we can do a matching excersise to find duplciates later
    filenameN <- str_split(directoriesN[n], 'Tag', simplify = TRUE)[2]
    dataN$tagID <- rep(filenameN, nrow(dataN))
    # 
    Accel <- rbind(Accel, dataN)
  }
  
}

Accel$date_time <- as_datetime(paste(Accel$year, '/' ,Accel$month, '/', Accel$day, '/', Accel$hour, ':', Accel$minute, ':', Accel$second))

# Look for data rows that are duplciated across logger files
Accel$dup <- duplicated(Accel[,1:12])
Accel <- subset(Accel, dup == FALSE)

# Remove data from prior to attachment
Accel <- filter(Accel, month(date_time) > 6)

SummAccel <- Accel %>% group_by(tagID) %>% summarise(
  n = length(tagID),
  mintime = min(date_time),
  maxtime= max(date_time),
  nrefs = length(unique(tagID))
)
