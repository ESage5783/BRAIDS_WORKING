###Read and fuse Pathtrack .pos files
##By: Saskia Wischnewski (SW)
##Date: 27/09/23
##Last edit: 17/10/23 (SW)

#will need plyr, dplyr, tidyr

#set working directory to folder with .pos files

Files<-data.frame(id=seq(1,65)) #change number (65) to number of files in folder

fileslist<-list.files( pattern = ".pos") #make list of all file names in folder

Files$Names<-fileslist #turn files list into vector of intial data frame

#i=1

PTList<-list() #make empty list object to save data from loop

#loop for reading and processing individual data files into one data frame  

for(i in 1:nrow(Files)) {
  
  PT<-readLines(print(Files$Names[i])) #read file
  PT1<-PT[6:length(PT)] #get rid of first few lines
  
  writeLines(PT1,"test.csv") #save file as csv
  data<-read.csv("test.csv", header=F, sep=",")#read in as csv again
  
  data$TagID<-substr(Files$Names[i], 21,25) #add column/vector with tag ID
  
  PTList[[i]]<-data #save to empty list file
  
}

AllPT<-do.call(rbind.data.frame, PTList) #turn into data frame

colnames(AllPT)<- c("Day","Month","Year", "Hour", "Minute", "Second", "Var1", "Satellites", "Latitude", "Longitude", "Height", "Clock_offset", "Var2", "Voltage", "Var3", "Var4", "TagID")

#Add/change column names

AllPT$Species<-"KITTI" #Add Species column, you could also add a site column here if you have multiple sites and are fusing all files per site

write.csv(AllPT, "AllPT_KITTI_2023.csv") #Save as .csv