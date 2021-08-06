# add libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(readxl)

# set working directory to SEL shared file with file lists
setwd("C:/Users/wwbeamon/Desktop/reupheno/noon_list_phenoanalyzer_batch")
setwd("C:/Users/wwbeamon/Desktop/reupheno/noon_list_phenoanalyzer")


#load file of noon list for all years from all cams
setwd("C:/Users/wwbeamon/Desktop/reupheno")
noonfiles.all<- read.table("2010_2021_allcams_noon.txt", sep=",",header = FALSE,col.names = c("full.path","timestamp3","date","filename"))

noonfiles.all<- noonfiles.all %>%
  mutate(date = ymd(date))

#looked through photos & determined processing batches based on camera FOV shift

cam2_batch <- read_xlsx("C:/Users/wwbeamon/Desktop/reupheno/CAM2_batch_notes.xlsx")

#merge cam2_batch with noonfiles.all & fill batch# between dates.

cam2_batchfiles <- left_join(noonfiles.all,cam2_batch, by=c("filename","date"))

cam2_batchfiles <- cam2_batchfiles  %>% 
  fill(batch_process,batch_seq)

#exclude -9999 in batch_process & save new list

setwd("C:/Users/wwbeamon/Desktop/reupheno/noon_list_phenoanalyzer_batch")

cam2_batchfiles %>%
  tidyr::separate(filename, c(NA, NA, "camera"),"_", extra = "drop", fill = "left", remove = FALSE) %>%
  separate(camera,c("camera2",NA),".txt", extra = "drop", fill = "right") %>%
  filter(camera2 %in% c("cam2")& batch_process!=-9999) %>%
  tidyr::separate(filename,c("filename2",NA),".txt", extra = "drop", fill = "right") %>%
  group_by(filename2, batch_process)%>%
  select(full.path, timestamp3)%>%
  group_walk(~write.table(.x, file= paste(.y$filename2,
                                          "_PhenoAnalyzer_batch_",.y$batch_process,".txt", sep=""),
                          sep =',', dec='.', row.names=FALSE, col.names=FALSE,quote=FALSE)) 

#save batch number combine years
#CHANGE CAM# HERE
targetcam <- "cam2"
cam2_batchfiles %>%
  tidyr::separate(filename, c(NA, NA, "camera"),"_", extra = "drop", fill = "left", remove = FALSE) %>%
  separate(camera,c("camera2",NA),".txt", extra = "drop", fill = "right") %>%
  filter(camera2 %in% c(targetcam)& batch_process!=-9999) %>%
  tidyr::separate(filename,c("filename2",NA),".txt", extra = "drop", fill = "right") %>%
  group_by(camera2,batch_process)%>%
  select(full.path, timestamp3)%>%
  group_walk(~write.table(.x, file= paste(.y$camera2,
                                          "_PhenoAnalyzer_batch_",.y$batch_process,".txt", sep=""),
                          sep =',', dec='.', row.names=FALSE, col.names=FALSE,quote=FALSE)) 



setwd("C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data")
# READ ALL NOON FILES & MERGE INTO SINGEL DATA FRAME W/ FILE NAME AS COLUMN

#combine all CAM2 batches 
# list files
files<- list.files(path = "C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data",pattern = "Cam2", full.names = TRUE) 
# read content of csv
data <- files %>% 
  lapply(read.csv) 
#list filenames
filenames<- basename(files)      
#map filename to file and create batchfile column
data_filename<- map2(filenames,data,~cbind(batchfile=.x,.y))
#binding files into single dataframe
CAM2_ROI_wRef<- data_filename %>%
  reduce(rbind)           

#arrange dates of files by idx_label and filter no image dates           
CAM2_ROI_wRef<- CAM2_ROI_wRef %>%
  arrange(idx_label) %>%
  filter(img != "none")

# pivot to long
Cam2.pheno.wRef<- CAM2_ROI_wRef %>%
  pivot_longer(cols = -c("img", "idx_label","batchfile"), names_to = "index")

#split img col into camera# and date cols. format index col into info 
Cam2.pheno.wRef<- Cam2.pheno.wRef %>%
  separate(img, c("img1",NA), sep = ".jpg", remove = FALSE) %>%
  separate(img1,c(NA,"camera","date.info"), sep = "_", extra = "merge", fill = "left") %>%
  mutate(datetime= ymd_hms(date.info),
         date=as.Date(datetime),
         camera=as.integer(camera)) %>%
  separate(index,c("colorspace","ROI","Spectral.Data"), sep = "\\.", extra = "merge", fill = "left", remove =FALSE) %>%
  mutate(across(c(colorspace, ROI, Spectral.Data),factor))        

#Remove "ROI" from ROI_#
Cam2.pheno.wRef<- Cam2.pheno.wRef %>%
  separate(ROI, c(NA, "ROI"), sep = "_") %>%  
  mutate(across(c(ROI),factor)) 
 
#plot
Cam2.pheno.wRef %>%
  filter(Spectral.Data =="nNDVI" & ROI %in% c("01","02","03","04","05")) %>%
  ggplot(.,aes(date,value, color= ROI)) +
  geom_line()+
  geom_smooth(method = , se = FALSE) +
  facet_grid("ROI")

        