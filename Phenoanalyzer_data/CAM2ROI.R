# add libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(readxl)

# set working directory to SEL shared file with file lists

setwd("Z:/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists")


# READ ALL NOON FILES & MERGE INTO SINGEL DATA FRAME W/ FILE NAME AS COLUMN
noonfiles <- list.files(path="Z:/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists",
                        full.names=TRUE, pattern= "[0-9]{4}_noon_cam[0-9]{1}.txt") %>%
  #  map_dfr(read.csv, .id="source") %>% 
  map_dfr(function(x) read.csv(x) %>% 
            mutate(filename=gsub(".csv","",basename(x)))) 

##############ALTERNATIVE CODE FOR STEPS 1 BY 1 ######################### 
# # list multiple files
# # create a string for the data path
# data_path <- "Z:/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists"
# 
# # list all the files with file name only, not full path
# noonfilenames <- list.files(path="Z:/Research Data/Desert/Jornada/Bahada/Phenocam/TwrPhenocam/lists",
#                             full.names=FALSE, pattern= "[0-9]{4}_noon_cam[0-9]{1}.txt")
# 
# # use purrr:map to read all lists and combine with file names
# # https://clauswilke.com/blog/2016/06/13/reading-and-combining-many-tidy-data-files-in-r/
# noonfiles <- data_frame(filename = noonfilenames) %>% # create a data frame
#   # holding the file names
#   mutate(file_contents = map(filename, ~read.csv(file.path(data_path,.), header=TRUE))) # a new data column
############################################
# Issue resolved and list renamed with _backup
#### brightness is a factor in some files, check where the issue is
## check str of all noon lists
#noonfiles[1,2] %>%
#map(str)
## go to specific lists that have brightness as a factor
#check2<- unnest(noonfiles[1,2], cols=c(file_contents))
# check3<- unnest(noonfiles2[13,2], cols=c(file_contents))
####

# # unnest only the files which have brightness as a number. 2010 and 2013 files for cam 1 have an issue
# noon <- noonfiles %>%
#   filter(!filename %in% c("2010_noon_cam1.txt" , "2013_noon_cam1.txt"))%>%
#   unnest(., cols=c(file_contents)) 

# put timestamp into POSIXct format (date/time format for R)
noonfiles <- noonfiles %>% # 'pipe command' which allows sequential exectution
  mutate(timestamp2 = ymd_hms(timestamp),
         timestamp3 = format(timestamp2, "%Y_%m%d_%H%M%S"))


find.multpic<- count.images %>% filter(n >1)

#save info on mult pics
# count.images %>%
#   select(filename, camera,date,n) %>%
#   write.csv(.,"C:/Users/wwbeamon/Desktop/reupheno/files_multpics.csv", row.names = FALSE)

# create list with only 1st noon pic
noonfiles.1<- noonfiles %>% 
  mutate(date = date(timestamp2))  %>%
  #  group_by(filename, date) %>%
  distinct(filename,date,.keep_all = TRUE)

#based on photos, 1st excluded bright/dark images, then select 1st pic of noon
# create list with only 1st noon pic
noonfiles.2<- noonfiles %>% 
  mutate(date = date(timestamp2))  %>%
  filter(brightness >75&brightness<200) %>%
  distinct(filename,date,.keep_all = TRUE)

cam2_batch <- read_xlsx("C:/Users/wwbeamon/Desktop/reupheno/CAM2_batch_notes.xlsx")

#merge cam3_batch with noonfiles.2 & fill batch# between dates.

cam2_batchfiles <- left_join(noonfiles.2,cam2_batch, by=c("filename","date"))

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



setwd("C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data")
# READ ALL NOON FILES & MERGE INTO SINGEL DATA FRAME W/ FILE NAME AS COLUMN

data.2010.cam2.b1 <- read.csv("data_2010_cam2_batch1.csv", stringsAsFactors = TRUE, na.strings = c("none"))
data.2010.cam2.b2 <-read.csv("data_2010_cam2_batch2.csv", stringsAsFactors = TRUE,na.strings = c("none"))

test22<- rbind(data.2010.cam2.b1,data.2010.cam2.b2)


test23 <- list.files(path = "C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data",pattern = "cam2", full.names = TRUE) %>% 
  lapply(read.csv) %>%                                          
  reduce(rbind)

test24<- left_join(data.2010.cam2.b1,data.2010.cam2.b2, by=c("img", "idx_label"))

test25<- do.call(rbind,lapply(list.files(path = "C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data",,pattern = "cam2", full.names = TRUE), 
                              read.csv))

list.files(path ="C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data", full.names = TRUE,path="cam2") 

                  


        
        
        