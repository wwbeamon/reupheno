#############################################################################################
# This code is to read lists of noon photo images for Phenocam and format for PhenoAnalyzer #
#     written by: M. Mauritz, 20 May 2021                                                   #
#############################################################################################

# add libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

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

#count images
count.images<- noonfiles %>%
  separate(filename, c(NA, NA, "camera"),"_", extra = "drop", fill = "left", remove = FALSE) %>%
  mutate(date = date(timestamp2))  %>%
  group_by(camera, date) %>%
  #mutate(count=count(filename))
 # count(camera, date)
  add_tally()

ggplot(count.images, aes(date,n))+
  geom_point()+
  facet_grid(camera~year(date), scales = "free_x")

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
  
count.images1<- noonfiles.1 %>%
  separate(filename, c(NA, NA, "camera"),"_", extra = "drop", fill = "left", remove = FALSE) %>%
    group_by(camera, date) %>%
  #mutate(count=count(filename))
  # count(camera, date)
  add_tally()

ggplot(count.images1, aes(date,n))+
  geom_point()+
  facet_grid(camera~year(date), scales = "free_x")

# save only filepath and timestamp2 for PhenoAnalyzer to read lists
# on days with mult noon pics save only first pic. 
# save data as txt by filename using group_walk, use filename in the saved name
# https://luisdva.github.io/rstats/export-iteratively/
# https://community.rstudio.com/t/map-write-csv/33292/2
# https://stackoverflow.com/questions/41233173/how-can-i-write-dplyr-groups-to-separate-files

setwd("C:/Users/wwbeamon/Desktop/reupheno/noon_list_phenoanalyzer")


noonfiles.1 %>%
   tidyr::separate(filename,c("filename2",NA),".txt", extra = "drop", fill = "right") %>%
group_by(filename2)%>%
  select(full.path, timestamp3)%>%
  group_walk(~write.table(.x, file= paste(.y$filename2,
                                          "PhenoAnalyzer.txt",sep="_"),
                          sep =',', dec='.', row.names=FALSE, col.names=FALSE,quote=FALSE)) 


