#script to visualize & anaylyze processed phenoanalyzer data

#add libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(readxl)

setwd("C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data")

#read csv 

cam3.pheno.2010 <- read.csv("data_2010_cam3_batch1.csv")

#view col names
colnames(cam3.pheno.2010)

#pivot wide to long

cam3.pheno.2010.long <- cam3.pheno.2010 %>%
  pivot_longer(cols = -c("img", "idx_label"), names_to = "index")

#split img col into camera# and date cols. format index col into info 
cam3.pheno.2010.long<- cam3.pheno.2010.long %>%
  filter(img != "none") %>%
  separate(img, c("img1",NA), sep = ".jpg", remove = FALSE) %>%
  separate(img1,c(NA,"camera","date.info"), sep = "_", extra = "merge", fill = "left") %>%
  mutate(datetime= ymd_hms(date.info),
         date=as.Date(datetime)) %>%
  separate(index,c("colorspace","ROI","Index"), sep = "\\.", extra = "merge", fill = "left", remove =FALSE)
