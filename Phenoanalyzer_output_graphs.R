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
         date=as.Date(datetime),
         camera=as.integer(camera)) %>%
  separate(index,c("colorspace","ROI","Index"), sep = "\\.", extra = "merge", fill = "left", remove =FALSE) %>%
    mutate(across(c(colorspace, ROI, Index),factor))


#view catagories in colorspace, ROI, Index
levels(cam3.pheno.2010.long$colorspace)
levels(cam3.pheno.2010.long$Index)

#create table of colorspace & Index
table1<- unique(cam3.pheno.2010.long[c("colorspace","Index" )])

#graph 1 index & 1 ROI
cam3.pheno.2010.long %>%
  filter(Index =="redDN"& ROI %in% c("ROI_01", "ROI_06")) %>%
  ggplot(.,aes(date,value, color=ROI))+
  geom_line()


#graph 1 index for all ROIs

cam3.pheno.2010.long %>%
  filter(Index =="redDN") %>%
  ggplot(.,aes(date,value, color=ROI))+
  geom_line()


#graph All index in one colorspace for all ROIs

cam3.pheno.2010.long %>%
  filter(Index %in% c("redDN","greenDN","blueDN")) %>%
  ggplot(.,aes(date,value, color=ROI))+
  geom_line()+
  facet_grid(Index~.)

#graph All index in one colorspace for all ROIs

cam3.pheno.2010.long %>%
  filter(colorspace %in% c("RGB")) %>%
  ggplot(.,aes(date,value, color=ROI))+
  geom_line()+
  facet_grid(Index~.,scales = "free_y")+
  labs(y="Index value (unitless)", x="Date")+
  theme_bw(base_size = 10)



