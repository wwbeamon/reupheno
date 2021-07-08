#all years Cam3
#add libraries
library(lubridate)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(readxl)

setwd("C:/Users/wwbeamon/Desktop/reupheno/Phenoanalyzer_data")

#read csv 
Cam3.pheno.wRef<- read.csv("CAM3_ROI_wRef.csv")

#pivot wide to long
Cam3.pheno.wRef<- Cam3.pheno.wRef %>%
  pivot_longer(cols = -c("img", "idx_label"), names_to = "index")

#split img col into camera# and date cols. format index col into info 
Cam3.pheno.wRef<- Cam3.pheno.wRef %>%
  filter(img != "none") %>%
  separate(img, c("img1",NA), sep = ".jpg", remove = FALSE) %>%
  separate(img1,c(NA,"camera","date.info"), sep = "_", extra = "merge", fill = "left") %>%
  mutate(datetime= ymd_hms(date.info),
         date=as.Date(datetime),
         camera=as.integer(camera)) %>%
  separate(index,c("colorspace","ROI","Spectral.Data"), sep = "\\.", extra = "merge", fill = "left", remove =FALSE) %>%
  mutate(across(c(colorspace, ROI, Spectral.Data),factor))

#Remove "ROI" from ROI_#
Cam3.pheno.wRef<- Cam3.pheno.wRef %>%
  separate(ROI, c(NA, "ROI"), sep = "_") %>%  
  mutate(across(c(ROI),factor))
  

#plot with 1 select Spectral.Data & ROI, 
Cam3.pheno.wRef %>%
  filter(Spectral.Data =="nNDVI" & ROI %in% c("01")) %>%
  ggplot(.,aes(date,value, color= ROI)) +
  geom_line()+
#with facet grid
  facet_grid(.~year(date), scales = "free_x")

#mult vars w/facet grid

Cam3.pheno.wRef %>%
  filter(Spectral.Data == "2G_RBi" & year(date)== 2010:2020 & ROI %in% c("01"))  %>%
  ggplot(.,aes(date,value))+
  geom_line(linetype="dotted")+
  facet_grid(Spectral.Data~., scales = "free_y")

Cam3.pheno.wRef %>%
  filter(Spectral.Data %in% c("2G_RBi","nNDVI") & year(date)== 2011:2020 & ROI %in% c("01"))  %>%
  ggplot(.,aes(date,value, color=ROI,))+
  geom_line(color="darkgreen")+
  scale_x_date(date_breaks = "3 month", date_labels = "%b") +
  facet_grid(Spectral.Data~ year(date), scales = "free")+
theme(axis.text.x = element_text(angle = 90))

#spline?

Cam3.pheno.wRef %>%
  filter(Spectral.Data %in% c("2G_RBi","nNDVI") & year(date)== 2017:2019 & ROI %in% c("01","02","03","04","05","06"))  %>%
  ggplot(.,aes(date,value, color=ROI))+
  geom_line()+
  facet_grid(Spectral.Data~., scales = "free_y")
#w/facet grid

#white boards
Cam3.pheno.wRef %>%
  filter(Spectral.Data %in% c("2G_RBi","nNDVI") & year(date)== 2020 & ROI %in% c("11","12"))  %>%
  ggplot(.,aes(date, value, color= ROI)) +
  geom_line() +
  facet_grid(Spectral.Data~., scales = "free_y")

#bareground
Cam3.pheno.wRef %>%
  filter(Spectral.Data == "totalRGB" & ROI %in% c("07","08","09","10"))  %>%
  ggplot(.,aes(date, value, color= ROI)) +
  geom_line()               
               
#mountains
Cam3.pheno.wRef %>%
  filter(Spectral.Data == "Hue" & ROI %in% c("13","14","15","16"))  %>%
  ggplot(.,aes(date, value, color= ROI)) +
  geom_line()

