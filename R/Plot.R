# Create figures for manuscript

rm(list=ls())

library(readxl)
library(dplyr)
library(sf)
library(prettymapr)
library(raster)
library(viridis)
library(lubridate)


## Read in data
#_________________________

# contact tracing forms
animal_ct <- as.data.frame(read_excel("data/Animal_Contact_Tracing_Form_-_all_versions_-_False_-_2025-03-24-13-22-00.xlsx"))
human_ct <- as.data.frame(read_excel("data/Human_Contact_Tracing_Form_KH_edit.xlsx"))

# investigation forms
animal_inv <- as.data.frame(read_excel("data/Animal_Investigation_Form_KH_edit.xlsx"))
human_inv <- as.data.frame(read_excel("data/Human_Investigation_Form_-_all_versions_-_False_-_2025-03-24-13-22-16.xlsx"))

# Grace clean file
cases <- read.csv("data/cases_combined.csv")

# Shapefiles
Nigeria <- read_sf("data/GIS/Nigeria/nga_admbnda_adm0_osgof_20190417.shp")
LGAs <- read_sf("data/GIS/Nigeria_LGAs/nga_admbnda_adm2_osgof_20190417.shp")
wards <- read_sf("data/GIS/Nigeria_-_Ward_Boundaries/Nigeria_-_Ward_Boundaries.shp")
states <- read_sf("data/GIS/Nigeria_states/nga_admbnda_adm1_osgof_20190417.shp")

# Health facility locations
health_facilities <- read.csv("data/Human health facilities & coordinates.csv")


## Subset and add variables for plotting
#_________________________

# Add number of investigations to health facilities
health_facilities <- health_facilities[order(health_facilities$Name.of.Facility),]
health_facilities$investigations <- table(human_inv$`_1_Name_of_Health_facility`)

# Dates
# For human exposures we use date bitten
# For human deaths we use date symptoms (ideally would be death date..)
# For suspect/confirmed rabid dogs we use date bitten (ideally would be date symptoms - also need to check with Grace whether this is the date the dog was bitten or the date it bit someone)
human_ct$date_plot <- as.Date(human_ct$Date_bitten)
human_ct$month_plot <- 12*(year(human_ct$date_plot)-2023) + month(human_ct$date_plot) - 3 # started in April 2023
human_ct$date_death_plot <- as.Date(human_ct$Date_sign_or_symptom_erved_after_the_bite)
human_ct$month_death_plot <- 12*(year(human_ct$date_death_plot)-2023) + month(human_ct$date_death_plot) - 3
animal_ct$date_plot <- as.Date(animal_ct$Date_bitten)
animal_ct$month_plot <- 12*(year(animal_ct$date_plot)-2023) + month(animal_ct$date_plot) - 3
animal_inv$date_plot <- as.Date(animal_inv$Date_of_Investigation)
animal_inv$month_plot <- 12*(year(animal_inv$date_plot)-2023) + month(animal_inv$date_plot) - 3

months <- 1:max(human_ct$month_plot,human_ct$month_death_plot,animal_inv$month_plot,na.rm=T)
years <- unique(year(c(human_ct$date_death_plot[which(!is.na(human_ct$date_death_plot))],human_ct$date_plot,animal_inv$date_plot)))
month_labs <- paste0(rep(month.abb,length(min(years):max(years))),rep(min(years):max(years),each=12)-2000)[months+3]

# Subset
human_ct_healthy <- filter(human_ct, exposure%in%c("low-risk")) # 6
human_ct_exposures <- filter(human_ct, exposure%in%c("high-risk")) # includes people that went on to die
human_ct_exposures_survived <- filter(human_ct, exposure%in%c("high-risk") & (cause_of_death!="rabies"|is.na(cause_of_death)))
human_ct_deaths <- filter(human_ct,cause_of_death=="rabies")
animal_ct_healthy <- filter(animal_ct, Suspect%in%c("no"))
animal_ct_suspect <- filter(animal_ct, Suspect%in%c("yes","unknown")) # includes confirmed
animal_ct_suspect_not_confirmed <- filter(animal_ct, Suspect%in%c("yes","unknown") & (Lateral_flow_test!="positive"|is.na(Lateral_flow_test)))
animal_ct_confirmed <- filter(animal_ct, Lateral_flow_test=="positive")
animal_ct_healthy <- filter(animal_ct, Suspect%in%c("no"))
animal_inv_prob <- filter(animal_inv, Test_Results%in%c("probable")) 
animal_inv_conf <- filter(animal_inv, Test_Results%in%c("confirmed"))

# Prepare time series
humans_month_ct <- humans_month_exposure <- humans_month_death <- animals_month_inv <- animals_month_prob <- animals_month_conf <- rep(0,max(months))
humans_month_ct[sort(unique(human_ct$month_plot))] <- table(human_ct$month_plot) 
humans_month_exposure[sort(unique(human_ct_exposures_survived$month_plot))] <- table(human_ct_exposures_survived$month_plot)
humans_month_death[sort(unique(human_ct_deaths$month_death_plot))] <- table(human_ct_deaths$month_death_plot)
animals_month_inv[sort(unique(animal_inv$month_plot))] <- table(animal_inv$month_plot)
animals_month_prob[sort(unique(animal_inv_prob$month_plot))] <- table(animal_inv_prob$month_plot)
animals_month_conf[sort(unique(animal_inv_conf$month_plot))] <- table(animal_inv_conf$month_plot)



## Map of Nigeria
#_________________________

pdf("figs/Fig_1.pdf", width=6, height=4)

# Map of Nigeria
par(mar=c(1,0,0,0.5))
plot(st_geometry(states),border="grey60")
plot(st_geometry(states[which(states$ADM1_EN=="Kaduna"),]),border=NA,col="blue3",lwd=2,add=T)
plot(st_geometry(LGAs),border="grey85",add=T)
plot(st_geometry(LGAs[which(LGAs$ADM2_EN=="Sabon-Gari"),]),col="orange",border="orange",add=T)
plot(st_geometry(states),border="grey60",add=T)
plot(st_geometry(Nigeria),add=T,lwd=2)
addscalebar(plotunit="latlon",widthhint = 0.2)
legend("bottomright", c("Kaduna State", "Sabon Gari LGA", "LGA boundary", "State boundary", "Country boundary"),
       lty=c(NA,NA,1,1,1), lwd=c(NA,NA,1,1,2), pch=c(15,15,NA,NA,NA),
       col=c("blue3","orange","grey85","grey60",1), bty="n",pt.cex = c(2,2,NA,NA,NA))
# text(states[which(states$ADM1_EN=="Kaduna"),],"Kaduna",cex=0.7)
dev.off()



## Map of LGAs & health facilities
#_________________________
pt_cex_denominator <- 2

pdf("figs/Fig_2.pdf",width=7.5, height=6)

# Health facilities & investigations
par(fig=c(0,1,0,1))
par(mar=c(1,1,1,1))
cols <- rev(viridis(nrow(health_facilities)))
max_x <- max(animal_ct$`_GPS_Location_in_UTMS_longitude`, human_ct$`_Record_your_current_location_longitude`)+0.06
min_x <- min(animal_ct$`_GPS_Location_in_UTMS_longitude`, human_ct$`_Record_your_current_location_longitude`)-0.18
max_y <- max(animal_ct$`_GPS_Location_in_UTMS_latitude`, human_ct$`_Record_your_current_location_latitude`)+0.05
min_y <- min(animal_ct$`_GPS_Location_in_UTMS_latitude`, human_ct$`_Record_your_current_location_latitude`)-0.063
plot(st_geometry(LGAs[which(LGAs$ADM1_EN=="Kaduna"),]),border="grey",xlim=c(min_x,max_x),ylim=c(min_y,max_y))
points(health_facilities$Longitude,health_facilities$Latitude,pch=19,col=cols,cex=health_facilities$investigations/pt_cex_denominator)
text(st_coordinates(st_centroid(LGAs)),LGAs$ADM2_EN,cex=0.9)
rect(max_x-0.47, min_y-0.04, max_x+0.28, min_y+0.365, density = NULL, border = NA,col="white",xpd=T)
legend(max_x-0.46, min_y+0.25,c(health_facilities$Name.of.Facility),title="Health facility:", title.cex=1, title.adj=0,col=cols,pch=19,bty="n")
legend(max_x-0.46, min_y+0.36,legend=c("1     ","5     ","10     "),title="Number of bite investigations:",
       col=1,pch=19,pt.cex=c(1,5,10)/pt_cex_denominator,ncol=3,cex=0.9,x.intersp = 1.8,y.intersp = 1.4,title.cex = 1, title.adj=0,bty="n")
# legend("topleft","B",bty="n",text.font=2, cex=1.4)
addscalebar(plotunit="latlon")

dev.off()


## COMBINED MAPS
#_________________________
pdf("figs/Fig_maps_study.pdf", width=10.5, height=4.2)
par(mfrow = c(1,2))
par(mar=c(1,0,0,0.5))

# Nigeria
plot(st_geometry(states),border="grey60")
plot(st_geometry(states[which(states$ADM1_EN=="Kaduna"),]),border=NA,col="blue3",lwd=2,add=T)
plot(st_geometry(LGAs),border="grey85",add=T)
plot(st_geometry(LGAs[which(LGAs$ADM2_EN=="Sabon-Gari"),]),col="orange",border="orange",add=T)
plot(st_geometry(states),border="grey60",add=T)
plot(st_geometry(Nigeria),add=T,lwd=2)
addscalebar(plotunit="latlon",widthhint = 0.2)
legend("bottomright", c("Kaduna State", "Sabon Gari LGA", "LGA boundary", "State boundary", "Country boundary"),
       lty=c(NA,NA,1,1,1), lwd=c(NA,NA,1,1,2), pch=c(15,15,NA,NA,NA),
       col=c("blue3","orange","grey85","grey60",1), bty="n",pt.cex = c(2,2,NA,NA,NA))
mtext("A", side = 3, line = -2, adj = 0, font = 2)

# Zaria
par(mar=c(1,1,1,1))
pt_cex_denominator <- 2
cols <- rev(viridis(nrow(health_facilities)))
max_x <- max(animal_ct$`_GPS_Location_in_UTMS_longitude`, human_ct$`_Record_your_current_location_longitude`)+0.06
min_x <- min(animal_ct$`_GPS_Location_in_UTMS_longitude`, human_ct$`_Record_your_current_location_longitude`)-0.18
max_y <- max(animal_ct$`_GPS_Location_in_UTMS_latitude`, human_ct$`_Record_your_current_location_latitude`)+0.05
min_y <- min(animal_ct$`_GPS_Location_in_UTMS_latitude`, human_ct$`_Record_your_current_location_latitude`)-0.063
plot(st_geometry(LGAs[which(LGAs$ADM1_EN=="Kaduna"),]),border="grey",xlim=c(min_x,max_x),ylim=c(min_y,max_y))
points(health_facilities$Longitude,health_facilities$Latitude,pch=19,col=cols,cex=health_facilities$investigations/pt_cex_denominator)
text(st_coordinates(st_centroid(LGAs)),LGAs$ADM2_EN,cex=0.7)
rect(max_x-0.47, min_y-0.04, max_x+0.28, min_y+0.365, density = NULL, border = NA,col="white",xpd=T)
legend(max_x-0.6, min_y+0.25,c(health_facilities$Name.of.Facility),title="Health facility:", title.cex=0.9, title.adj=0,col=cols,pch=19,cex=0.7,bty="n")
legend(max_x-0.6, min_y+0.38,legend=c("1     ","5     ","10     "),title="Facility-linked investigations:",
       col=1,pch=19,pt.cex=c(1,5,10)/pt_cex_denominator,ncol=3,cex=0.7,x.intersp = 1.8,y.intersp = 1.4,title.cex = 0.9, title.adj=0,bty="n")
# legend("topleft","B",bty="n",text.font=2, cex=1.4)
addscalebar(plotunit="latlon")
mtext("B", side = 3, line = -1, adj = 0, font = 2)
dev.off()


## Fig.3
#_________________________

set.seed(0)
jitter_amount <-0.01

pdf("figs/Fig_3.pdf",width=14, height=5.2)

# Cases, exposures & deaths
par(fig=c(0,0.5,0,1))
par(mar=c(1,0.2,1,3))
cols <- c("lightblue1","dodgerblue","navy","gold","darkorange","red3")
plot(st_geometry(LGAs[which(LGAs$ADM1_EN=="Kaduna"),]),border="grey",xlim=c(min_x,max_x),ylim=c(min_y,max_y))
text(st_coordinates(st_centroid(LGAs)),LGAs$ADM2_EN,cex=0.9)
points(jitter(animal_ct_healthy$`_GPS_Location_in_UTMS_longitude`,amount=jitter_amount),jitter(animal_ct_healthy$`_GPS_Location_in_UTMS_latitude`,amount=jitter_amount),pch=19,col=cols[1],cex=1)
points(jitter(human_ct_healthy$`_Record_your_current_location_longitude`,amount=jitter_amount),jitter(human_ct_healthy$`_Record_your_current_location_latitude`,amount=jitter_amount),pch=17,col=cols[4],cex=1)
points(jitter(animal_ct_suspect_not_confirmed$`_GPS_Location_in_UTMS_longitude`,amount=jitter_amount),jitter(animal_ct_suspect_not_confirmed$`_GPS_Location_in_UTMS_latitude`,amount=jitter_amount),pch=19,col=cols[2],cex=1)
points(jitter(animal_ct_confirmed$`_GPS_Location_in_UTMS_longitude`,amount=jitter_amount),jitter(animal_ct_confirmed$`_GPS_Location_in_UTMS_latitude`,amount=jitter_amount),pch=19,col=cols[3],cex=1)
points(jitter(human_ct_exposures_survived$`_Record_your_current_location_longitude`,amount=jitter_amount),jitter(human_ct_exposures_survived$`_Record_your_current_location_latitude`,amount=jitter_amount),pch=17,col=cols[5],cex=1)
points(jitter(human_ct_deaths$`_Record_your_current_location_longitude`,amount=jitter_amount),jitter(human_ct_deaths$`_Record_your_current_location_latitude`,amount=jitter_amount),pch=17,col=cols[6],cex=1.5)
legend("topleft","A",bty="n",text.font=2, cex=1.4)
legend("bottomright", c("Healthy dog","Probable rabid dog","RDT confirmed rabid dog","Low risk human bite", "Probable human exposure", "Human rabies death"),
       pch=c(19,19,19,17,17,17),pt.cex=c(1,1,1,1,1,1.5),
       col=cols, box.col = NA)
addscalebar(plotunit="latlon")

# Time series
cols <- c("gold","orange","red")
par(fig=c(0.49,1,0.5,1),new=T)
par(mar=c(1.5,2,2,0.5))
barplot(humans_month_ct,col=cols[1],names=NA,border=NA,cex.names=0.8,axes=F,las=2) # adding on deaths so that height of bars is correct to show both bites and deaths
mtext("Count",side=2,line=2.2,cex=1.2)
box(bty="l")
barplot(humans_month_exposure+humans_month_death,col=cols[2],add=T,border=NA)
barplot(humans_month_death,col=cols[3],add=T,border=NA)
legend("topright",c("Low risk human bites","Probable human exposures","Human rabies deaths"),col=cols,pch=15,pt.cex=2,bty="n")
legend("topleft","B",bty="n",text.font=2, cex=1.4)
cols <- c("lightblue1","dodgerblue","navy")

par(mar=c(2.5,2,1,0.5))
par(fig=c(0.49,1,0.05,0.55),new=T)
barplot(animals_month_inv,col=cols[1],names=month_labs,border=NA,cex.names=0.8,axes=F,las=2)
axis(2)
box(bty="l")
mtext("Count",side=2,line=2.2,cex=1.2)
barplot(animals_month_prob + animals_month_conf,col=cols[2],add=T,border=NA)
barplot(animals_month_conf,col=cols[3],add=T,border=NA)
legend("topright",c("Healthy dogs","Probable rabid dogs","RDT confirmed rabid dogs"),col=cols,pch=15,pt.cex=2,bty="n")
legend("topleft","C",bty="n",text.font=2, cex=1.4)

dev.off()



pdf("figs/Fig_3_rearrange.pdf",width=6.7, height=9)

set.seed(0)

# Cases, exposures & deaths
par(fig=c(0,1,0.49,1))
par(mar=c(1.5,3,0.5,3))
cols <- c("lightblue1","dodgerblue","navy","gold","darkorange","red3")
plot(st_geometry(LGAs[which(LGAs$ADM1_EN=="Kaduna"),]),border="grey",xlim=c(min_x,max_x),ylim=c(min_y,max_y))
text(st_coordinates(st_centroid(LGAs)),LGAs$ADM2_EN,cex=0.9)
points(jitter(animal_ct_healthy$`_GPS_Location_in_UTMS_longitude`,amount=jitter_amount),jitter(animal_ct_healthy$`_GPS_Location_in_UTMS_latitude`,amount=jitter_amount),pch=19,col=cols[1],cex=1)
points(jitter(human_ct_healthy$`_Record_your_current_location_longitude`,amount=jitter_amount),jitter(human_ct_healthy$`_Record_your_current_location_latitude`,amount=jitter_amount),pch=17,col=cols[4],cex=1)
points(jitter(animal_ct_suspect_not_confirmed$`_GPS_Location_in_UTMS_longitude`,amount=jitter_amount),jitter(animal_ct_suspect_not_confirmed$`_GPS_Location_in_UTMS_latitude`,amount=jitter_amount),pch=19,col=cols[2],cex=1)
points(jitter(animal_ct_confirmed$`_GPS_Location_in_UTMS_longitude`,amount=jitter_amount),jitter(animal_ct_confirmed$`_GPS_Location_in_UTMS_latitude`,amount=jitter_amount),pch=19,col=cols[3],cex=1)
points(jitter(human_ct_exposures_survived$`_Record_your_current_location_longitude`,amount=jitter_amount),jitter(human_ct_exposures_survived$`_Record_your_current_location_latitude`,amount=jitter_amount),pch=17,col=cols[5],cex=1)
points(jitter(human_ct_deaths$`_Record_your_current_location_longitude`,amount=jitter_amount),jitter(human_ct_deaths$`_Record_your_current_location_latitude`,amount=jitter_amount),pch=17,col=cols[6],cex=1.5)
legend("topleft","A",bty="n",text.font=2, cex=1.4)
legend("bottomright", c("Healthy dog","Probable rabid dog","RDT confirmed rabid dog","Low risk human bite", "Probable human exposure", "Human rabies death"),
       pch=c(19,19,19,17,17,17),pt.cex=c(1,1,1,1,1,1.5),
       col=cols, box.col = NA)
addscalebar(plotunit="latlon")

# Time series
cols <- c("gold","orange","red")
par(fig=c(0,1,0.265,0.53),new=T)
par(mar=c(1.3,2.5,1.5,0.1))
barplot(humans_month_ct,col=cols[1],names=NA,border=NA,cex.names=0.8,axes=F,las=2) # adding on deaths so that height of bars is correct to show both bites and deaths
mtext("Count",side=2,line=2.2,cex=1.2)
box(bty="l")
barplot(humans_month_exposure+humans_month_death,col=cols[2],add=T,border=NA)
barplot(humans_month_death,col=cols[3],add=T,border=NA)
legend("topright",c("Low risk human bites","Probable human exposures","Human rabies deaths"),col=cols,pch=15,pt.cex=2,bty="n")
legend("topleft","B",bty="n",text.font=2, cex=1.4)

cols <- c("lightblue1","dodgerblue","navy")
par(mar=c(3.2,2.5,0,0.1))
par(fig=c(0,1,0,0.265),new=T)

barplot(animals_month_inv,col=cols[1],names=month_labs,border=NA,cex.names=0.8,axes=F,las=2)
axis(2)
box(bty="l")
mtext("Count",side=2,line=2.2,cex=1.2)
barplot(animals_month_prob + animals_month_conf,col=cols[2],add=T,border=NA)
barplot(animals_month_conf,col=cols[3],add=T,border=NA)

legend("topright",c("Healthy dogs","Probable rabid dogs","RDT confirmed rabid dogs"),col=cols,pch=15,pt.cex=2,bty="n")
legend("topleft","C",bty="n",text.font=2, cex=1.4)

dev.off()




