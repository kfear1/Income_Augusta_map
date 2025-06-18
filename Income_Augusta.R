#set working directory
setwd("~/Documents/R/Github/Income_violence_map/")

#imoprt libraries


#Calculate average weighted income for 3 buffer rings in Augusta 
library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(ggmap)
library(ggplot2)

income<- read_sf("Income.shp")
Aug<- read_sf("Augusta_ll.shp")

#Projection conversion- check each projection and make sure they are the same- make them both NAD83

st_crs(income)
st_crs(Aug)
Aug_Nad83<- Aug %>% st_transform(crs=26919)
st_crs(Aug_Nad83)

#Create 3 buffer feature classes from Augusta_ll with     3 distance settings: 100,000; 200,000; 300,000. (Unit = Meter) 

d100<- st_buffer(Aug_Nad83, dist= 100000)
plot(d100)
d200<- st_buffer(Aug_Nad83, dist=200000)
d300<- st_buffer(Aug_Nad83, dist=300000)
plot(d200)
plot(d300)

#remove Name.1 column
d1<- st_difference(d300, d200)
plot(d1)
d1<- subset(d1,select=-Name.1)
plot(d1)
d2<- st_difference(d200, d100)
d2<-subset(d2, select=-Name.1)

#combine into one shapefile
dAll<- rbind(d1,d2,d100)
plot(dAll)
dAll$ID<-1:3
plot(dAll)

#Create sf object (dAllIncome) by intersecting dAll with income.shp (dAllIncome) 

dAllIncome<- st_intersection(dAll, income)
plot(dAllIncome)

#Create a field “area” in dAllIncome to store area value for each polygon.

dAllIncome$Area <- st_area(dAllIncome)/1000000
head(dAllIncome)

#Create a sf object (AllBand) which dissolves polygon boundaries inside each buffer. Also create a field “AB_Area” to store area value for each buffer area (0-100k, 100k-200k, 200k-300k) 

AllBand<- dAllIncome%>% group_by(ID)%>% summarise()%>% ungroup()%>% st_as_sf()

plot(AllBand)


AllBand$AB_Area<- st_area(AllBand)/1000000
AllBand_df<- AllBand%>% st_drop_geometry()

#Join AllBand to dAllIncome to calculate weighted average income for each buffer ring. 
DAllIncome_Band<- left_join(dAllIncome,AllBand_df, by=c("ID"="ID"))
head(DAllIncome_Band)

DAllIncome_Band$ratio<-DAllIncome_Band$Area/DAllIncome_Band$AB_Area

DAllIncome_Band$wtInc<-DAllIncome_Band$Income*DAllIncome_Band$ratio 

AllBandInc<- DAllIncome_Band%>%group_by(ID)%>%summarise(wtInc=sum(wtInc,na.rm=TRUE))%>% ungroup()%>% st_as_sf()


AllBandInc$wtInc <- as.numeric(AllBandInc$wtInc)

#initiate png creation

png("Augusta_income.png",width = 800, height = 500) 

#Plot the map
ggplot(AllBandInc) +
  geom_sf(aes(fill = wtInc)) + theme_bw()+labs(fill="Weighted average income",title="Weighted average income in Augusta", x="Longitude", y="Latitude")+scale_fill_continuous(low="khaki", high="firebrick", labels=comma)

#finish making image
dev.off()

