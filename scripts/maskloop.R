library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(dplyr)
library(sf)


usa=readOGR('C:/Users/linds/Dropbox/SDModeling/Environmental_Shape_files/usa','US_STATES_wgs84')
globe=readOGR('C:/Users/linds/Dropbox/SDModeling/Environmental_Shape_files/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
NAm=crop(NAm,extent(-165,-60,8,85))
usaWGS=spTransform(usa,CRS(proj4string(NAm)))
key= read.csv("C:/Users/linds/Dropbox/SDModeling/Bees/Megachilidae_diversity_paper_modeling/US_140EVT_04252017/LF_140EVT_09152016.csv")

Agriculture = readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Agriculture', 'Agriculture')
Barren= readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Barren', 'Barren')
Developed= readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Developed', 'Developed')
Herb = readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Herb', 'Herb')
Shrub= readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Shrub', 'shrub')
Sparse= readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Sparse', 'Sparse')
Tree = readOGR('C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/landcover layers/Landcover_vars/Landcover_vars/Tree', 'tree')




setwd("C:/Users/linds/Dropbox/TCN_iDigBees/Background Bee data_Paige_Lindsie/Bee Paper/Final Species Lists")
spList= read.csv("contiguousSpecies.csv") #column 1 correctedName 
setwd("C:/Users/linds/Dropbox/SDModeling/Bees/Megachilidae_diversity_paper_modeling/US_140EVT_04252017/Grid/us_140evt")
land=raster("dblbnd.adf")
i = 2 

for (i in c(1:1190)){
  ##extract values from species lat/long
  sp = raster(paste(paste("C:/Users/linds/Dropbox/Symbiota_database/Gap_analysis copy/Ranges/Observed_Richness/Halictidae/110km",paste(spList[i,2]),sep="/"),"raster.grd",sep="_"))
  sp = rasterToPoints(sp)
  sp= as.data.frame(sp)
  sp = filter(sp, layer >0)
  coordinates(sp) <- c(1,2) #set coordinates
  projection(sp) <- "+proj=longlat +ellps=WGS84"
  sp=spTransform(sp, CRS(proj4string(land)))
  values <- extract(land,sp)
  values <- cbind.data.frame(coordinates(sp),values)

  val = merge(values, key, by.x=c("values"),by.y=c("VALUE"),all.x=TRUE)
  ppsub = val %>% select (EVT_LF) %>% count(EVT_LF)%>%
    mutate(prop= 100*(n / sum(n)))


  # determine cutoff for landfire inclusion
  ppsub2 = ppsub %>% filter(EVT_LF != "Nodata")  %>% filter(prop > 10)

  # TODO: stack() grids of landfire_keepers
    #i=1
    #maskrast = paste(ppsub2[i,1]) + paste(ppsub2[i,1]) 
  Mask = bind(Herb, Shrub, Tree) # NEED TO FIGURE OUT HOW TO CALL THESE IN LOOP!


  # TODO: mask() taxon range with the landfire_keepers
  spRange = raster(paste(paste("C:/Users/linds/Dropbox/Symbiota_database/Gap_analysis copy/Ranges/Halictidae/110km",paste(spList[i,2]),sep="/"),"raster.grd",sep="_"))
  spRangeN = crop(spRange, Mask)

  writeRaster = (spRangeN, file = paste(paste("C:/Users/linds/Dropbox/Symbiota_database/Gap_analysis copy/Ranges/Mask/Halictidae/110km",paste(spList[i,2]),sep="/"),"raster.grd",sep="_"))
}