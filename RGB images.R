

library(neonUtilities)
library(ForestTools)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(stringr) # this is for data management
library(tidyr)
# Set global option to NOT convert all character variables to factors

vst <-
  loadByProduct(
    site = "BART",
    dpID = "DP1.10098.001",
    package = "basic",
    check.size = FALSE
  )



# read in shapefile of plot locations
plots<-readOGR("~/GitHub/MELNHE_NEON_AOP/data_folder","Bartlett_intensive_sites_30x30")
plot(plots)
# transform to UTM coordinates
crss <- make_EPSG()

UTM <- crss %>% dplyr::filter(grepl("WGS 84", note))%>% 
  dplyr::filter(grepl("19N", note))

stands <- sp::spTransform(plots, CRS(paste0("+init=epsg:",UTM$code)))

# centroids are the 'plot centers'. code for Lidar tiles works with point data
centroids <- as.data.frame(getSpPPolygonsLabptSlots(stands))


stdf<-as.data.frame(stands)
stdf$staplo<-paste(stdf$stand, stdf$plot)
stdf
stands$Treatment<-sapply(stdf[ ,7],switch,
                         "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                         "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                         "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                         "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                         "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                         "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                         "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                         "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                         "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N",
                         "HBM 1"="NP", "HBM 2"="N",  "HBM 3"="Control","HBM 4"="P",
                         "HBO 1"="P",  "HBO 2"="N",  "HBO 3"="NP",  "HBO 4"="Control", "HBO 7"="Control",
                         "JBM 1"="NP", "JBM 2"="N",  "JBM 3"="Control","JBM 4"="P",
                         "JBO 1"="NP", "JBO 2"="P",  "JBO 3"="N",   "JBO 4"="Control")


## format the neon tree data


# start by subsetting data to plots with trees
vst.trees <- vst$vst_perplotperyear[which(
  vst$vst_perplotperyear$treesPresent=="Y"),]

# make variable for plot sizes
plot.size <- numeric(nrow(vst.trees))

# populate plot sizes in new variable
plot.size[which(vst.trees$plotType=="tower")] <- 40
plot.size[which(vst.trees$plotType=="distributed")] <- 20

# create map of plots
symbols(vst.trees$easting,
        vst.trees$northing,
        squares=plot.size, inches=F,
        xlab="Easting", ylab="Northing")


plot(stands, add=T, col="blue", size="yellow")






