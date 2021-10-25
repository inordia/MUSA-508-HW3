library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

policeDistricts <- 
  st_read("https://data.cityofchicago.org/api/geospatial/fthy-xz3r?method=export&format=GeoJSON") %>%
  st_transform('ESRI:102271') %>%
  dplyr::select(District = dist_num)

policeBeats <- 
  st_read("https://data.cityofchicago.org/api/geospatial/aerh-rz74?method=export&format=GeoJSON") %>%
  st_transform('ESRI:102271') %>%
  dplyr::select(District = beat_num)

bothPoliceUnits <- rbind(mutate(policeDistricts, Legend = "Police Districts"), 
                         mutate(policeBeats, Legend = "Police Beats"))

drug <- 
  read.socrata("https://data.cityofchicago.org/Public-Safety/Crimes-2017/d62x-nvdr") %>% 
  filter(Primary.Type == "NARCOTICS") %>%
  mutate(x = gsub("[()]", "", Location)) %>%
  separate(x,into= c("Y","X"), sep=",") %>%
  mutate(X = as.numeric(X),Y = as.numeric(Y)) %>% 
  #na.omit() %>%
  filter(is.na(X)==FALSE) %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102271') %>% 
  distinct()

chicagoBoundary <- 
  st_read(file.path(root.dir,"/Chapter5/chicagoBoundary.geojson")) %>%
  st_transform('ESRI:102271')

drug<-st_intersection(drug,chicagoBoundary)

grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = chicagoBoundary) +
               geom_sf(data = drug, colour="red", size=0.1, show.legend = "point") +
               labs(title= "drug, Chicago - 2017") +
               mapTheme(title_size = 14),
             
             ggplot() + 
               geom_sf(data = chicagoBoundary, fill = "grey40") +
               stat_density2d(data = data.frame(st_coordinates(drug)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 40, geom = 'polygon') +
               scale_fill_viridis() +
               scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
               labs(title = "Density of drug") +
               mapTheme(title_size = 14) + theme(legend.position = "none"))

#A map of your outcome joined to the fishnet
fishnet <- 
  st_make_grid(chicagoBoundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[chicagoBoundary] %>%            # <- MDH Added
  st_sf() %>%
  mutate(uniqueID = rownames(.))

## add a value of 1 to each crime, sum them with aggregate
crime_net <- 
  dplyr::select(drug) %>% 
  mutate(countDrug = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countDrug = replace_na(countDrug, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = crime_net, aes(fill = countDrug), color = NA) +
  scale_fill_viridis() +
  labs(title = "Count of drug for the fishnet") +
  mapTheme()

#multiple map of your risk factors in the fishnet (counts, distance, and/or other feature engineering approaches).
abandonCars <-
  read.socrata(paste0("https://data.cityofchicago.org/Service",
                      "-Requests/311-Service-Requests-Abandoned-Vehicles/",
                      "3c9v-pnva")) %>%
  mutate(year = substr(creation_date,1,4)) %>%
  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Cars")
abandonBuildings <-
  read.socrata(paste0("https://data.cityofchicago.org/Service",
                      "-Requests/311-Service-Requests-Vacant-and-Abandoned-Building/",
                      "7nii-7srd")) %>%
  mutate(year=substr(date_service_request_was_received,1,4))%>%
  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Buildings")
graffiti <-
  read.socrata(paste0("https://data.cityofchicago.org/Service",
                      "-Requests/311-Service-Requests-Graffiti-Removal-Historical/",
                      "hec5-y4x5")) %>%
  mutate(year = substr(creation_date,1,4)) %>%
  filter(year == "2017") %>%
  filter(where_is_the_graffiti_located_ %in%
           c("Front","Rear","Side")) %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Graffiti")
streetLightsOut <-
  read.socrata(paste0("https://data.cityofchicago.org/Service",
                      "-Requests/311-Service-Requests-Street-Lights-All-Out/",
                      "zuxi-7xem")) %>%
  mutate(year = substr(creation_date,1,4)) %>%
  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Street_Lights_Out")

sanitation <-
  read.socrata(paste0("https://data.cityofchicago.org/Service",
                      "-Requests/311-Service-Requests-Sanitation-Code-Complaints-Hi/",
                      "me59-5fac")) %>%
  mutate(year = substr(creation_date,1,4)) %>%
  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Sanitation")
liquorRetail <-
  read.socrata(paste0("https://data.cityofchicago.org/resource/",
                      "nrmj-3kcf.json")) %>%
  filter(business_activity ==
           "Retail Sales of Packaged Liquor") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Liquor_Retail")
neighborhoods <-
  st_read(paste0("https://raw.githubusercontent.com/",
                 "blackmad/neighborhoods/master/chicago.geojson")) %>%
  st_transform(st_crs(fishnet))

#pot holes
pot_holes<-read.socrata(paste0("https://data.cityofchicago.org/resource/_311-potholes.json")) %>%
  mutate(year = substr(creation_date,1,4)) %>%
  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "pot_holes")

#public schools
schools<-read.socrata(paste0("https://data.cityofchicago.org/resource/mntu-576c.json")) %>%
  dplyr::select(y, x) %>%
  na.omit() %>%
  st_as_sf(coords = c("x","y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "schools")

vars_net<-rbind(abandonCars,streetLightsOut,abandonBuildings,liquorRetail,graffiti,sanitation,schools,pot_holes)%>%
  st_join(.,fishnet,join=st_within)%>%
  st_drop_geometry()%>%
  group_by(uniqueID,Legend)%>%
  summarize(count=n())%>%
  full_join(fishnet)%>%
  spread(Legend,count,fill=0)%>%
  st_sf()%>%
  dplyr::select(-`<NA>`)%>%
  na.omit()%>%
  ungroup()

vars_net.long<-gather(vars_net,Variable,value,-geometry,-uniqueID)
vars<-unique(vars_net.long$Variable)
mapList<-list()
for(i in vars)
  {mapList[[i]]<-
    ggplot()+
    geom_sf(data=filter(vars_net.long,Variable==i),
            aes(fill=value),colour=NA)+
    scale_fill_viridis(name="")+
    labs(title=i)+mapTheme()}

do.call(grid.arrange,c(mapList,ncol=3,top="Risk Factors by Fishnet"))

#3. A small multiple map of your risk factors in the fishnet(counts,distance,and/or other feature engineering approaches).
## Nearest Neighbor Feature
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

st_c    <- st_coordinates
st_coid <- st_centroid

vars_net <-
  vars_net %>%
  mutate(
    Abandoned_Buildings.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(abandonBuildings),1),
    abandonCars.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(abandonCars),1),
    graffiti.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(graffiti),1),
    liquorRetail.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(liquorRetail),1),
    streetLightsOut.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(streetLightsOut),1),
    sanitation.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(sanitation),1),
    pot_holes.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(pot_holes),1),
    schools.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(schools),1))

## Visualize the NN feature
vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

vars <- unique(vars_net.long.nn$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList,ncol=3,top="Nearest Neighbor risk Factors by Fishnet"))

#measure distance to one point
loopPoint<-filter(neighborhoods,name=="Loop")%>%
  st_centroid()
vars_net$loopDistance=st_distance(st_centroid(vars_net),loopPoint)%>%
  as.numeric()

ggplot() +
  geom_sf(data=vars_net, aes(fill=loopDistance)) +
  scale_fill_viridis(name="") +
  labs(title="Euclidean Distance to The Loop") +
  mapTheme() 

#create final_set
final_net<-left_join(crime_net,st_drop_geometry(vars_net),by="uniqueID")
final_net<-st_centroid(final_net)%>%
  st_join(dplyr::select(neighborhoods,name))%>%
  st_join(dplyr::select(policeDistricts,District))%>%
  st_drop_geometry()%>%
  left_join(dplyr::select(final_net,geometry,uniqueID))%>%
  st_sf()%>%
  na.omit()

#Local Moran¡¯s I-related small multiple map of your outcome (see4.1)
final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

final_net.localMorans<-
  cbind(as.data.frame(localmoran(final_net$countDrug,
                                 final_net.weights)),
        as.data.frame(final_net))%>%
  st_sf()%>%
  dplyr::select(drug_Count=countDrug,
                Local_Morans_I=Ii,
                P_Value=`Pr(z>0)`)%>%
  mutate(Significant_Hotspots=ifelse(P_Value<=
                                       0.05,1,0))%>%
  gather(Variable,Value,-geometry)

vars<-unique(final_net.localMorans$Variable)
varList<-list()
for(i in vars){
  varList[[i]]<-
    ggplot()+
    geom_sf(data=filter(final_net.localMorans,Variable==i),
            aes(fill=Value),colour=NA)+
    scale_fill_viridis(name="")+
    labs(title=i)+
    mapTheme()+
    theme(legend.position="bottom")}
do.call(grid.arrange,c(varList,ncol=4,top="Local Morans I statistics, Drug"))

final_net<-final_net%>%
  mutate(drug.isSig=
           ifelse(localmoran(final_net$countDrug,
                             final_net.weights)[,5]
                  <=0.0000001,1,0))%>%
  mutate(drug.isSig.dist=
           nn_function(st_coordinates(st_centroid(final_net)),
                       st_coordinates(st_centroid(filter(final_net,drug.isSig==
                                                           1))),1))

ggplot()+
  geom_sf(data=final_net, aes(fill = drug.isSig.dist), colour=NA) +
  scale_fill_viridis(name="")+
  labs(title="Distance to highly significant drug hotsopt")+
  mapTheme()

#correlation test

