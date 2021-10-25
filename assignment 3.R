##Asssignment 3

##Set Up

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
library(mapview)

# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
st_c    <- st_coordinates
st_coid <- st_centroid

## Data Wrangling

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
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")%>%
  st_transform('ESRI:102271') %>% 
  distinct()

chicagoBoundary <- 
  st_read(file.path(root.dir,"/Chapter5/chicagoBoundary.geojson")) %>%
  st_transform('ESRI:102271') 

drug <- st_intersection(drug, chicagoBoundary)

## Data Visualization

grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = chicagoBoundary) +
               geom_sf(data = drug, colour="red", size=0.1, show.legend = "point") +
               labs(title= "Narcotics, Chicago - 2017") +
               mapTheme(title_size = 14),
             
             ggplot() + 
               geom_sf(data = chicagoBoundary, fill = "grey40") +
               stat_density2d(data = data.frame(st_coordinates(drug)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 40, geom = 'polygon') +
               scale_fill_viridis() +
               scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
               labs(title = "Density of Narcotics") +
               mapTheme(title_size = 14) + theme(legend.position = "none"))

##Fishnet Grid

## using {sf} to create the grid
## Note the `.[chicagoBoundary] %>% ` line. This is needed to clip the grid to our data
fishnet <- 
  st_make_grid(chicagoBoundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[chicagoBoundary] %>%            # <- MDH Added
  st_sf() %>%
  mutate(uniqueID = rownames(.))

##Add Points to Fishnet

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
  labs(title = "Count of Narcotics for the fishnet") +
  mapTheme()

mapview(crime_net, zcol = "countDrug")

## Modeling Spatial Features

abandonCars <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Abandoned-Vehicles/3c9v-pnva") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Cars")

abandonBuildings <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Vacant-and-Abandoned-Building/7nii-7srd") %>%
  mutate(year = substr(date_service_request_was_received,1,4)) %>%  filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Abandoned_Buildings")

graffiti <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Graffiti-Removal-Historical/hec5-y4x5") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  filter(where_is_the_graffiti_located_ %in% c("Front", "Rear", "Side")) %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Graffiti")

streetLightsOut <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Street-Lights-All-Out/zuxi-7xem") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Street_Lights_Out")

sanitation <-
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Sanitation-Code-Complaints-Hi/me59-5fac") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Sanitation")

liquorRetail <- 
  read.socrata("https://data.cityofchicago.org/resource/nrmj-3kcf.json") %>%  
  filter(business_activity == "Retail Sales of Packaged Liquor") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Liquor_Retail")

potholes <- 
  read.socrata("https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Pot-Holes-Reported-Historical/7as2-ds3y") %>%
  mutate(year = substr(creation_date,1,4)) %>% filter(year == "2017") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Potholes")

neighborhoods <- 
  st_read("https://raw.githubusercontent.com/blackmad/neighborhoods/master/chicago.geojson") %>%
  st_transform(st_crs(fishnet)) 


## Aggregate features to our fishnet

vars_net <-
  rbind(abandonCars,streetLightsOut,abandonBuildings,
        liquorRetail, graffiti, sanitation, potholes)%>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet, by = "uniqueID") %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

##Visualization

vars_net.long <- 
  gather(vars_net, Variable, value, -geometry, -uniqueID)

vars <- unique(vars_net.long$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList, ncol=3, top="Risk Factors by Fishnet"))

## NN feature

vars_net <-
  vars_net %>%
  mutate(
    Abandoned_Buildings.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(abandonBuildings),3),
    Abandoned_Cars.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(abandonCars),3),
    Graffiti.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(graffiti),3),
    Liquor_Retail.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(liquorRetail),3),
    Street_Lights_Out.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(streetLightsOut),3),
    Sanitation.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(sanitation),3),
    Potholes.nn = 
      nn_function(st_c(st_coid(vars_net)), st_c(sanitation),3))

##Visualization of NN Features

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

do.call(grid.arrange,c(mapList, ncol = 3, top = "Nearest Neighbor risk Factors by Fishnet"))

## Distance to a place
Garfield_Park_Point <-
  filter(neighborhoods, name == "Garfield Park") %>%
  st_centroid()

vars_net$Garfield_Park_Distance =
  st_distance(st_centroid(vars_net),Garfield_Park_Point) %>%
  as.numeric() 

## Join NN Features

final_net <-
  left_join(crime_net, st_drop_geometry(vars_net), by="uniqueID") 


## Join in Areal Data

final_net <-
  st_centroid(final_net) %>%
  st_join(dplyr::select(neighborhoods, name), by = "uniqueID") %>%
  st_join(dplyr::select(policeDistricts, District), by = "uniqueID") %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(final_net, geometry, uniqueID)) %>%
  st_sf() %>%
  na.omit()

mapview(final_net, zcol = "District")

## Local Moran's I

final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)

final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

local_morans <- localmoran(final_net$countDrug, final_net.weights, zero.policy=TRUE) %>% 
  as.data.frame()

final_net.localMorans <- 
  cbind(local_morans, as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Drug_Count = countDrug, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.001, 1, 0)) %>%
  gather(Variable, Value, -geometry)

## Visualization

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(final_net.localMorans, Variable == i), 
            aes(fill = Value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme(title_size = 14) + theme(legend.position="bottom")}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Narcotics"))

## NN Feature to Hot Spot

final_net <- final_net %>% 
  mutate(drug.isSig = 
           ifelse(local_morans[,5] <= 0.001, 1, 0)) %>%
  mutate(drug.isSig.dist = 
           nn_function(st_c(st_coid(final_net)),
                       st_c(st_coid(filter(final_net, 
                                           drug.isSig == 1))), 
                       k = 1))

## Visualization

ggplot() +
  geom_sf(data = final_net, aes(fill=drug.isSig.dist), colour=NA) +
  scale_fill_viridis(name="NN Distance") +
  labs(title="Narcostics NN Distance") +
  mapTheme()

## Modeling and VC


