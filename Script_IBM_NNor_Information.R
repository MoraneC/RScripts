# -----------------------------------------------------------#
# -----------         BITER Project           ---------------#
# ---------     Extraction of dispersal IA N nor      -------#
# -----------------------------------------------------------#
# 
# 
# Written by: Morane Clavel-Henry morane@icm.csic.es
# First Edition : 16 November 2024
# Updates: 9 April 2026
# Hosting institute: ICM-CSIC
# R-Version

###  ----------------------------- ###
####       FUNCTIONS & LIBRARY    ####
###  ----------------------------- ###
library("cowplot")
library(dplyr)
library(ggplot2)
library(ggmap) # Needs access to internet
library(ggspatial)
library(ncdf4)
library(raster)
library(marmap) 
library(sf)
library(Matrix)
library(ConnMatTools)

# ---- Functions ----

# TOTAL NUMBER OF CONNECTION HAPPENING FROM A SOURCE  
Tot_Link <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name)]) %>% 
          group_by(ID_Source) %>% 
          dplyr::count(ID_Source)
}

# TOTAL NUMBER OF DIFFERENT CONNECTION HAPPENING FROM A SOURCE PER TIME UNIT 
Diff_Link <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name)]) %>% 
          group_by(ID_Source,across(Sink_name)) %>% 
          dplyr::count(across(Sink_name)) %>% 
          filter(n ==1) %>%
          group_by(ID_Source) %>% 
          dplyr::count(ID_Source)}

q <- 5
Hill_lambda <- function(x,Sink_name,q){
     x[,c('ID_Source',Sink_name,'Count')] %>% 
          group_by(ID_Source,across(Sink_name)) %>% 
          summarise_at(vars(Count),list(n=sum)) %>%
          group_by(ID_Source) %>% 
          mutate(tot=sum(n)) %>%
          mutate(Hill=(n/tot)^q) %>%
          summarise_at(vars(Hill),list(Hill_l = function(x) sum(x)^(1/(1-q))))}

Hill_alpha <- function(x,Sink_name,Time_name,q){
     x[,c('ID_Source',Sink_name,Time_name,'Count')] %>% 
          group_by(ID_Source,across(Time_name),across(Sink_name)) %>% 
          summarise_at(vars(Count),list(n=sum)) %>%
          group_by(ID_Source,across(Time_name)) %>% 
          mutate(tot=sum(n)) %>%
          mutate(Hill=(n/tot)^q) %>%
          dplyr::summarise(across(Hill, sum),
                           across(tot,mean)) %>%
          group_by(ID_Source) %>% 
          mutate(W=tot/sum(tot)) %>%
          mutate(Hill_inter=Hill*W) %>%
          summarise_at(vars(Hill_inter),list(Hill_a = function(x) sum(x)^(1/(1-q))))
}


Tot_Link <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name)]) %>% 
          group_by(ID_Source) %>% 
          dplyr::count(ID_Source)
}

# PROPORTION OF DIFFERENT CONNECTION HAPPENING FROM A SOURCE PER TIME UNIT 
Var_Link <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name,'Count')]) %>% 
          group_by(ID_Source,across(Sink_name)) %>% 
          summarise_at(vars(Count),list(n=sum)) %>%
          group_by(ID_Source) %>% 
          mutate(tot=sum(n)) %>%
          filter(n ==1) %>%
          summarise_at(vars(n,tot),list(n1=sum,tot1=mean)) %>%
          mutate(var=n_n1/tot_tot1) 
}

# SCALING SOURCE WITH UNIQUE OR MULTIPLE CONNECTIONS HAPPENING FROM A SOURCE PER TIME UNIT 
N_scale_Link <- function(x,Sink_name,Time_name){
     unique(x[,c('ID_Source',Sink_name,Time_name,'Count')]) %>% 
          group_by(ID_Source,across(Time_name)) %>% 
          summarise_at(vars(Count),list(n=sum)) %>%
          group_by(ID_Source) %>% 
          summarise_at(vars(n),list(avg=mean)) %>%
          ungroup() %>%
          mutate(scale=scale(avg))}

# TOTAL NUMBER OF PARTICLES CONNECTING PER A SOURCE
Tot_Part <- function(x,Sink_name,Time_name){
     x[,c('ID_Source',Sink_name,Time_name,'Count')] %>% 
          group_by(ID_Source) %>% 
          summarise_at(vars(Count),list(tot.p=sum))}

# STRENGTH Variability OF LINK OCCURRENCE 
Diff_Part <- function(x, Sink_name,Time_name){
     x[,c('ID_Source',Sink_name,Time_name,'Count')] %>% 
          group_by(ID_Source,across(Sink_name),across(Time_name)) %>% 
          summarise_at(vars(Count),list(n=sum)) %>% 
          group_by(ID_Source,across(Sink_name)) %>% 
          # mutate(Bay.dup = ifelse(duplicated(across(Sink_name)) | duplicated(across(Sink_name), fromLast = TRUE), 1,0)) %>% filter(Bay.dup == 0) %>%
          filter(n() <= 1) %>%
          group_by(ID_Source) %>% 
          summarise_at(vars(n),list(diff.p=sum))
}

# Modification of ConnMatTools function
protected_function <- function (conn.mat, nev = dim(conn.mat)[1], delta = 0.1, theta = 0.05, 
                                M = 20, epsilon.lambda = 1e-04, epsilon.uv = 0.05, only.list = T, 
                                ...) {
     if (all(class(conn.mat) != "matrix")) 
          stop("Input conn.mat must be a matrix.")
     n = dim(conn.mat)[1]
     r <- eigs(conn.mat, nev = nev,use.arpack = FALSE, ...)
     l <- eigs(t(conn.mat), nev = nev,use.arpack = FALSE, ...)
     lambda_r <- r$values
     u <- r$vectors
     v <- l$vectors
     rm(r, l)
     index_real <- which(abs(Im(lambda_r)) < epsilon.lambda)
     lambda_r <- Re(lambda_r[index_real])
     u <- Re(u[, index_real])
     v <- Re(v[, index_real])
     k <- length(lambda_r)
     uv = colSums(u * v)
     u = sweep(u, 2, uv, "/")
     v = sweep(v, 2, uv, "/")
     donorRecipient <- u * v
     index_pos <- array(NA, dim = k)
     for (i in 1:k) {
          maxx <- max(donorRecipient[, i])
          minn <- min(donorRecipient[, i])
          if (maxx >= 0 && minn >= 0) {
               index_pos[i] <- 1
               next
          }
          if (maxx < 0 && minn < 0) {
               donorRecipient[, i] <- donorRecipient[, i] * -1
               index_pos[i] <- 1
               next
          }
          if (abs(maxx/minn) > 1/epsilon.uv) {
               index_pos[i] <- 1
               next
          }
          if (abs(maxx/minn) < epsilon.uv) {
               donorRecipient[, i] <- donorRecipient[, i] * -1
               index_pos[i] <- 1
               next
          }
          index_pos[i] <- 0
     }
     id_pos <- which(index_pos == 1)
     lambda_r <- lambda_r[id_pos]
     u <- u[, id_pos]
     v <- v[, id_pos]
     donorRecipient <- donorRecipient[, id_pos]
     k <- length(lambda_r)
     sites <- array(dim = c(n, k))
     valueForSites <- array(dim = c(n, k))
     for (i in 1:k) {
          a <- sort(donorRecipient[, i], decreasing = T, index.return = T)
          sites[, i] <- a$ix
          valueForSites[, i] <- a$x
     }
     modifiedValuesForSites <- valueForSites
     modifiedValuesForSites[(M + 1):n, ] <- theta - 1000
     zeroedValuesForSites <- modifiedValuesForSites
     zeroedValuesForSites[zeroedValuesForSites < theta] <- 0
     predictedValues <- lambda_r * (1 + delta * colSums(zeroedValuesForSites))
     listOrder <- sort(predictedValues, decreasing = T, index.return = T)$ix
     finalList <- vector()
     for (i in 1:k) {
          finalList <- c(finalList, sites[modifiedValuesForSites[, 
                                                                 listOrder[i]] > theta, listOrder[i]])
     }
     finalList <- unique(finalList)
     if (only.list) {
          return(finalList)
     }
     else {
          return(list(finalList = finalList, predictedValues = predictedValues))
     }
}


'%nin%' = Negate('%in%')
# ---- End Function ----
# 

# ---- GIS Files ----
# Download the MPAs of the NW Mediterranean Sea (Spanish and French waters) XXXXX

Layer_NTZ <-  st_read('~/Downloads/MPA_capas.gpkg',layer="Closed_Areas_Feb_2022")

FRAzone1 <- st_read('~/Downloads/zones_cantonnement/zone1.shp')
FRAzone2 <- st_read('~/Downloads/zones_cantonnement/zone2.shp')
FRAzone3 <- st_read('~/Downloads/zones_cantonnement/zone3.shp')
FRAzone1$value=3
FRAzone2$value=2
FRAzone3$value=1

FRA_Layer <- st_cast(dplyr::bind_rows(list(FRAzone1,FRAzone2,FRAzone3)),'MULTIPOLYGON')
names(FRA_Layer)[1] <- 'ID'

names(Layer_NTZ)[1] <- 'ID'
ID_MPA <- grep("Escamarlà",Layer_NTZ[['name']])
ID_perm <- grep("Permanent",Layer_NTZ[['closed']])

nc <-Layer_NTZ
nc <- st_centroid(nc)

nc.sort <- nc %>% cbind(st_coordinates(.)) %>%
     arrange(-Y) %>% mutate(ID_sort = 1:n() + 3) %>% dplyr::select(ID,ID_sort) 
nc.sort$y_i <- st_coordinates(nc.sort)[,2]


nc <-FRA_Layer
nc <- st_centroid(nc)
nc$ID_sort = nc$ID
nc$ID = nc$ID
nc$y_i <- st_coordinates(nc)[,2]

nc.sort <- rbind(as.data.frame(nc.sort) %>% dplyr::select(-geom),as.data.frame(nc) %>% dplyr::select(-geometry))

# Donwload the GSA
GSA_shp <- st_read('~/Documents/QGIS_FILE/GSAs_simplified/GSAs_simplified.shp',quiet=T) # GSA shapefile available at XXXXX

# For distance calculation
r.mask <- raster("~/Downloads/CMEMS_mask.nc", varname = "mask") # Marine Copernicus NETCDF Product ID: XXXXX
r.trans <- gdistance::geoCorrection(gdistance::transition(r.mask,transitionFunction = function(x) 1,directions=8), type='c')

#Download the shapefile of the coastline
ShorePolyA <- st_read('/Volumes/Crucial X6/Mac Else/UCD_PostDoc/FILES/Europe_coastline_shapefile/Europe_coastline_poly.shp',quiet=T) # Shapefile available at EEA coastline for analysis (v.3)
# Reproject to +init=epsg:4326 and crop it to the area of interest
ShorePolyB <- st_transform(ShorePolyA,crs='+init=epsg:4326');box <- c(xmin=-1,ymin=38,xmax=6,ymax=44)
ShorePolyC <- st_crop(ShorePolyB,st_bbox(box))
rm(ShorePolyA,ShorePolyB)

ga_grid <- ShorePolyC %>% 
     st_make_grid(n = c(180, 180))

# Download estimation of the lobster habitat got from the jSDM model; see Publications: XXXXX
Habitat <-  st_read("~/Downloads/Polygon_extent_nobuffer/v3_delta_Nephrops_habitat_05_1000m_threshold.shp")

# Get Bathymetry. Only work with online connection
NWmed <- getNOAA.bathy(lon1 = -2, lon2 = 7,
                       lat1 = 37, lat2 = 44, resolution = 1)
dat <- fortify(NWmed)

# Make grids
r <- raster(ncol=100, nrow=150, xmn=-1, xmx=7.2,  ymn=38, ymx=43.5) # raster MNS

grid <- st_make_grid(st_as_sfc(st_bbox(c(xmin = -1, xmax = 7.2, ymin = 38, ymax = 43.5))), cellsize = c(0.082, 0.03666667))
grid_sf <- st_sf(geometry = grid)
grid_sf$ID <- 1:nrow(grid_sf)

# r <- raster(ncol=200, nrow=200, xmn=-0.3, xmx=6.08,  ymn=38, ymx=43.1) # raster Nixon
values(r) <- 1:length(values(r))
r.df=as.data.frame(r,xy=T);colnames(r.df)[3]='ID'
Db_fin_sf <- st_as_sf(r.df, coords = c('x','y'), crs = 4326) # Make spatial data
intersections <- st_join(Db_fin_sf, GSA_shp)
intersections_unique <- intersections %>%
     group_by(ID) %>%
     slice(1) 
r.df = base::merge(r.df,intersections_unique[,c('ID','SECT_COD')])


r.df2 <- r.df
colnames(r.df2)[1:3] <- c('ID_ri','x_r','y_r')

# ---- End GIS Files ----


###  --------------------###
###  ---------------------- ###
###  ------------------------- ###
###  ----------------------------- ###
####        Start analysis         ####
###  ----------------------------- ###
###  ------------------------- ###
###  --------------------- ###
###  ------------------ ###

 

###  ------------------- ###
####         MAPS        ####
###  ------------------- ###

load('~/path_to_file/Results_lobster_full.RData') # Name: "results", Results_lobster_full.RData is provided in Github repository Nephrops_IBM_files

### --------- Plot the average density over the X years

## Process the dataframe
all_drift_reduced <- subset(results,in_habitat_i ==0) %>% 
     filter(in_habitat==0) %>% 
     select(ID_rf,year,count) %>%
     group_by(ID_rf,year) %>%
     summarise(n=sum(count))%>%
     group_by(ID_rf) %>%
     summarise(avg=mean(n))

all_drift_reduced <- merge(all_drift_reduced,r.df,by.x='ID_rf',by.y='ID')


## For Figure 2
## --- Keep data in format for QGIS
all_drift_reduced_dens <- all_drift_reduced %>%
     tidyr::uncount(floor(avg))
#st_write(st_as_sf(all_drift_reduced_dens, coords = c('x','y'), crs = 4326), "IBM_NOR.gpkg", layer = "Larval_density_avg", delete_layer = TRUE)
#st_write(grid_sf, "IBM_NOR.gpkg", layer = "Grid", delete_layer = TRUE, append = TRUE)



## INFORMATION in MNS
## Section 3.1

## -- Interannual Percentage of PostLarvae in habitat
subset(results,in_habitat_i ==0) %>% ungroup() %>% 
     group_by(year,in_habitat) %>%
     summarise(n=sum(count))  %>%
     group_by(year) %>%
     mutate(n*100/sum(n)) %>% filter(in_habitat==0)

## -- Interannual Percentage of PostLarvae not in habitat, above or below continental slope
View(subset(results,in_habitat_i ==0) %>% ungroup() %>% 
          mutate(isAbove = z > -200, isBelow = z < -1000) %>% 
          group_by(year,in_habitat,isAbove,isBelow) %>%
          summarise(n=sum(count))  %>%
          group_by(year) %>%
          mutate(n*1000/sum(n)) %>% filter(is.na(in_habitat)))



## Map of the NW with Postlarvae densities (FIGURE 2 R version)
map2 <- ggplot(all_drift_reduced) +
     geom_tile(aes(x=x, y= y, fill = avg)) +
     scale_fill_viridis_c(name='Density \n(larvae/km2)', option='viridis',direction=-1) +  # Better color scale for density
     theme_bw() +
     theme(panel.grid=element_blank(),
           axis.title=element_text(size=12),axis.text = element_text(size=12),
           legend.frame = element_rect(color='black',linewidth = 0.2),
           legend.ticks = element_line(color='black',linewidth = 0.2),
           panel.grid.major=element_line(linetype=2,color='grey',linewidth=0.1)
     )+
     scale_x_continuous(name='',expand = c(0,0)) +
     scale_y_continuous(name='', expand = c(0,0)) 

map2 + 
     coord_sf(xlim=c(-1,5), ylim=c(38,43.5))+ 
     annotation_scale(location='br',height=unit(0.15,'cm'))+
     annotation_north_arrow(height=unit(0.85,'cm'),width= unit(0.85,'cm'),
                            location = "tl", which_north = "true",style=north_arrow_fancy_orienteering())


## For Figure 2
## --- Keep data in format for QGIS
selected_cells <- r_sf[unique(all_drift_reduced$ID_rf),] %>%
     base::merge(all_drift_reduced %>% select(ID_rf,avg),by.x='layer',by.y='ID_rf')
#st_write(selected_cells, "IBM_NOR.gpkg", layer = "Avg Larvae settlement", delete_layer=TRUE,append = TRUE)


## INFORMATION in MNS
## Section 3.1
## -- Calculate the number of larvae reaching the MPAs
all_drift_reduced <- results %>% 
     filter(in_habitat==0) %>% 
     select(ID_rf,year,count,ID_MPA_f) %>%
     group_by(ID_rf,year,ID_MPA_f) %>%
     summarise(n=sum(count)) %>%
     group_by(ID_rf) %>%
     mutate(has_na = all(is.na(ID_MPA_f))) %>%  
     filter(has_na==FALSE) %>% 
     group_by(ID_MPA_f,year) %>%
     summarise(ntot=sum(n)) %>%
     group_by(ID_MPA_f) %>%
     summarise(avg=mean(ntot), sd=sd(ntot)) %>%
     filter(!is.na(ID_MPA_f)) %>%
     plyr::join(as.data.frame(Layer_NTZ %>% mutate(ID_MPA_f = ID_lat ))) %>% 
     mutate(avg2=avg/area)

## -- Define the origin of larvae settling in ID_Lat 14 and 17 (Figure S2 and S3)
ID_selected = 14
# ID_selected = 17

#### Average map (origin of larvae reaching ID_selected)

map2 <- ggplot(results %>% 
                    filter(ID_MPA_f %in% c(ID_selected)) %>%
                    select(ID_ri,year,count,ID_MPA_f,x_i,y_i) %>%
                    group_by(ID_ri, ID_MPA_f) %>%
                    summarise(x_i=mean(x_i), y_i=mean(y_i), n=sum(count))
) +
     geom_tile(aes(x=x_i, y= y_i, fill = n),height=0.05, width =0.05) +
     scale_fill_viridis_c(name='Density \n(larvae/km2)', option='viridis',direction=-1) +  # Better color scale for density
     theme_bw() + 
     theme(panel.grid=element_blank(),
           axis.title=element_text(size=12),axis.text =element_text(size=12),
           legend.frame = element_rect(color='black',linewidth = 0.2),
           legend.ticks = element_line(color='black',linewidth = 0.2),
           #legend.position = c(0.2, 0.8)
           panel.grid.major=element_line(linetype=2,color='grey',linewidth=0.1)
     )+
     scale_x_continuous(name='',expand = c(0,0)) +
     scale_y_continuous(name='', expand = c(0,0)) 

map2 

#### Maps per year (origin of larvae reaching ID_selected)

map2 <- ggplot(results %>%
                    filter(ID_MPA_f %in% c(ID_selected)) %>%
                    select(ID_ri,year,count,ID_MPA_f,x_i,y_i) %>%
                    group_by(ID_ri, ID_MPA_f,year) %>%
                    summarise(x_i=mean(x_i), y_i=mean(y_i), n=sum(count))
) +
     geom_tile(aes(x=x_i, y= y_i, fill = n),height=0.05, width =0.05) +
     scale_fill_viridis_c(name='Density \n(larvae/km2)', option='viridis',direction=-1) +  # Better color scale for density
     theme_bw() + facet_wrap(.~ year) +
     theme(panel.grid=element_blank(),
           axis.title=element_text(size=12),axis.text =element_text(size=12),
           legend.frame = element_rect(color='black',linewidth = 0.2),
           legend.ticks = element_line(color='black',linewidth = 0.2),
           legend.position = 'bottom',
           panel.grid.major=element_line(linetype=2,color='grey',linewidth=0.1)
     )+
     scale_x_continuous(name='',expand = c(0,0),breaks= c(0,3,6)) +
     scale_y_continuous(name='', expand = c(0,0), breaks = c(38,40,42,44)) 

map2 


## -- Percentage of settled larvae protected by MPA

results %>% 
     filter(in_habitat==0) %>% 
     select(ID_rf,year,count,ID_MPA_f) %>%
     group_by(ID_rf,year,ID_MPA_f) %>%
     summarise(n=sum(count)) %>%
     group_by(ID_rf) %>%
     mutate(has_na = all(is.na(ID_MPA_f))) %>%  
     group_by(has_na,year) %>%
     summarise(ntot=sum(n)) %>%
     group_by(has_na) %>%
     summarise(avg=mean(ntot), sd=sd(ntot)) 

## -- Information about PL arriving in Balearic Archipelago (Section 3.1) - Take time

res2 <- data.frame()
for (i in unique(results$date_release)){
     r.df2 <- r.df2 %>% rename(ID_rf = ID_ri) 
     RES <- results %>% filter(date_release==i)  %>% group_by(ID_rf) %>%  dplyr::left_join(r.df2,by='ID_rf') %>% filter(SECT_COD != 'GSA05') %>% 
          rename(GSA_f = SECT_COD) %>% select(-geometry,-x_r,-y_r) # elimin
     
     r.df2 <- r.df2 %>% rename(ID_ri = ID_rf) 
     RES <- RES %>% filter(date_release == i)  %>% group_by(ID_ri) %>%  dplyr::left_join(r.df2,by='ID_ri') %>% 
          rename(GSA_i = SECT_COD) %>% select(-geometry,-x_r,-y_r)
     res2 <- rbind(res2,RES)
}

View(res2 %>% 
          filter(in_habitat==0) %>% 
          select(year,count,GSA_i) %>%
          group_by(year,GSA_i) %>%
          summarise(n=sum(count)) %>%
          group_by(year) %>%
          mutate(tot = sum(n)) %>%
          filter(GSA_i != 'GSA05') %>%
          mutate(n2 = sum(n))%>%
          mutate(avg = n2/tot ))
rm(res2)


## Calculate the distance drift by PL (subseted because the calculation is too long)
all_drift_reduced <- results %>% filter(!is.na(ID_MPA_f) & in_habitat==0)
filter(in_habitat==0)#### check if it is well prepared because I ve deleted some code lines here.

Shortest_dist <- diag(gdistance::costDistance(r.trans, as.matrix(all_drift_reduced[1:10000, c('x_i','y_i')]),
                                              as.matrix(all_drift_reduced[1:10000, c('x','y')])))/1000
Shortest_dist2 <- diag(gdistance::costDistance(r.trans, as.matrix(all_drift_reduced[10001:20000, c('x_i','y_i')]),
                                               as.matrix(all_drift_reduced[10001:20000, c('x','y')])))/1000
Shortest_dist3 <- diag(gdistance::costDistance(r.trans, as.matrix(all_drift_reduced[20001:27367, c('x_i','y_i')]),
                                               as.matrix(all_drift_reduced[20001:27367, c('x','y')])))/1000

## For FIGURE 4B
all_drift_reduced$Distance_if <- c(Shortest_dist,Shortest_dist2,Shortest_dist3)
base::merge(all_drift_reduced %>% group_by(ID_MPA_f) %>% 
                 summarise(km_avg = mean(Distance_if)), nc.sort,by.x='ID_MPA_f',by.y='ID')


###  ------------------- ###
####         EPTs        ####
###  ------------------- ###
## Spatial grid is the same as above

load('~/path_to_file/Results_lobster_subset.RData') # Name: "res", The file is provided in Github repository Nephrops_IBM_files

## ------ Expanded MPAs --------

results_all <- subset(res, in_habitat_i==0 & in_habitat==0
                      & GSA_f %nin% c('GSA111')) %>%
     filter((is.na(ID_MPA_i) & !is.na(ID_MPA_f)) |  !is.na(ID_MPA_i) & is.na(ID_MPA_f)) %>% # (!is.na(ID_MPA_i) & !is.na(ID_MPA_f)) |
     ungroup()

## --- Preparation and application of EPT
# Rename ID_rX with the ID of MPAs
results_all$ID_ri[!is.na(results_all$ID_MPA_i)] <- results_all$ID_MPA_i[!is.na(results_all$ID_MPA_i)]
results_all$ID_rf[!is.na(results_all$ID_MPA_f)] <- results_all$ID_MPA_f[!is.na(results_all$ID_MPA_f)]
summary(results_all$ID_ri)

all_drift_reduced <- results_all %>% group_by(ID_ri,ID_rf) %>%
     summarise(count=sum(count)) %>% ungroup()

## Make a squared matrix ## Make a squared matrixy_i
ID_u <- unique(c(all_drift_reduced$ID_ri,all_drift_reduced$ID_rf))
df_ID <- data.frame(ID_ri=ID_u,ID_rf=ID_u,ID_new = 1:length(ID_u))

all_drift_reduced$ID_i1 <- plyr::join(all_drift_reduced,df_ID,by='ID_ri')$ID_new
all_drift_reduced$ID_f1 <- plyr::join(all_drift_reduced,df_ID,by='ID_rf')$ID_new

all_drift_reduced <- as.data.frame(all_drift_reduced)

p <- sparseMatrix(
     i = all_drift_reduced[,4],    # Row indices
     j = all_drift_reduced[,5],    # Column indices
     x = all_drift_reduced[,3]  )

## Create a larger sparse matrix with dimensions 100x100
p2 <- Matrix(0, nrow = dim(p)[2], ncol = dim(p)[2], sparse = TRUE)

## Insert values from the original sparse matrix into the larger sparse matrix
p2[1:dim(p)[1], 1:dim(p)[2]] <-p
n=dim(p2)[1]

## Associate spatial coordinates to the grid
coord <- aggregate(cbind(x_i,y_i)~ID_ri,results_all,mean)
coord2 <- aggregate(cbind(x,y)~ID_rf,results_all[!(results_all$ID_rf %in% coord$ID_ri),],mean) # careful with GSA
names(coord2) <- c("ID_ri","x_i","y_i")

coord <- rbind(coord,coord2)
coord$ID <- plyr::join(coord,df_ID,by='ID_ri')$ID_new
coord <- coord[order(coord$ID),]

## Normalization
pp <- (p2 + t(p2)) / 2.0
diag_vals <- 1 / (rowSums(pp) + 1e-10)
dia <-  Diagonal(n, diag_vals) # sparseMatrix(i = 1:length(diag_vals), j = 1:length(diag_vals), x = diag_vals)

pp <- pp %*% dia
pp <- (pp + t(pp)) / 2.0

# Set diagonal elements to 0
diag(pp) <- 0
num <- prod(dim(pp)) / sum(pp)

### Apply function from ConnMartools R Package
val <- data.frame(ID=protected_function(as.matrix(pp),only.list=F)[[1]], MPA=1)
#val$ID_save <- val$ID

pops.EPT_MPA_expanded_all <- left_join(coord, val, by = "ID")

## --- Cleaning the selection of new MPAs
# How many sites are designed as MPA? How much of them have repeated locations?
Db_fin_sf <- st_as_sf(pops.EPT_MPA_expanded_all, coords = c('x_i','y_i'), crs = 4326) # Make spatial data
pops.EPT_MPA_expanded_all$inMPA <- st_join(Db_fin_sf,Layer_NTZ[ID_perm,])$ID_lat# Extract MPA
pops.EPT_MPA_expanded_all$inMPA[pops.EPT_MPA_expanded_all$ID_ri==1]=1 # French MPAs
pops.EPT_MPA_expanded_all$inMPA[pops.EPT_MPA_expanded_all$ID_ri==3]=3 # French MPAs

nrow(subset(pops.EPT_MPA_expanded_all, MPA == 1)) # total new potential MPAs

trans <- subset(pops.EPT_MPA_expanded_all, MPA == 1) #  Select the gridcell designed as a MPA

# 1) Find overlapping new MPAs with existing ones
results_bis <- results
results_bis$ID_ri[!is.na(results_bis$ID_MPA_i)] <- results_bis$ID_MPA_i[!is.na(results_bis$ID_MPA_i)]
results_bis$ID_rf[!is.na(results_bis$ID_MPA_f)] <- results_bis$ID_MPA_f[!is.na(results_bis$ID_MPA_f)]

# 2) From main dataset (results/results_bis), subset particles associated with MPAs
newMPA <- subset(results_bis, ID_ri %in% trans$ID_ri)
newMPA <- subset(newMPA, ID_rf %in% trans$ID_ri)
#newMPA <- subset(newMPA, !is.na(ID_MPA_i))

length(unique(newMPA$ID_ri)) # recalculated total of new potential MPAs

#3) Explore if new potential MPAs make a connection  with others along the 12 years
all.drift.reduced <- aggregate(count~ID_ri + year,newMPA,sum)
ggplot(all.drift.reduced)+geom_tile(aes(as.factor(year),as.factor(ID_ri),fill=count)) # Visual information

# keep MPAs stayed connected over the 12 years and keep already existing MPAs
newMPA_df <- as.data.frame(table(all.drift.reduced$ID_ri))

all_drift_reduced <- subset(all.drift.reduced, ID_ri %in% as.numeric(as.character(subset(newMPA_df, Freq > 11)$Var1)) | ID_ri < 100)
#all_drift_reduced <- all.drift.reduced
length(unique(all_drift_reduced$ID_ri)) #year

trans <- unique(all_drift_reduced$ID_ri)
trans2 <- subset(pops.EPT_MPA_expanded_all,ID_ri %in% trans)
trans2 %>% filter(MPA == 1) %>% group_by(ID_ri) %>% mutate(Freq=n())

ggplot(all_drift_reduced)+geom_tile(aes(as.factor(year),as.factor(ID_ri),fill=count))  # Visual information


# --- Preparation for calculation of connectivity indicators
newMPA$count <-1

# number of New sites already above a MPA
unique(newMPA$ID_MPA_i[newMPA$ID_ri==trans])
unique(newMPA$ID_MPA_i[newMPA$ID_rf==trans])
 
DataBase_expanded <- subset(newMPA,ID_ri %in% subset(newMPA_df, Freq > 11)$Var1 | ID_ri < 100) %>%
     dplyr::select(c(ID_ri,ID_rf,year,count)) %>%
     filter(ID_rf %in% ID_ri)


colnames(DataBase_expanded) <- c('ID_Source','ID_Sink','Year','Count')
length(unique(DataBase_expanded$ID_Source))

# --- Aditional preparation for QGIS plot (Figure 3)
newMPA_expanded <- trans2

r_sp <- rasterToPolygons(r, dissolve = FALSE)
r_sf <- st_as_sf(r_sp)
st_crs(r_sf) = crs(r)

selected_cells <- r_sf[unique(newMPA_expanded$ID_ri),]
# Find which polygons are touching
touching_groups <- st_touches(r_sf[unique(newMPA_expanded$ID_ri),])
# Create graph of touching features
g <- igraph::graph_from_adj_list(touching_groups)
# Find connected components (i.e. groups that touch)
components <- components(g)$membership
# Add component ID to the data
selected_cells$group_id <- components

## For Figure 3
# st_write(selected_cells, "IBM_NOR.gpkg", layer = "ExpandedEPTMPA_updated", delete_layer=TRUE,append = TRUE)


DataBase_expanded$ID_Source <- merge(DataBase_expanded,selected_cells,by.x='ID_Source',by.y='layer')$group_id
DataBase_expanded$ID_Sink <- merge(DataBase_expanded,selected_cells,by.x='ID_Sink',by.y='layer')$group_id


## ------ END Expanded MPAs --------

## ------ De novo MPAs --------

results_all <- subset(res, in_habitat_i==0  & in_habitat==0
                      & GSA_f %nin% c('GSA111'))

## --- Preparation and application of EPT
all_drift_reduced <- aggregate(count~ID_ri+ID_rf,results_all,sum)

## Make a squared matrix
ID_u <- unique(c(all_drift_reduced$ID_ri,all_drift_reduced$ID_rf))
df_ID <- data.frame(ID_ri=ID_u,ID_rf=ID_u,ID_new = 1:length(ID_u))

all_drift_reduced$ID_i1 <- plyr::join(all_drift_reduced,df_ID,by='ID_ri')$ID_new
all_drift_reduced$ID_f1 <- plyr::join(all_drift_reduced,df_ID,by='ID_rf')$ID_new

p <- sparseMatrix(
     i = all_drift_reduced[,4],    # Row indices
     j = all_drift_reduced[,5],    # Column indices
     x = all_drift_reduced[,3]  )

## Create a larger sparse matrix with dimensions 100x100
p2 <- Matrix(0, nrow = dim(p)[2], ncol = dim(p)[2], sparse = TRUE)

## Insert values from the original sparse matrix into the larger sparse matrix
p2[1:dim(p)[1], 1:dim(p)[2]] <-p
n=dim(p2)[1]
## Associate spatial coordinates to the grid
coord <- aggregate(cbind(x_i,y_i)~ID_ri,results_all,mean)
# coord2 <- aggregate(cbind(x,y)~ID_rf,results_all[!(results_all$ID_rf %in% coord$ID_ri),],mean) # careful with GSA
coord2 <- aggregate(cbind(x,y)~ID_rf,results_all[!(results_all$ID_rf %in% coord$ID_ri),],mean) # careful with GSA

names(coord2) <- c("ID_ri","x_i","y_i")

coord <- rbind(coord,coord2)
coord$ID <- plyr::join(coord,df_ID,by='ID_ri')$ID_new
coord <- coord[order(coord$ID),]

## Normalization
pp <- (p2 + t(p2)) / 2.0
diag_vals <- 1 / (rowSums(pp) + 1e-10)
dia <-  Diagonal(n, diag_vals) # sparseMatrix(i = 1:length(diag_vals), j = 1:length(diag_vals), x = diag_vals)

pp <- pp %*% dia
pp <- (pp + t(pp)) / 2.0

# Set diagonal elements to 0
diag(pp) <- 0
num <- prod(dim(pp)) / sum(pp)

### Apply function from ConnMartools R Package
val <- data.frame(ID=protected_function(as.matrix(pp),only.list=F)[[1]], MPA=1)

pops.EPT_MPA_new_all <- left_join(coord, val, by = "ID")

## --- Cleaning the selection of new MPAs
# How many sites are designed as MPA? How much of them have repeated locations?
nrow(subset(pops.EPT_MPA_new_all, MPA == 1)) # total

# 1) From main dataset (results), subset particles associated with MPAs
trans <- subset(pops.EPT_MPA_new_all, MPA == 1)
newMPA.df <- trans %>% filter(MPA == 1)

newMPA <- subset(results, ID_ri %in% unique(newMPA.df$ID_ri))
newMPA <- subset(newMPA, ID_rf %in% unique(newMPA.df$ID_ri))
length(unique(newMPA$ID_ri))


# Do a matrix
all.drift.reduced <- aggregate(count~ID_ri + year,newMPA,sum)
ggplot(all.drift.reduced)+geom_tile(aes(as.factor(year),as.factor(ID_ri),fill=count))

#3) Explore if new potential MPAs make a connection  with others along the 12 years
newMPA_df <- as.data.frame(table(all.drift.reduced$ID_ri))

all_drift_reduced <- subset(all.drift.reduced, ID_ri %in% as.numeric(as.character(subset(newMPA_df, Freq > 11)$Var1)))
#all_drift_reduced <- all.drift.reduced
length(unique(all_drift_reduced$ID_ri)) #year

trans <- unique(all_drift_reduced$ID_ri)
trans2 <- subset(pops.EPT_MPA_new_all,ID_ri %in% trans)
#trans2 %>% filter(MPA == 1) %>% group_by(ID_ri) %>% mutate(Freq=n()) %>% group_by(year) %>% summarise(n=n())

ggplot(all_drift_reduced)+geom_tile(aes(as.factor(year),as.factor(ID_ri),fill=count))

# --- Preparation for calculation of connectivity indicators
newMPA$count <-1

trans <- as.character(subset(newMPA_df, Freq > 11)$Var1)

DataBase_new <- newMPA %>% filter(ID_ri %in% trans, ID_rf %in% trans) %>%
     dplyr::select(c(ID_ri,ID_rf,year,count))

# number of New sites already above a MPA
unique(newMPA$ID_MPA_i[newMPA$ID_ri==trans])
unique(newMPA$ID_MPA_i[newMPA$ID_rf==trans])
newMPA_novo <-  trans2
colnames(DataBase_new) <- c('ID_Source','ID_Sink','Year','Count')
length(unique(DataBase_new$ID_Source))

## --- Additional preparation for QGIS plot (Figure 3)
### Merge the MPA which edges are shared

r_sp <- rasterToPolygons(r, dissolve = FALSE)
r_sf <- st_as_sf(r_sp)
st_crs(r_sf) = crs(r)

selected_cells <- r_sf[unique(newMPA_novo$ID_ri),]
# Find which polygons are touching
touching_groups <- st_touches(r_sf[unique(newMPA_novo$ID_ri),])
# Create graph of touching features
g <- graph_from_adj_list(touching_groups)
# Find connected components (i.e. groups that touch)
components <- components(g)$membership
# Add component ID to the data
selected_cells$group_id <- components

## For Figure 3
# st_write(selected_cells, "IBM_NOR.gpkg", layer = "NewEPTMPA",delete_layer = TRUE,append = TRUE)

DataBase_new$ID_Source <- merge(DataBase_new,selected_cells,by.x='ID_Source',by.y='layer')$group_id
DataBase_new$ID_Sink <- merge(DataBase_new,selected_cells,by.x='ID_Sink',by.y='layer')$group_id


## ------ END De NOVO MPAs --------


###  ---------------------------------------------- ###
####         Calculation of general metrics        ####
###  ---------------------------------------------- ###

### -------- Larvae reaching a suitable habitat  -------- 
## -- MPA
data <- results %>% filter(!is.na(ID_MPA_i), in_habitat_i==0) %>% group_by(year,in_habitat) %>% summarise(n=n())%>% 
     group_by(year) %>% mutate(perc=n*100/sum(n))
data$MPA <- 'Existing'

## -- MPA extended
data2 <- results %>% filter(!is.na(ID_MPA_i) | ID_ri %in% DataBase_expanded$ID_Source, in_habitat_i==0) %>% 
     group_by(year,in_habitat) %>% summarise(n=n())%>% 
     group_by(year) %>% mutate(perc=n*100/sum(n))
data2$MPA <- 'Expanded'

## -- MPA de novo
data3 <- results %>% filter(ID_ri %in% DataBase_new$ID_Source, in_habitat_i==0) %>% group_by(year,in_habitat) %>% summarise(n=n())%>% 
     group_by(year) %>% mutate(perc=n*100/sum(n))
data3$MPA <- 'New'

hab.plot <- ggplot(rbind(data,data2,data3) %>% filter(in_habitat == 0) %>% group_by(MPA) %>% 
                        summarise(Perc_avg=mean(perc),Perc_max=mean(perc)+sd(perc),Perc_min=mean(perc)-sd(perc)))
hab.PLOT <- hab.plot +
     geom_col(aes(MPA,Perc_avg,fill=MPA))+
     geom_errorbar(aes(x=MPA,y=Perc_avg,ymin=Perc_min,ymax=Perc_max),width=0.2)+
     geom_errorbar(data= data3 %>% filter(in_habitat == 0) %>% group_by(MPA) %>% 
                        summarise(Perc_avg=mean(perc),Perc_max=mean(perc)+sd(perc),Perc_min=mean(perc)-sd(perc))
                   , aes(x=MPA,y=Perc_avg,ymin=Perc_min,ymax=Perc_avg),color='white',width=0.2)+
     theme_bw()+xlab('')+ylab('Percentage of larvae \nover a suitable habitat')+
     theme(legend.position = 'bottom', axis.text.y =element_text(size=16),axis.text.x = element_blank(),axis.title=element_text(size=18),
           legend.key.spacing.x = unit(10, "pt"))+
     scale_fill_manual(name='MPAs network status:',values=c('lightblue','darkgrey','black'))+
     scale_y_continuous(expand=c(0,0),limits=c(0,50))+
     guides(fill = guide_legend(label.position = "bottom",title.position="top",hjust=0.5,
                                keywidth=3,keyheight=0.5))




### -------- Larvae age in a suitable habitat when settling  -------- 
## -- MPA
data <- results %>% filter(!is.na(ID_MPA_i)) %>% filter(in_habitat==0, in_habitat_i==0) %>% group_by(year) %>% 
     summarise(avg=mean(PL_age_real),max=mean(PL_age_real)+sd(PL_age_real),min=mean(PL_age_real)-sd(PL_age_real))
data$MPA <- 'Existing'
## -- MPA extended
data2 <- results %>% filter(!is.na(ID_MPA_i) | ID_ri %in% DataBase_expanded$ID_Source) %>% filter(in_habitat==0, in_habitat_i==0)  %>% group_by(year) %>%
     summarise(avg=mean(PL_age_real),max=mean(PL_age_real)+sd(PL_age_real), min=mean(PL_age_real)-sd(PL_age_real))
data2$MPA <- 'Expanded'
## -- MPA de novo
data3 <- results %>% filter(ID_ri %in% DataBase_new$ID_Source)  %>% filter(in_habitat==0, in_habitat_i==0) %>% group_by(year) %>%
     summarise(avg=mean(PL_age_real),max=mean(PL_age_real)+sd(PL_age_real), min=mean(PL_age_real)-sd(PL_age_real))
data3$MPA <- 'New'

age.plot <- ggplot(rbind(data,data2,data3))
age.PLOT <- age.plot +
     geom_line(aes(x=year,y=avg,color=MPA))+
     scale_color_manual(values=c('lightblue','darkgrey','black'))+
     theme_bw()+xlab('')+ylab('Age of larval \nsettlement (day)')+
     scale_x_continuous(expand=c(0,0),breaks=2011:2022)+
     theme(axis.text=element_text(size=16),axis.text.x=element_blank(),axis.title=element_text(size=18),legend.position='none',
           plot.margin = margin(t = 10,  # Top margin
                                r = 10,  # Right margin
                                b = 1,  # Bottom margin
                                l = 4))  # Left margin)

### -------- Larvae transported distance in a suitable habitat when settling  -------- 
## -- MPA
data <- results %>% filter(!is.na(ID_MPA_i)) %>% filter(in_habitat==0) 
data$dist=0
for (yy in 2011:2022){
     data$dist[data$year==yy] <- diag(gdistance::costDistance(r.trans, as.matrix(data.frame(data$x_i[data$year==yy],data$y_i[data$year==yy])),
                                                              as.matrix(data.frame(data$x[data$year==yy],data$y[data$year==yy])))/1000)
}
DATA <- data %>%
     group_by(year) %>% 
     summarise(avg=mean(dist/PL_age_real),max=mean(dist)+sd(dist),min=mean(dist)-sd(dist))
DATA$MPA <- 'Existing'

## -- MPA extended
data2 <- results %>% filter(!is.na(ID_MPA_i) | ID_ri %in% DataBase_expanded$ID_Source)
data2$dist=0
for (yy in unique(data2$Release_time)){
     for (inrow in unique(data2$ID_ri[data2$Release_time==yy])){
          cat(yy)
          data2$dist[data2$Release_time==yy & data2$ID_ri==inrow] <- diag(gdistance::costDistance(r.trans, as.matrix(data.frame(data2$x_i[data2$Release_time==yy & data2$ID_ri==inrow],data2$y_i[data2$Release_time==yy & data2$ID_ri==inrow])),
                                                                                                  as.matrix(data.frame(data2$x[data2$Release_time==yy & data2$ID_ri==inrow],data2$y[data2$Release_time==yy & data2$ID_ri==inrow])))/1000)
     }
}
DATA2 <- data2 %>%
     group_by(year) %>% 
     summarise(avg=mean(dist/PL_age_real),max=mean(dist)+sd(dist),min=mean(dist)-sd(dist))
DATA2$MPA <- 'Expanded'

## -- MPA de novo
data3 <- results %>% filter(ID_ri %in% DataBase_new$ID_Source) 
data3$dist=0
for (yy in unique(data3$Release_time)){
     for (inrow in unique(data2$row[data2$Release_time==yy])){
          cat(yy)
          data3$dist[data3$Release_time==yy & data3$row==inrow] <- diag(gdistance::costDistance(r.trans, as.matrix(data.frame(data3$x_i[data3$Release_time==yy & data3$row==inrow],data3$y_i[data3$Release_time==yy & data3$row==inrow])),
                                                                                                as.matrix(data.frame(data3$x[data3$Release_time==yy & data3$row==inrow],data3$y[data3$Release_time==yy & data3$row==inrow])))/1000)
          cat(yy) }     
}

DATA3 <- data3 %>%
     group_by(year) %>% 
     summarise(avg=mean(dist/PL_age_real),max=mean(dist)+sd(dist),min=mean(dist)-sd(dist))
DATA3$MPA <- 'New'

dist.plot <- ggplot(rbind(DATA,DATA2,DATA3))
dist.PLOT <- dist.plot+
     #geom_ribbon(aes(x=year,y=avg,ymin=min,ymax=max,fill=MPA),alpha=0.2) +
     #scale_fill_manual(values=c('lightblue','darkgrey',rgb(1,1,1,alpha=0)))+
     geom_line(aes(x=year,y=avg,color=MPA))+
     scale_color_manual(values=c('lightblue','darkgrey','black'))+
     #geom_line(data=data3,aes(year,min),linetype=3,linewidth=0.2) +
     #geom_line(data=data3,aes(year,max),linetype=3,linewidth=0.2) +
     theme_bw()+xlab('Year')+ylab('Travelled \ndistance (km/day)')+
     scale_x_continuous(expand=c(0,0),breaks=2011:2022)+
     theme(axis.text=element_text(size=16),axis.text.x=element_text(angle=45,hjust=1),axis.title=element_text(size=18),
           axis.title.y= element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
           legend.position='none',
           plot.margin = margin(t = 10,  # Top margin
                                r = 10,  # Right margin
                                b = 1,  # Bottom margin
                                l = 4))  # Left margin)




### Arrange the three last plots with cowplot functions

library("cowplot")
ggdraw() +
     draw_plot(hab.PLOT, x = 0, y = 0, width = .37, height = 1) +
     draw_plot(age.PLOT, x = .4, y = .5, width = .6, height = .48) +
     draw_plot(dist.PLOT, x = .4, y = 0, width = .6, height = 0.55) +
     draw_plot_label(label = c("A", "B", "C"), size = 15,
                     x = c(0, 0.4, 0.4), y = c(1, 1, 0.55))



### -------- Maps of density from theses sources  -------- 

data <- results %>% filter(!is.na(ID_MPA_i)) %>% filter(in_habitat==0,in_habitat_i==0) 
data2 <- results %>% filter(!is.na(ID_MPA_i) | ID_ri %in% DataBase_expanded$ID_Source) %>% filter(in_habitat==0, in_habitat_i==0) 
data3 <- results %>% filter(ID_ri %in% DataBase_new$ID_Source) %>% filter(in_habitat==0, in_habitat==0) 

ggplot()+ 
     geom_sf(data=subset(Layer_NTZ,closed=='Permanent'),fill=NA, colour='red')+
     # stat_density_2d_filled(data=data2,aes(x,y,fill=after_stat(level)),geom='polygon')+
     geom_tile(data=all_drift_reduced,aes(x,y,fill=as.numeric(avg))) +
     scale_fill_manual(values=c('transparent',viridis::viridis(12))) +
     theme_bw() +
     theme(panel.grid=element_blank(),
           axis.title=element_text(size=12),axis.text =element_text(size=12),
           legend.frame = element_rect(color='black',linewidth = 0.2),
           legend.ticks = element_line(color='black',linewidth = 0.2),
           #legend.position = c(0.2, 0.8)
           panel.grid.major=element_line(linetype=2,color='grey',linewidth=0.1)
     )+
     scale_x_continuous(name='',expand = c(0,0)) +
     scale_y_continuous(name='', expand = c(0,0)) + # geom_sf(data=ShorePolyC)+
     coord_sf(xlim=c(-1,5), ylim=c(38,43.5))+ 
     annotation_scale(location='br',height=unit(0.15,'cm'))+
     annotation_north_arrow(height=unit(0.85,'cm'),width= unit(0.85,'cm'),
                            location = "tl", which_north = "true",style=north_arrow_fancy_orienteering())

### Save for MNS

all_drift_reduced <- data3 %>%
     dplyr::select(ID_rf,year,count) %>%
     group_by(ID_rf,year) %>%
     summarise(n=sum(count))%>%
     group_by(ID_rf) %>%
     summarise(avg=mean(n))

all_drift_reduced <- merge(all_drift_reduced,r.df,by.x='ID_rf',by.y='ID')

selected_cells <- r_sf[unique(all_drift_reduced$ID_rf),] %>%
     base::merge(all_drift_reduced %>% dplyr::select(ID_rf,avg),by.x='layer',by.y='ID_rf')
st_write(selected_cells, "IBM_NOR.gpkg", layer = "Larvae Settlement Expanded MPA", delete_layer=TRUE,append = TRUE)


## ------ END General metrics --------

###  ------------------- ###
####         Indicators       
###  ------------------- ### 

### ----- Normal network ----

DataBase <- subset(results, !is.na(ID_MPA_f) & !is.na(ID_MPA_i) & in_habitat==0)
DataBase <- DataBase[,c(7,12,20)]
colnames(DataBase) <- c('ID_Sink','ID_Source','year')
DataBase$Count <- 1
DataBase <- as.data.frame(DataBase)

Var_Link2 <- Var_Link(as.data.frame(DataBase),'ID_Sink','year')


# TOTAL NUMBER OF PARTICLES CONNECTING PER A SOURCE
N_scale_Link2 <-  N_scale_Link(as.data.frame(DataBase),'ID_Sink','year')
df <- base::merge(Var_Link2, N_scale_Link2, by='ID_Source',all.y=T)

#df$ID_Sort <-  base::merge(df, nc.sort,by.x='ID_Source',by.y='ID')$ID_sort
df$y_i <-  base::merge(df, nc.sort,by.x='ID_Source',by.y='ID_sort')$y_i
df$var_s <- df$var*max(df$tot_tot1,na.rm=T)/df$tot_tot1
df[is.na(df)] <- 0


gg1 <- ggplot(df,aes(avg,1-var))


# TOTAL NUMBER OF PARTICLES CONNECTING PER A SOURCE
Tot_Part2 <-  Tot_Part(DataBase,'ID_Sink','year')

# STRENGTH Variability OF LINK OCCURRENCE 
Diff_Part2 <-  Diff_Part(DataBase,'ID_Sink','year')

Var.part <- base::merge(Diff_Part2,Tot_Part2,by='ID_Source',all.y=T)

a <- seq(from = 0, to = 7, by = 1)
N_l <- sapply(X = a, FUN = function(a) {
     Hill_lambda(DataBase,'ID_Sink',a)$Hill_l
})

N_a <- sapply(X = a, FUN = function(a) {
     Hill_alpha(DataBase,'ID_Sink','year',a)$Hill_a
})

df <- as.data.frame(N_a)
colnames(df)<-a
df$ID_Source <- unique(DataBase$ID_Source)

df<- reshape2::melt(df,id.vars=c(9),variable.name='q',value.name='Hill_b')

opt_plot <- ggplot(subset(df,q!=1),aes(as.numeric(as.character(q)),Hill_b))+
     geom_line(aes(group=ID_Source))+
     geom_point(aes(fill=as.factor(ID_Source)),shape=21,size=2)+
     xlab('q')+
     ylab(expression("Flow rate of particles ("[beta]~H^5~")"))+ theme_classic()+
     theme(legend.position = c(0.85,0.75),panel.background = element_blank(),panel.border = element_rect(colour='black',fill=NA),legend.background  = element_rect(fill=NA))



df <- as.data.frame(N_l/N_a)
colnames(df)<-a
df$ID_Source <- unique(DataBase$ID_Source)

df <- base::merge(Var.part,df, by= 'ID_Source')
df$var.p <- df$diff.p/df$tot.p

df <- reshape2::melt(df,id.vars=c(1,2,3,12),variable.name='q',value.name='Hill_b')
df = subset(df,q==3)
df[is.na(df)] <- 0


df$y_i <-  base::merge(df, nc.sort,by.x='ID_Source',by.y='ID_sort')$y_i


#df$var_s <- df$var*max(df$tot_tot1,na.rm=T)/df$tot_tot1


gg2 <- ggplot(df,aes(var.p,Hill_b))


Frequency_info <- function(Table,Tot_time){
     Table[is.na(Table)] <- 0 # if NA are present in third to n-th column, put 0
     Table[,c(-1,-2)][Table[,c(-1,-2)] > 0] <- 1
     if (ncol(unique(Table[,-c(1,2)])) != Tot_time){ # In case a Time has been simulated but no link has established, make that year accounted in the table
          
          missing_time <- which(1:Tot_time %nin% colnames(Table[,-c(1,2)])) # Find which time is missing in the table
          cat('Missing release time',missing_time,'will be added\n')
          
          for (t_missing in missing_time){
               Table <- add_column(Table, n_month = 0 , .after = t_missing+2-1) # Add a column
               colnames(Table)[t_missing+2] <- paste(t_missing,sep='') # rename the column
          }
     }
     
     Table_info <- Table[,1:2]
     Table_info$Freq <- 0;  Table_info$MaxConsecutive <- 0 ; Table_info$AvgConsecutive <- 0
     for (i in 1:dim(Table)[1]) {
          Length_Occ <- rle(as.vector(Table[i,-c(1,2)],mode='numeric'))
          Info <- Length_Occ$lengths[Length_Occ$values==1]
          Table_info$Freq[i] <- sum(Info)
          Table_info$MaxConsecutive[i] <- max(Info)
          Table_info$AvgConsecutive[i] <- mean(Info)
     }
     return(Table_info)
}


DataBase_sub <- as.data.frame(DataBase)
Table <- reshape2::dcast(DataBase_sub,ID_Source + ID_Sink ~ year ,value.var='Count')
Tot_time = length(unique(DataBase_sub$year)) # total number of years with or without links
Table_info <- Frequency_info(Table,Tot_time)
Table_res <- aggregate(cbind(MaxConsecutive,AvgConsecutive,Freq)~ID_Source, Table_info, function(x) mean(x)/Tot_time)
#Table_res$ID_Sort <-  base::merge(Table_res, nc.sort,by.x='ID_Source',by.y='ID')$ID_sort
Table_res$y_i <-  base::merge(Table_res, nc.sort,by.x='ID_Source',by.y='ID_sort')$y_i


gg3 <- ggplot(Table_res,aes(Freq,MaxConsecutive))


### ----- Indicators De novo ----

Var_Link2 <- Var_Link(as.data.frame(DataBase_new),'ID_Sink','Year')


# TOTAL NUMBER OF PARTICLES CONNECTING PER A SOURCE
N_scale_Link2 <-  N_scale_Link(as.data.frame(DataBase_new),'ID_Sink','Year')
df <- base::merge(Var_Link2, N_scale_Link2, by='ID_Source',all.y=T)
df$var_s <- df$var*max(df$tot_tot1,na.rm=T)/df$tot_tot1
df[is.na(df)] <- 0
df <-  base::merge(df,coord, by.x= 'ID_Source', by.y ='ID_ri')
df$MPAin <- 'No'
df$MPAin[df$ID_Source %in% unique(c(subset(results,!is.na(ID_MPA_i))$ID_ri, subset(results,!is.na(ID_MPA_f))$ID_rf))] <- 'MPA'
df$MPAin[df$ID_Source %in% c(1176, 1275, 1278, 1473,1474)] = 'common'
# common site with expanded



ggEPT1 <- ggplot(subset(df,ID_Source != -1),aes(avg,1-var))

# STRENGTH Variability OF LINK OCCURRENCE 
Tot_Part2 <-  Tot_Part(DataBase_new,'ID_Sink','Year')

Diff_Part2 <-  Diff_Part(DataBase_new,'ID_Sink','Year')

Var.part <- base::merge(Diff_Part2,Tot_Part2,by='ID_Source',all.y=T)

a <- 3
N_l <- sapply(X = a, FUN = function(a) {
     Hill_lambda(DataBase_new,'ID_Sink',a)$Hill_l
})

N_a <- sapply(X = a, FUN = function(a) {
     Hill_alpha(DataBase_new,'ID_Sink','Year',a)$Hill_a
})

df <- as.data.frame(N_l/N_a)
colnames(df)<-a
df$ID_Source <- unique(DataBase_new$ID_Source)

df <- base::merge(Var.part,df, by= 'ID_Source')
df$var.p <- df$diff.p/df$tot.p

names(df)[4]<- 'Hill_b'
df[is.na(df)] <- 0
df <-  base::merge(df,coord, by.x= 'ID_Source', by.y ='ID_ri')
df$MPAin <- 'No'
df$MPAin[df$ID_Source %in% unique(c(subset(results,!is.na(ID_MPA_i))$ID_ri, subset(results,!is.na(ID_MPA_f))$ID_rf))] <- 'MPA'
df$MPAin[df$ID_Source %in% c(1176, 1275, 1278, 1473,1474)] = 'common'


ggEPT2 <- ggplot(df,aes(var.p,Hill_b))


Frequency_info <- function(Table,Tot_time){
     Table[is.na(Table)] <- 0 # if NA are present in third to n-th column, put 0
     Table[,c(-1,-2)][Table[,c(-1,-2)] > 0] <- 1
     if (ncol(unique(Table[,-c(1,2)])) != Tot_time){ # In case a Time has been simulated but no link has established, make that year accounted in the table
          
          missing_time <- which(1:Tot_time %nin% colnames(Table[,-c(1,2)])) # Find which time is missing in the table
          cat('Missing release time',missing_time,'will be added\n')
          
          for (t_missing in missing_time){
               Table <- add_column(Table, n_month = 0 , .after = t_missing+2-1) # Add a column
               colnames(Table)[t_missing+2] <- paste(t_missing,sep='') # rename the column
          }
     }
     
     Table_info <- Table[,1:2]
     Table_info$Freq <- 0;  Table_info$MaxConsecutive <- 0 ; Table_info$AvgConsecutive <- 0
     for (i in 1:dim(Table)[1]) {
          Length_Occ <- rle(as.vector(Table[i,-c(1,2)],mode='numeric'))
          Info <- Length_Occ$lengths[Length_Occ$values==1]
          Table_info$Freq[i] <- sum(Info)
          Table_info$MaxConsecutive[i] <- max(Info)
          Table_info$AvgConsecutive[i] <- mean(Info)
     }
     return(Table_info)
}


DataBase_sub <- as.data.frame(DataBase_new)

Table <- reshape2::dcast(DataBase_sub,ID_Source + ID_Sink ~ Year ,value.var='Count')
Tot_time = length(unique(DataBase_sub$Year)) # total number of years with or without links
Table_info <- Frequency_info(Table,Tot_time)
Table_res <- aggregate(cbind(MaxConsecutive,AvgConsecutive,Freq)~ID_Source, Table_info, function(x) mean(x)/Tot_time)
Table_res <-  base::merge(Table_res,coord, by.x= 'ID_Source', by.y ='ID_ri')


Table_res$MPAin <- 'No'
Table_res$MPAin[Table_res$ID_Source %in% unique(c(subset(results,!is.na(ID_MPA_i))$ID_ri, subset(results,!is.na(ID_MPA_f))$ID_rf))] <- 'MPA'
Table_res$MPAin[Table_res$ID_Source %in% c(1176, 1275, 1278, 1473,1474)] = 'common'

ggEPT3 <- ggplot(Table_res,aes(Freq,MaxConsecutive))


### ----- Indicators Extension ----

DataBase_expanded <- rbind(DataBase_expanded %>% filter(ID_Source %nin% c(7,11,14,17,24,25)),
                           DataBase %>% filter(ID_Source %in% c(7,11,14,17,24,25)) %>% rename(Year=year))

Var_Link2 <- Var_Link(as.data.frame(DataBase_expanded),'ID_Sink','Year')


# TOTAL NUMBER OF PARTICLES CONNECTING PER A SOURCE
N_scale_Link2 <-  N_scale_Link(as.data.frame(DataBase_expanded),'ID_Sink','Year')
df <- base::merge(Var_Link2, N_scale_Link2, by='ID_Source',all.y=T)
df$var_s <- df$var*max(df$tot_tot1,na.rm=T)/df$tot_tot1
df[is.na(df)] <- 0


df$MPAin <- 'No'
df$MPAin[df$ID_Source < 100] <- 'MPA'
df$MPAin[df$ID_Source %in% c(1176, 1275, 1278, 1473,1474)] = 'common'

df <-  left_join(df,coord%>% rename(ID_Source=ID_ri), by = 'ID_Source')
nc.sort$ID_sort <- as.numeric(nc.sort$ID_sort)
df <-  left_join(df,nc.sort %>% rename(y=y_i,ID_Source=ID_sort) %>% select(-ID), by = 'ID_Source')
df$y_i[is.na(df$y_i)] <- df$y[is.na(df$y_i)]
df$label <- df$ID_Source
df$label[df$MPAin!='MPA'] <- ''

gg1EPT <- ggplot(subset(df,ID_Source != -1),aes(avg,1-var))

# STRENGTH Variability OF LINK OCCURRENCE 
Tot_Part2 <-  Tot_Part(DataBase_expanded,'ID_Sink','Year')

Diff_Part2 <-  Diff_Part(DataBase_expanded,'ID_Sink','Year')

Var.part <- base::merge(Diff_Part2,Tot_Part2,by='ID_Source',all.y=T)

a <- 3
N_l <- sapply(X = a, FUN = function(a) {
     Hill_lambda(DataBase_expanded,'ID_Sink',a)$Hill_l
})

N_a <- sapply(X = a, FUN = function(a) {
     Hill_alpha(DataBase_expanded,'ID_Sink','Year',a)$Hill_a
})

df <- as.data.frame(N_l/N_a)
colnames(df)<-a
df$ID_Source <- unique(DataBase_expanded$ID_Source)

df <- base::merge(Var.part,df, by= 'ID_Source')
df$var.p <- df$diff.p/df$tot.p

names(df)[4]<- 'Hill_b'
df[is.na(df)] <- 0

df$MPAin <- 'No'
df$MPAin[df$ID_Source < 100] <- 'MPA'
df$MPAin[df$ID_Source %in% c(1176, 1275, 1278, 1473,1474)] = 'common'

df$label <- df$ID_Source
df$label[df$MPAin!='MPA'] <- ''

df <-  left_join(df,coord%>% rename(ID_Source=ID_ri), by = 'ID_Source')
nc.sort$ID_sort <- as.numeric(nc.sort$ID_sort)
df <-  left_join(df,nc.sort %>% rename(y=y_i,ID_Source=ID_sort) %>% select(-ID), by = 'ID_Source')
df$y_i[is.na(df$y_i)] <- df$y[is.na(df$y_i)]


gg2EPT <- ggplot(df,aes(var.p,Hill_b))


Frequency_info <- function(Table,Tot_time){
     Table[is.na(Table)] <- 0 # if NA are present in third to n-th column, put 0
     Table[,c(-1,-2)][Table[,c(-1,-2)] > 0] <- 1
     if (ncol(unique(Table[,-c(1,2)])) != Tot_time){ # In case a Time has been simulated but no link has established, make that year accounted in the table
          
          missing_time <- which(1:Tot_time %nin% colnames(Table[,-c(1,2)])) # Find which time is missing in the table
          cat('Missing release time',missing_time,'will be added\n')
          
          for (t_missing in missing_time){
               Table <- add_column(Table, n_month = 0 , .after = t_missing+2-1) # Add a column
               colnames(Table)[t_missing+2] <- paste(t_missing,sep='') # rename the column
          }
     }
     
     Table_info <- Table[,1:2]
     Table_info$Freq <- 0;  Table_info$MaxConsecutive <- 0 ; Table_info$AvgConsecutive <- 0
     for (i in 1:dim(Table)[1]) {
          Length_Occ <- rle(as.vector(Table[i,-c(1,2)],mode='numeric'))
          Info <- Length_Occ$lengths[Length_Occ$values==1]
          Table_info$Freq[i] <- sum(Info)
          Table_info$MaxConsecutive[i] <- max(Info)
          Table_info$AvgConsecutive[i] <- mean(Info)
     }
     return(Table_info)
}


DataBase_sub <- as.data.frame(DataBase_expanded)

Table <- reshape2::dcast(DataBase_sub,ID_Source + ID_Sink ~ Year ,value.var='Count')
Tot_time = length(unique(DataBase_sub$Year)) # total number of years with or without links
Table_info <- Frequency_info(Table,Tot_time)
Table_res <- aggregate(cbind(MaxConsecutive,AvgConsecutive,Freq)~ID_Source, Table_info, function(x) mean(x)/Tot_time)

Table_res$MPAin <- 'No'
Table_res$MPAin[Table_res$ID_Source < 100] <- 'MPA'
Table_res$MPAin[Table_res$ID_Source %in% c(1176, 1275, 1278, 1473,1474)] = 'common'
Table_res$label <- Table_res$ID_Source
Table_res$label[Table_res$MPAin!='MPA'] <- ''

Table_res <-  left_join(Table_res,coord %>% rename(ID_Source=ID_ri), by = 'ID_Source')
nc.sort$ID_sort <- as.numeric(nc.sort$ID_sort)
Table_res <-  left_join(Table_res,nc.sort %>% rename(y=y_i,ID_Source=ID_sort) %>% select(-ID), by = 'ID_Source')
Table_res$y_i[is.na(Table_res$y_i)] <- Table_res$y[is.na(Table_res$y_i)]


gg3EPT <- ggplot(Table_res,aes(Freq,MaxConsecutive))



sort(unique(results$ID_MPA_f))

### ----- Plot Indicators  ----


GG1 <- gg1 +   geom_point(aes(fill=y_i),size=2.5,shape=24) +
     #labs(y = expression("Proportion of repeated links (P"["X>1"]~")"),
     #    x= expression("Average of links occurrence (n"["link"]~")"))+
     scale_fill_continuous(name='Latitude ºN of grid cells')+
     theme_classic()+ 
     geom_hline(yintercept=1,linewidth=0.3,linetype=2)+
     coord_cartesian(xlim=c(0,25),ylim=c(0,1.05))+
     ggrepel::geom_text_repel(aes(label=ID_Source),size=3,max.overlaps = 25,force=3,min.segment.length = 0.3)+
     theme(legend.position = 'None',#c(0.85,0.3),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank()) 

GG2 <- gg2+ geom_point(shape=24,size=2.5,aes(fill=y_i))+ 
     #labs(x =  expression(atop("Proportion of particle in non-repeated links",paste("(P"["N|X=1"]*")"))),
     #          y= expression("Flow rate of particles ("[beta]~H^5~")"))+ 
     theme_classic()+
     coord_cartesian(xlim=c(0,1),ylim=c(0,5.1))+
     scale_x_continuous(breaks=c(0,0.5,1))+
     geom_vline(xintercept=0,linewidth=0.3,linetype=2)+
     ggrepel::geom_text_repel(aes(label=ID_Source),size=3,max.overlaps = 25,force=3,min.segment.length = 0.3)+
     theme(legend.position = 'None',#c(0.75,0.85),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.direction = 'horizontal', #legend.box to make side by side
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank(),axis.text.y = element_text(margin = margin(l = 16)))


GG3 <- gg3+    geom_point(aes(fill=y_i),size=2.5,shape=24) +   
     ggrepel::geom_text_repel(aes(label=ID_Source),size=3,max.overlaps = 25,force=3,min.segment.length = 0.3)+
     coord_cartesian(xlim=c(0.05,.67),ylim=c(0.05,.6))+
     scale_x_continuous(breaks=c(0.1,0.3,0.5))+
     theme_classic()+
     theme(legend.position = 'None',#c(0.75,0.85),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.direction = 'horizontal', #legend.box to make side by side
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank(),
           axis.text.y = element_text(margin = margin(l = 7)))




GGEPT1 <- ggEPT1 +
     geom_point(aes(fill=y_i,shape=MPAin),size=2.5)+    scale_fill_continuous(name='Latitude ºN of grid cells')+
     scale_shape_manual(name='Grid cell in MPA', values=c(21,24,23))+
     coord_cartesian(xlim=c(0,25),ylim=c(0,1.05))+
     theme_classic()+ # geom_vline(xintercept=0,linewidth=0.3,linetype=2)+
     geom_hline(yintercept=1,linewidth=0.3,linetype=2)+
     theme(legend.position = 'None',#c(0.85,0.3),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12),axis.text.y=element_blank(), axis.title = element_blank())  

GGEPT2 <- ggEPT2 +
     geom_point(size=2.5,aes(fill=y_i,shape=MPAin))+ 
     scale_fill_continuous(name='Latitude ºN of \ngrid cells')+
     scale_shape_manual(name='Grid cell in MPA', values=c(21,24,23))+
     theme_classic()+
     scale_x_continuous(breaks=c(0,0.5,1))+
     coord_cartesian(xlim=c(0,1),ylim=c(0,5.1))+
     geom_vline(xintercept=0,linewidth=0.3,linetype=2)+
     theme(legend.position = 'None',#c(0.75,0.85),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.direction = 'horizontal', #legend.box to make side by side
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank(),
           axis.text.y= element_blank())


GGEPT3 <- ggEPT3 + geom_point(size=2.3,aes(fill=y_i,shape=MPAin) )      +
     scale_fill_continuous(name='Latitude ºN of grid cells')+
     scale_shape_manual(name='Grid cell in MPA', values=c(21,24,23))+
     coord_cartesian(xlim=c(0.05,.67),ylim=c(0.05,.6))+
     scale_x_continuous(breaks=c(0.1,0.3,0.5))+
     theme_classic()+
     theme(legend.position = 'None',#c(0.75,0.85),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.direction = 'horizontal', #legend.box to make side by side
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank(),
           axis.text.y = element_blank())


GG1EPT <- gg1EPT +
     geom_point(aes(fill=y_i,shape=MPAin),size=2.5)+ #labs(y = expression("Proportion of repeated links (P"["X>1"]~")"),
     #x= expression("Average of links occurrence (n"["link"]~")"))+
     scale_shape_manual(name='Grid cell in MPA', values=c(21,24,23))+
     scale_fill_continuous(name='Latitude ºN of grid cells')+
     ggrepel::geom_text_repel(aes(label=label),size=3,max.overlaps = 25,force=3,min.segment.length = 0.3)+
     theme_classic()+ # geom_vline(xintercept=0,linewidth=0.3,linetype=2)+
     geom_hline(yintercept=1,linewidth=0.3,linetype=2)+
     coord_cartesian(xlim=c(0,25),ylim=c(0,1.05))+
     theme(legend.position = 'None',#c(0.85,0.3),
           legend.title=element_text(size=18),
           legend.text = element_text(size=18),
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12),axis.text.y=element_blank(), axis.title = element_blank()) 

GG2EPT <- gg2EPT +
     geom_point(aes(fill=y_i,shape=MPAin),size=2.5)+ 
     scale_fill_continuous(name='Latitude ºN of \ngrid cells')+
     scale_shape_manual(name='Grid cell in MPA', values=c(21,24,23))+
     theme_classic()+
     ggrepel::geom_text_repel(aes(label=label),size=3,max.overlaps = 25,force=3,min.segment.length = 0.3)+
     geom_vline(xintercept=0,linewidth=0.3,linetype=2)+
     coord_cartesian(xlim=c(0,1),ylim=c(0,5.1))+
     scale_x_continuous(breaks=c(0,0.5,1))+
     theme(legend.position = 'None',#c(0.75,0.85),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.direction = 'horizontal', #legend.box to make side by side
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank(),
           axis.text.y = element_blank())

GG3EPT <- gg3EPT +
     geom_point(aes(fill=y_i,shape=MPAin),size=2.5)+ 
     scale_fill_continuous(name='Latitude ºN of grid cells')+
     scale_shape_manual(name='Grid cell in MPA', values=c(21,24,23))+
     coord_cartesian(xlim=c(0.05,.67),ylim=c(0.05,.6))+
     theme_classic()+
     ggrepel::geom_text_repel(aes(label=label),size=3,max.overlaps = 25,force=3,min.segment.length = 0.3)+
     scale_x_continuous(breaks=c(0.1,0.3,0.5))+
     theme(legend.position = 'None',#c(0.75,0.85),
           legend.title=element_text(size=20),
           legend.text = element_text(size=18),
           legend.direction = 'horizontal', #legend.box to make side by side
           legend.frame = element_rect(color='black',size=0.2),
           legend.background  = element_rect(fill=NA), panel.background = element_blank(),panel.border  =element_rect(colour='black',fill=NA),
           axis.text = element_text(size=12), axis.title = element_blank(),
           axis.text.y = element_blank())




## -- Figure 6
val_height=0.31
ggdraw() +
     draw_plot(GG1, x = 0.05, y = 0.69, width = .33, height =   val_height) +
     draw_plot(GG2, x = 0.05, y = .36, width = .33, height =   val_height) +
     draw_plot(GG3, x = 0.05, y = 0.03, width = .33, height =   val_height) +
     draw_plot(GG1EPT, x = 0.33+0.05, y = .69, width = .33-0.05, height =   val_height) +
     draw_plot(GG2EPT, x = 0.33+0.05, y = .36, width = .33-0.05, height =   val_height) +
     draw_plot(GG3EPT, x = 0.33+0.05, y = .03, width = .33-0.05, height =   val_height)+
     draw_plot(GGEPT1, x = 0.66, y = .69, width = .33-0.05, height =   val_height) +
     draw_plot(GGEPT2, x = 0.66, y = .36, width = .33-0.05, height =   val_height) +
     draw_plot(GGEPT3, x = 0.66, y = .03, width = .33-0.05, height =   val_height)+
     draw_label(label ="Average of links occurrence ", 
                size = 11, x = 0.5, y = .689)+
     draw_label(label ="Proportion of particle in non-repeated links", 
                size = 11, x = 0.5, y = .359)+
     draw_label(label ="Linking frequency", 
                size = 11, x = 0.5, y = 0.029)+
     draw_label(label ="Proportion of \nrepeated links  ", 
                size = 11, x = 0.03, y = .85,angle=90) +
     draw_label(label ="Flow rate \nof particles", 
                size = 11, x = 0.03, y = .54, angle=90)+
     draw_label(label ="Freq. of max. \nuninterrupted \nlink duration", 
                size = 11, x = 0.04, y = 0.2, angle=90)

###-------------------###
