# This file apply_lumpR.R is part of the experiment scripts for the
# paper Pilz et al. (2018), HESS
# Copyright (C) 2018 Tobias Pilz
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.





# This is scripts was used to initialise the WASA-SED model employing the lumpR R-package.



### INSTALLATION ###

# you will need devtools to install lumpR from github!
if("lumpR" %in% rownames(installed.packages())) {
  library(lumpR)
} else if("devtools" %in% rownames(installed.packages())) {
  library(devtools)
  install_github("tpilz/lumpR")
  library(lumpR)
} else {
  install.packages("devtools")
  library(devtools)
  install_github("tpilz/lumpR")
  library(lumpR)
}

# meteo time series interpolation
library(geostat)



### SETTINGS ###

# switch to specified working directory
setwd("/home/tobias/Promotion/Modellierung/Jaguaribe_new/lumpR/results_new/")


# DATABASE #
# you will need to set up an ODBC database
# for Windows consider https://github.com/tpilz/lumpR/wiki/04-Databases-and-ODBC
dbname <- "wasa_jaguaribe" # DSN registered at ODBC

# LINUX ONLY:
# register database (write into .odbc.ini in your $HOME)
# str_odbc <- c(paste0("[", dbname, "]"), # adjust as you like
#               "Description = Jaguaribe WASA parameter database",
#               "Driver = MySQL", # INSTALL AND FILL IN HERE YOUR ODBC DRIVER!
#               "ServerName = localhost", # as needed
#               "Database = WASA_jaguaribe", # adjust as you like
#               "UserName = tobias",
#               "Password = tobias",
#               "")
# write(str_odbc, file="~/.odbc.ini", ncolumns=1, append=T, sep="\n")
# 


# INPUT #
# inputs marked MANDATORY have to be given, the rest can be 'NULL' if not available

# watershed outlet (coordinates in projection of GRASS location!) 
# drain_p <- data.frame(utm_x_m=630345, utm_y_m=9483900)
# # specifiy columns containing coordinates
# coordinates(drain_p) <- c("utm_x_m", "utm_y_m")
# revised drain points; available after first try of calc_subbas(); (un)comment lines accordingly
drain_p_rev_file <- "drain_p_all_revised"

# GRASS vector file with river gages
riv_gages <- "riv_gages_sel"
# gage codes to be used
gage_codes_sel <- c("36760000", # Quixere, shortly upstream of Banabuiu mouth
                    "36580000", # Morada Nova II, Banabuiu shortly before streaming into Jaguaribe
                    "36470000", # Senador Pompeu, Rio Banabuiu, upstream of reservoir
                    "36520000", # Quixeramobim, Rio Quixeramobim, downstream of reservoir before flowing into Banabuiu reservoir
                    "36386000", # Alto Santo, small tributary downstream of Castanhao
                    "36320000", # Jaguaribe, upstream of Castanhao
                    "36290000", # Ico, Rio Salgado before flowing into Jaguaribe
                    "36160000", # Iguatu, upstream of Oros
                    "36130000", # Carius, Rio Carius before flowing into upper Jaguaribe
                    "36125000", # Sitio Poco Santo, Rio dos Bastioe, tributary to Rio Carius
                    "36070000", # Sitio Patos, upper Jaguaribe before Rio Carius
                    "36045000", # Malhada, Riacho Conceica, before flowing into upper Jaguaribe
                    "36020000" # Arneiroz, upper Jaguaribe (barrage)
                    )

# DEM raster in GRASS location
dem <- "dem"

# land / vegetation cover raster map in GRASS location - MANDATORY
lcov <- "lcov"

# soil raster map in GRASS location - MANDATORY
soil <- "soil"

# soil depth raster map
soil_depth <- "soil_depth"

# water mask raster map in GRASS location (1=water, 0=no water)
watermask <- "watermask"

# impervious surface areas raster map in GRASS location (1=impervious, 0=permeable)
imperviousmask <- "urbanmask"

# river vector map
river <- NULL

# meteo time series
# grid-based (Xavier)
# needed variables for WASA: precipitation, temperature, relative humidity, short-wave incoming radiation - MANDATORY
meteo_ts <- c(prec = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/ts_prec.dat",
              relhum = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/ts_RH.dat",
              temper = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/ts_Tmean.dat",
              glorad = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/ts_glorad.dat")

# meteo data location for interpolation - MANDATORY
# named vector of location files for each variable, see ?externalInputLocationsTable 'files_sources'
meteo_locs <- c(prec = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/locs.dat",
                relhum = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/locs.dat",
                temper = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/locs.dat",
                glorad = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/meteo/gridded/prep/locs.dat")

# file of extraterrestrial radiation for the whole watershed area (already in WASA format)
radex_file <- "/home/tobias/Promotion/Modellierung/LUMP-Paper/preprocessing/climate/radex/extraterrestrial_radiation.dat"


# path to prepared vegetation parameter table 'vegetation.dat' in WASA format - MANDATORY
veg_path <- "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/landcover/param/"

# path to prepared soil parameter tables 'horizons.dat, 'particle_classes.dat', 'r_soil_contains_particles.dat', and 'soil.dat' in WASA format - MANDATORY
soil_path <- "/home/tobias/Promotion/Modellierung/Jaguaribe_new/data/soil/param/out/"

# Reservoir data (vector files in GRASS with parameters needed by WASA)
# Small reservoirs; should be point instead of polygon feature (i.e. reservoir outlet locations)
# Needs at least either column 'volume' with information on volume in [m^3] or column 'area' with information on lake area in [m^2] in the attribute table.
res_lumped_vect <- "res_lumped"
# Strategic reservoirs; should be point instead of polygon feature (i.e. reservoir outlet locations); columns see ?reservoir_strategic
res_strat_vect <- "res_strategic"



# OUTPUT #
# outputs marked MANDATORY have to be given, the rest can be 'NULL'
# some outputs are inputs for other functions (e.g. flow accumulation)

# subbasin raster map - MANDATORY
subbas <- "subbas"

# prefix of calculated stream segments raster and vector maps
stream_pref <- "stream_accum"

# prefix of drainage point vector files (given points snapped to river and internally calculated points, respectively) - MANDATORY
drainp_processed <- "drain_points"

# elementary hillslope areas raster map - MANDATORY
eha <- "eha"

# flow direction raster map - MANDATORY
flowdir <- "flowdir"

# flow accumulation raster map - MANDATORY
flowacc <- "flowacc"

# stream segments raster map (based on eha calculation; much finer than 'stream_pref' to delineate hillslopes) - MANDATORY
stream <- "stream"

# Horton stream order raster map (based on 'stream' above) - MANDATORY
stream_horton <- "stream_horton"

# elevation relative to next river segment raster map - MANDATORY
elevriv <- "elevriv"

# distance to next river segment raster map - MANDATORY
distriv <- "distriv"

# soil vegetation components raster map - MANDATORY
svc <- "svc"

# landscape units raster map - MANDATORY
lu <- "lu"

# name of file containing svc parameters - MANDATORY
svc_file <- "soil_vegetation_components.dat"

# Name of output file containing mean catena information as input for prof_class - MANDATORY
catena_out <- "rstats.txt"

# Name of output header file containing meta-information as input for prof_class - MANDATORY
catena_head_out <- "rstats_head.txt"

# Name of subbasin statistics file containing subbasin parameters
sub_ofile <- "sub_stats.txt"

# Name of file containing subbasins and the corresponding LUs with their fraction of area in the subbasin
lu_ofile <- "lu_stats.txt"

# Name of file containing LUs and related parameters
lupar_ofile <- "lu_pars.txt"

# Name for the vector reservoir map created in GRASS location containing information on reservoir size classes in the attribute table
# If NULL it will not be created
res_vect_class <- "res_vect_class"
# vector file of outlet locations of strategic reservoirs
res_strat_outlets <- "res_strat_out"
# vector file of outlet locations of lumped reservoirs
res_lumped_outlets <- "res_lumped_out"



# PARAMETERS #
# STRONGLY CASE-STUDY SPECIFIC! Try out what suits your needs / data

# MISCELLANEOUS PARAMETERS #
# parameters influecing some outputs but not directly discretisation complexity

# Raster cells in accumulation map with values greater than thresh_stream are considered as streams. Needs to be set only if river is not set - MANDATORY
thresh_stream <- 2000

# maximum distance for snapping of drain_points to stream segments in units of your GRASS location - MANDATORY
snap_dist <- 500

# shall small spurious subbasins smaller than rm_spurious * thresh_sub, created within calc_subbas(), be deleted?
rm_spurious <- 0.01

# minimum size of EHAs (in hectares) not to be removed, smaller EHAs (artefacts) are removed; parameter for GRASS function r.reclass.area - MANDATORY
sizefilter <- 30 

# growing radius (in raster cells) to remove artefacts in EHA data; parameter for GRASS function r.grow - MANDATORY
growrad <- 50

# minimum number of cells a hillslope area must have, all smaller ones are skipped
min_cell_in_slope <- 10

# minimum number of sampling points (cells) a catena should have. If there are less, the catena is not saved
min_catena_length <- 3

# maximum distance to river [in cells]: if the closest cell of an EHA is farther than max_riv_dist, the EHA is skipped, otherwise all distances within the EHA are redurced by the distance of the closest cell to river
max_riv_dist <- 15


# RUN TIME PARAMETERS #

# shall temporary files be kept in the GRASS location, e.g. for debugging or further analyses?
keep_temp <- F

# Shall output of previous calls of this function be deleted? If FALSE the function returns an error if output already exists
overwrite <- T

# Shall the function be silent (also suppressing warnings of internally used GRASS functions, etc.)?
silent <- F

# produce plots (scatter, mean catena, etc.) for each area / class (written into sub-directory plots_area2catena)
plot_catena <- T

# produce plots of classification of catenas to landscape units and terrain components
plot_profclass <- T

# produce GRASS reclassification files for qualitative raster data
grass_files <- F

# number of cores that should be used for parallel computation (where possible)
ncores <- 1

# RESERVOIRS #
# lumped reservoir parameters, see ?reservoir_lumped
res_lumped_pars <- data.frame(class=1:5,
                              f_vol_init=c(0.05,0.05,0.1,0.1,0.2), # rough subjective estimation for 1 Jan (begin of rainy season)
                              class_change=0,
                              alpha_Molle=2.7,
                              damk_Molle=1500,
                              damc_hrr=c(12.5,18.75,25,37.5,50),
                              damd_hrr=1.5)

# LANDSCAPE DISCRETISATION PARAMETERS #

# Parameter for GRASS function r.watershed defining the minimum size of an exterior watershed basin in number of grid cells. If NULL only the given drainage points are used for subbasin delineation
thresh_sub <- 80000
  
# parameter for GRASS function r.watershed. This is a crucial parameter affecting the size of delineated hillslopes - MANDATORY
eha_thres <- 1200
  
# vector with GRASS file names of quantitative supplemental information for LU deviation (adjust no_LUs accordingly)
supp_quant <- NULL

# vector with GRASS file names of qualitative supplemental information for LU deviation (adjust no_LUs accordingly)
supp_qual <- c("soil", "lcov", "svc") # svc has to be defined to generate SVC parameters needed for WASA! Map is generated by lump_grass_prep()

# number of LUs to be produced per attribute in the order:
# c( <shape>, <extent>, <weighting vertical vs. horizontal extent>, <quant. suppl. information>, <qual. suppl. information>, <slope width> )
no_LUs <- c(3, 3, 10, 2, 2, 2, 3)

# number of TCs that are created by algorithm (numTC)
no_TCs <- 3


# GRASS #
# ATTENTION: GRASS 6.4 is needed, not compatible to GRASS 7!
# Best is to use GRASS 6.4.6 as GRASS 6.4.5 by autumn 2016 suddenly started producing segmentation faults (on Linux)!
addon_path="/home/tobias/.grass6/addons/" # path to your locally installed GRASS add-ons. Must only be given if necessary, see ?lump_grass_prep
# initialisation of session
initGRASS(gisBase="/usr/local/grass-6.4.6/", # path to GRASS installation (use / instead of \ under windows, e.g. "d:/programme/GRASS6.4.3" )
          home=getwd(), # The directory in which to create the .gisrc file
          location="Jaguaribe", # GRASS location
          mapset="lumpr_new",    # corresp. mapset
          gisDbase="/home/tobias/Promotion/Modellierung/Jaguaribe/grassdata/",  # path to 'grassdata' directory containing the location specified above and all corresp. data
          override=TRUE)
  
  

  
  
### CALCULATIONS ####
# no adjustments necessary
# run line-by-line to understand what is going on!


      
# SUBBASIN DELINEATION #
# # get outlet points of strategic reservoirs as input for calc_subbas(); manual investigation and corrections necessary!
# outlet_points <- reservoir_outlet(dem=dem,
#                  res_vct = res_strat_vect,
#                  outlets_vect = res_strat_outlets,
#                  keep_temp = keep_temp,
#                  overwrite = TRUE)
# outlet_points <- readVECT6(res_strat_outlets)
# outlet_sp <- SpatialPoints(coordinates(outlet_points), proj4string = CRS(getLocationProj()))
# 
# # get discharge location from GRASS
# gage_points <- readVECT6(riv_gages)
# gage_points <- gage_points[which(gage_points@data$id %in% gage_codes_sel & gage_points@data$resol=="day"),]
# gage_sp <- SpatialPoints(coordinates(gage_points), proj4string = CRS(getLocationProj()))
# 
# # define projection of drainage point(s) (use projection of GRASS location)
# drain_sp <- SpatialPoints(coordinates(drain_p), CRS(getLocationProj()))
# 
# # merge with drainage points derived from reservoir outlet and gage points
# drain_p_all <- rbind(drain_sp, gage_sp, outlet_sp)
# drain_p_all_out <- SpatialPointsDataFrame(drain_p_all, data.frame(cat=1:length(drain_p_all)))
# writeVECT6(drain_p_all_out, "drain_p_all_corr", v.in.ogr_flags = "overwrite")

# revised drain points "drain_p_all_corr"; available after first try of calc_subbas(); (un)comment lines accordingly
drain_p_all <- readVECT6(drain_p_rev_file)
drain_p_all <- SpatialPoints(coordinates(drain_p_all), proj4string = CRS(getLocationProj()))

# calculate subbasins; one subbasin for each drainage point
?calc_subbas # read the documentation!
calc_subbas(
  # INPUT #
  dem=dem,
  drain_points=drain_p_all,
  river=river,
  # OUTPUT #
  basin_out=subbas,
  stream=stream_pref,
  points_processed=drainp_processed,
  # PARAMETERS #
  outlet=1,
  thresh_stream=2000,
  thresh_sub=80000,
  snap_dist=snap_dist,
  rm_spurious=0.01,
  keep_temp=F,
  overwrite=overwrite,
  silent=silent
)
      
      
      
# PREPROCESSING AND HILLSLOPE DEVIATION #
?lump_grass_prep # read the documentation!
lump_grass_prep(
  # INPUT #
  mask = subbas,
  dem = dem,
  lcov = lcov,
  soil = soil,
  watermask = watermask,
  imperviousmask = imperviousmask,
  # OUTPUT #
  eha=eha,
  flowdir = flowdir,
  flowacc = flowacc,
  stream = stream,
  stream_horton = stream_horton,
  elevriv = elevriv,
  distriv = distriv,
  mask_corr = "MASK_corr",
  svc = svc,
  dir_out = getwd(),
  svc_ofile = svc_file,
  # PARAMETERS #
  eha_thres = eha_thres,
  sizefilter = sizefilter,
  growrad = growrad,
  keep_temp=keep_temp,
  overwrite=overwrite,
  silent=silent,
  addon_path = addon_path
)
      
      
      
# CALCULATE MEAN CATENA FOR HILLSLOPES #
# Part of algorithm described by Francke et al. (2008)
?area2catena # read the documentation!
area2catena(
  # INPUT #
  mask="MASK_corr",
  flowacc=flowacc,
  eha=eha,
  distriv=distriv,
  elevriv=elevriv,
  supp_quant=supp_quant,
  supp_qual=supp_qual,
  # OUTPUT #
  dir_out=getwd(),
  catena_out=catena_out,
  catena_head_out=catena_head_out,
  # PARAMETERS #
  ridge_thresh=1,
  min_cell_in_slope=min_cell_in_slope,
  min_catena_length=min_catena_length,
  max_riv_dist=max_riv_dist,
  plot_catena=plot_catena,
  grass_files=grass_files,
  ncores=3,
  eha_subset=NULL,
  overwrite=overwrite,
  silent=silent
)

# change header file according to rstats_header
header_dat <- readLines(paste(getwd(), catena_head_out, sep="/"))
no_LUs[1] <- no_LUs[1] * -1
header_dat[8] <- paste(no_LUs, "\t", sep="", collapse="")
header_dat[9] <- paste(c(no_TCs, rep(0, length(no_LUs)-1)), "\t", sep="", collapse="")
writeLines(header_dat,paste(getwd(), catena_head_out, sep="/"))



# CATENA CLASSIFICATION INTO LANDSCAPE UNITS AND TERRAIN COMPONENTS #
# Part of algorithm described by Francke et al. (2008)
# get resolution (mean between x and y resolution)
res <- execGRASS("r.info", map=dem, flags=c("s"), intern=TRUE)
res <- sum(as.numeric(gsub("[a-z]*=", "", res))) / 2

?prof_class # read the documentation!
prof_class(
  # INPUT #
  catena_file=catena_out,
  catena_head_file=catena_head_out,
  svc_column="svc",
  # OUTPUT #
  dir_out=getwd(),
  luoutfile="lu.dat",
  tcoutfile="tc.dat",
  lucontainstcoutfile="lucontainstc.dat",
  tccontainssvcoutfile="tc_contains_svc.dat",
  terraincomponentsoutfile="terraincomponents.dat",
  recl_lu="reclass_lu.txt",
  saved_clusters=NULL,
  # PARAMETERS #
  seed=1312,
  resolution=res,
  classify_type=' ',
  max_com_length=50,
  com_length=NULL,
  make_plots=plot_profclass,
  eha_subset=NULL,
  overwrite=overwrite,
  silent=silent
)



# POST PROCESSING #
?lump_grass_post # read the documentation!
lump_grass_post(
# INPUT #
mask = "MASK_corr",
dem = dem,
recl_lu = "reclass_lu.txt",
lu = lu,
subbasin = subbas,
eha = eha,
flowacc = flowacc,
flowdir = flowdir,
stream_horton = stream_horton,
soil_depth = NULL,
sdr=NULL,
# OUTPUT #
dir_out = getwd(),
sub_ofile = sub_ofile,
lu_ofile = lu_ofile,
lupar_ofile = lupar_ofile,
# PARAMETER #
fill_holes=T,
groundwater=0,
keep_temp = keep_temp,
overwrite = overwrite,
silent = silent
)



# METEO DATA PREPARATION #
# interpolation of meteo data from prepared raw data (time series and location definition file for every variable)
# output to meteo/ are WASA input files ready to use
dir.create(paste(getwd(),"rainy_season/", sep="/"))
dir.create(paste(getwd(),"meteo/", sep="/"))
# calculate weights for precipitation interpolation to subbasin centroids
externalInputLocationsTable(file_targets= paste(getwd(),sub_ofile,sep="/"), 
                            files_sources= meteo_locs,
                            idField_targets="pid", idField_sources="id", colsep = "\t", nsectors = 10, norigins = 10, power = 2,
                            file_result= paste(getwd(),"meteo/weights.dat",sep="/"), ndigits = 3, overwrite = T)

# read weights data
dat_wgt <- read.table(paste(getwd(),"meteo/weights.dat",sep="/"), header = T, sep = "\t")

# read catchment data
dat_cat <- read.table(paste(getwd(),sub_ofile,sep="/"), header = T)

# loop over variables
for (v in names(meteo_ts)) {
  
  # read ts data
  dat_ts <- read.zoo(meteo_ts[v], header=T, sep="\t")
  
  # remove leading "X" in colnames if present
  colnames(dat_ts) <- gsub("^X", "", colnames(dat_ts))
  
  # get relevant weight data
  var_wgt <- dat_wgt[grep(v, dat_wgt$variable),]
  
  # loop over target stations
  dat_interp <- NULL
  for (s in 1:nrow(dat_cat)) {
    
    # target location
    stat_tar <- dat_cat$pid[s]
    
    # get relevant rows in weight table
    rows_wgt <- which(var_wgt$object == stat_tar)
    
    # get relevant stations from ts data
    dat_t <- dat_ts[,as.character(var_wgt$location[rows_wgt]), drop=F]
    weights_s <- var_wgt$weight[rows_wgt]
    
    # compute weighted mean
    dat_interp_t <- apply(dat_t, 1, function(x,w=weights_s) sum(x*w))
    
    # combine output
    dat_interp <- cbind(dat_interp, dat_interp_t)
    
  } # loop over stations
  
  # xts object for interpolated precipitation
  dat_interp_xts <- xts(dat_interp, index(dat_ts))
  colnames(dat_interp_xts) <- dat_cat$pid
  
  # wasa time series input file structure
  wasa_out <- format(index(dat_interp_xts), "%d%m%Y")
  wasa_out <- cbind(wasa_out, 1:length(wasa_out))
  wasa_out <- cbind(wasa_out, round(coredata(dat_interp_xts),1))
  colnames(wasa_out) <- c("0", "0", colnames(dat_interp_xts))
  
  # write output
  if (v == "prec") {
    write("Daily average precipitation [mm/d] for each subasin, ordered according to Map-IDs", paste(getwd(),"meteo/rain_daily.dat", sep="/"))
    wasa_name <- "rain_daily"
    dat_prec_interp_xts <- dat_interp_xts # needed later for rainy season
  }
  if (v == "temper") {
    write("Daily average temperature (in degree Celcius) for each subasin, ordered according to Map-IDs", paste(getwd(),"meteo/temperature.dat", sep="/"))
    wasa_name <- "temperature"
  }
  if (v == "relhum"){
    write("Daily average humidity [in %] for each subasin, ordered according to Map-IDs", paste(getwd(),"meteo/humidity.dat", sep="/"))
    wasa_name <- "humidity"
  }
  if (v == "glorad") {
    write("Daily average shortwave radiation [in W/m2] for each subasin, ordered according to Map-IDs", paste(getwd(),"meteo/radiation.dat", sep="/"))
    wasa_name <- "radiation"
  }
  
  write("Date  No. of days, Subasin-ID, Subasin-ID,...", paste0(getwd(), "/meteo/", wasa_name,".dat"), append=T)
  write.table(wasa_out, paste0(getwd(), "/meteo/", wasa_name,".dat"), col.names=T, row.names=F, append=T, quote=F, sep="\t")
  
} # loop over variables



# SEASONALITIES #
# This is about the vegetation parameterisation in WASA

# read in prepared precipitation data (time series for each subbasin centroid)
dat_prec <- read.table(paste0(getwd(), "/meteo_funceme/rain_daily_funceme_idw.dat"), skip=2, header=T)
dat_prec_xts <- xts(dat_prec[,-c(1,2)], as.Date(sprintf(dat_prec[,1], fmt="%08d"), "%d%m%Y"))
colnames(dat_prec_xts) <- gsub("X", "", colnames(dat_prec_xts)) # remove leading X in column names

# 1980 as 1979 to get values for 1980 (minor error because only the last ~3 months of 1980 are takes as 1979)
dat_t <- dat_prec_xts[grep("1980", index(dat_prec_xts)),]
index(dat_t) <- index(dat_t) - nrow(dat_t)
dat_prec_xts <- rbind(dat_t[-1,], dat_prec_xts)

# apply function to calculate start and end of rainy season
dat_rainy <- rainy_season(dat_prec_xts, 243, -9999)
colnames(dat_rainy) <- c("subbas_id", "yearm", "node1", "node2", "node3", "node4")

# make compliant with WASA parameter table
dat_rainy <- cbind(pid=seq(1, nrow(dat_rainy)), dat_rainy, veg_id=rep(-1, nrow(dat_rainy)))

# create output file
write.table(dat_rainy, "rainy_season_funceme_idw_1980_2014.csv", append=F, col.names=T, row.names=F, sep="\t", quote=F)



# RESERVOIR DATA PREPARATION #
x <- reservoir_outlet(dem=dem,
                 res_vct = res_lumped_vect,
                 outlets_vect = res_lumped_outlets,
                 keep_temp = F,
                 overwrite = TRUE)
# lumped reservoirs
reservoir_lumped(
  # INPUT #
  res_vect = res_lumped_outlets,
  sub_rast = subbas,
  # OUTPUT #
  res_vect_class = NULL,
  dir_out = "./reservoir",
  lake_file = "lake.dat",
  lakenum_file = "lake_number.dat",
  # PARAMETER #
  res_param = res_lumped_pars,
  keep_temp = keep_temp,
  overwrite = overwrite,
  silent = silent
)

# strategic reservoirs
# apply function
reservoir_strategic(
  ### INPUT ###
  res_vect = res_strat_outlets,
  subbasin = subbas,
  ### OUTPUT ###
  dir_out = "./reservoir",
  reservoir_file = "reservoir.dat",
  ### PARAMETER ###
  overwrite=overwrite,
  silent=silent
)



# DATABASE #
# rainy_season not included within this example

# create database
?db_create
db_create(dbname)

# update database (if necessary)
?db_update
db_update(dbname)

# create information file for filling landscape_units into database (SIMPLEST POSSIBLE PARAMETER VALUES USED HEREIN)
luout <- read.table("lu.dat", header=T)
lupar <- read.table(lupar_ofile, header=T)
lupar$slopelength <- luout$x_length
lupar$soil_depth <- -1 # groundwater option I.1.1 (WASA documentation) 
lupar$allu_depth <- -1 # groundwater option I.1.1 (WASA documentation) 
lupar$riverbed_depth <- 2000 # riverbed in any case below soil (no information whether this is reasonable or not)
lupar$kf_bedrock <- -9999
lupar$gw_dist <- -9999
lupar$frgw_delay <- -9999
write.table(lupar, "lu_db.dat", quote = F, row.names = F, sep="\t")

# copy soil and vegetation parameter files into output_dir
file.copy(paste(veg_path, "vegetation.dat", sep="/"), "vegetation.dat", overwrite=T)
file.copy(paste(soil_path, "soil.dat", sep="/"), "soil.dat", overwrite=T)
file.copy(paste(soil_path, "horizons.dat", sep="/"), "horizons.dat", overwrite=T)
file.copy(paste(soil_path, "particle_classes.dat", sep="/"), "particle_classes.dat", overwrite=T)
file.copy(paste(soil_path, "r_soil_contains_particles.dat", sep="/"), "r_soil_contains_particles.dat", overwrite=T)

# lumpR output and manually prepared information (e.g. soil parameters) to database
?db_fill
db_fill(dbname=dbname,
        tables = c("r_subbas_contains_lu", "subbasins",
                   "landscape_units", "r_lu_contains_tc", "terrain_components", "r_tc_contains_svc",
                   "vegetation", "soils", "horizons", "soil_veg_components",
                   "particle_classes", "r_soil_contains_particles",
                   "rainy_season"),
        dat_files=c("lu_stats.txt", "sub_stats.txt",
                    "lu_db.dat", "lucontainstc.dat", "terraincomponents.dat", "tc_contains_svc.dat",
                    "vegetation.dat", "soil.dat", "horizons.dat", "soil_vegetation_components.dat",
                    "particle_classes.dat", "r_soil_contains_particles.dat",
                    "rainy_season.csv"), 
        dat_dir=getwd(),
        overwrite=T, verbose=T)

# Please process these cleaning actions step-by-step according to your needs.
# ATTENTION: Execute checks in pre-defined order as some checks build upon each other and lead to erroneous results when interchanged!
?db_check

db_check(dbname, 
         check=c("filter_small_areas"), 
         option=list(area_thresh=0.0001),
         fix=T,
         verbose=T)

db_check(dbname, 
         check=c("tc_slope"), 
         option=list(treat_slope=c(3,0.01,0.1)),
         fix=T,
         verbose=T)

# db_check(dbname,
#          check=c("special_areas"),
#          option=list(special_area = data.frame(reference_tbl=c("vegetation", "vegetation", "soils"), ref_id=c(3,4,10), special_id=c(1,1,2))),
#          fix=F,
#          verbose=F)

db_check(dbname, 
         check=c("remove_water_svc"),
         fix=T,
         verbose=T)

db_check(dbname, 
         check=c("compute_rocky_frac"),
         fix=T,
         verbose=T)

db_check(dbname, 
         check=c("remove_impervious_svc"),
         option=list(update_frac_impervious=F),
         fix=T,
         verbose=T)

# db_check(dbname, 
#          check=c("proxy_frgw_delay"), 
#          option=list(total_mean_delay=50),
#          fix=T,
#          verbose=T)

db_check(dbname,
         check=c("delete_obsolete"),
         fix=T,
         verbose=T)

db_check(dbname,
         check=c("completeness"),
         fix=T,
         verbose=T)

db_check(dbname,
         check=c("subbasin_order"),
         fix=T,
         verbose=T)

# generate input files for WASA
?db_wasa_input
db_wasa_input(dbname = dbname,
              dest_dir = paste(getwd(), "WASA_input_gw_on", sep="/"),
              files=c("info.dat", "River/routing.dat", "River/response.dat", "Hillslope/hymo.dat",
                      "Hillslope/soter.dat", "Hillslope/terrain.dat", "Hillslope/soil_vegetation.dat",
                      "Hillslope/soil.dat", "Hillslope/vegetation.dat", "Hillslope/svc_in_tc.dat",
                      "part_class.dat", "Hillslope/soil_particles.dat", "Hillslope/svc.dat",
                      "Hillslope/rainy_season.dat", "do.dat", "maxdim.dat"),
              overwrite = T, verbose=T)
      
# adjust model input data to your needs ...

# Generate WASA input data outside the scope of lumpR (meteo data etc.) ...
 