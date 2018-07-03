# This file calc_subcatch.R is part of the experiment scripts for the
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


# This script extracts sub-catchments from a WASA parameter database and pre-processes the input to prepare calibration
# Pre-processing steps for every extracted sub-catchment:
# - create wasa input files
# - use meteo input from a template and extract the necessary subbasins for the sub-catchment
# - use reservoir input from a template and extract the necessary subbasins for the sub-catchment
#   - this supports the following reservoir files: "cav.dat", "lake_number.dat", "reservoir.dat", "lake_maxvol.dat"
#   - order of rows (subbasins) in files adapted to order in hymo.dat
# - do.dat: start and end of simulation (i.e., calibration) period, kfkorr transient with params 1 10 0
# - create gauges_catchment_area.txt (dds specific)
# - prepare observations for calibration (compliant with gauges_catchment_area.txt)
#   - for outfiles equal 'res_watbal', volume differences are calculated (will be treated as "streamflow" due to lack of other information)
# - create file calibration.dat for calibration of ksat (set to 1 for all units)
# - outfiles.dat according to selection

library(lumpR)
library(plyr)
library(dplyr)
library(xts)

setwd("/home/tobias/Promotion/Modellierung/Jaguaribe_new/lumpR/subcatch")

dbname <- "wasa_jaguaribe"

# Subbasin IDs, gage IDs and names of reservoirs/discharge gages to be selected (order needs to be the same!)
sub_extract <- c(1, # Jaguaribe (lower)
                 17, # Castanhao
                 10, # Banabuiu
                 30, # Oros
                 25) # Rio Salgado (river gage instead of reservoir)
gage_ids <- c(NA, 194, 2, 9, 36290000)
dbname_new <- c("Jaguaribe", "Castanhao", "Banabuiu", "Oros", "Salgado")

# start and end dates of calibration period
date_start <- "1980-01-01"
date_end <- "2014-06-30"

# meteo input (whole jaguaribe)
meteo_dir <- "../results_new/meteo_funceme/"

# reservoir input (whole jaguaribe)
reservoir_dir <- "../results_new/reservoir_calib/"

# prepared file with obervations (for use within dds)
obs_file <- data.frame(volume = "../../data/reservoir/volume/calc_vol_1000m3_02_2018.dat",
                 discharge = "../../WASA_calib/dds/old/runs_2017-05/prep_obs/discharge_obs_24.txt")

# give a vector with output files to be generated (will be inserted into outfiles.dat) for each sub-catchment, i.e. list element (except Jaguaribe which was not calibrated)
outfiles <- list(NULL,
                 c("res_watbal"),
                 c("res_watbal"),
                 c("res_watbal"),
                 c("River_flow"))


### CALCULATIONS ###

# extract sub-catchments (takes a lot of time due to slow database operations!)
db_extract_subcatch(dbname, sub_extract, dbname_new, verbose = T)

# loop over sub-catchs
for (i in 3:length(dbname_new)) {
#for(i in 2) { # Castanhao after running first half of this script and then prep_castanhao.R
#for(i in 1) { # Lower Jaguaribe running first half of this script and then prep_jaguaribe.R
  dir_out <- paste0("wasa_input_", dbname_new[i], "/input/")

  # create wasa input files
  db_wasa_input(dbname_new[i], dest_dir=dir_out, verbose=F)
  
  # read subbasin data
  dat_sub <- readLines(paste0(dir_out, "/Hillslope/hymo.dat"))
  dat_sub <- strsplit(dat_sub[-c(1,2)], "\t")
  dat_sub <- matrix(as.numeric(unlist(lapply(dat_sub, function(x) x[c(1,2)]))), ncol=2, dimnames = list(NULL, c("id", "area")), byrow = T)

  # adjust meteo input
  dir.create(paste0(dir_out, "/Time_series/"))
  file.copy(dir(meteo_dir, full.names = T), paste0(dir_out, "/Time_series/"))
  # loop over meteo input files and select subbasins
  for (f in c("humidity.dat", "radiation.dat", "rain_daily.dat", "temperature.dat")) {
    filename <- paste0(dir_out, "/Time_series/", f)
    dat_head <- readLines(filename, n = 2)
    dat <- read.table(filename, skip=2, header=T, check.names = F)
    cols <- which(as.numeric(colnames(dat)) %in% dat_sub[,"id"])
    dat_out <- dat[,c(1,2,cols)]
    dat_out[,1] <- sprintf(dat_out[,1],fmt="%08d")
    colnames(dat_out)[c(1,2)] <- c(0,0)
    writeLines(dat_head, filename)
    write.table(dat_out, filename, append = T, row.names = F, sep = "\t", quote = F)
  }

  # adjust reservoir input
  dir.create(paste0(dir_out, "/Reservoir/"))
  file.copy(dir(reservoir_dir, full.names = T), paste0(dir_out, "/Reservoir/"))
  # loop over reservoir input files and select subbasins
  for (f in c("cav.dat", "lake_number.dat", "reservoir.dat", "lake_maxvol.dat")) {
    filename <- paste0(dir_out, "/Reservoir/", f)
    dat <- readLines(filename)
    sub <- as.numeric(lapply(strsplit(dat[-c(1,2)], "\t"), function(x) x[1]))
    dat_df <- data.frame(sub_id = sub, data = dat[-c(1,2)])
    dat_join <- inner_join(data.frame(sub_id = dat_sub[,"id"]), dat_df, by="sub_id")
    write(dat[c(1,2)], filename)
    write(as.character(dat_join$data), filename, append = T)
  }
  # select columns in intake.dat
  dat_head <- readLines(paste(dir_out, "Reservoir", "intake.dat", sep="/"), n=1)
  dat_intake <- read.table(paste(dir_out, "Reservoir", "intake.dat", sep="/"), header = T, sep="\t", skip=1, check.names = F)
  dat_intake_out <- dat_intake[,c(1,2, which(colnames(dat_intake) %in% dat_sub[,1]))]
  writeLines(dat_head, paste(dir_out, "Time_series", "intake.dat", sep="/"))
  suppressWarnings(write.table(dat_intake_out, paste(dir_out, "Time_series", "intake.dat", sep="/"), row.names=F, sep="\t", quote=F, append=T))
  unlink(paste(dir_out, "Reservoir", "intake.dat", sep="/"))

  # adjust do.dat for calibration
  dat <- readLines(paste0(dir_out, "/do.dat"))
  dat_out <- dat
  dat_out[2] <- "../input/"
  dat_out[3] <- "../output/"
  dat_out[4] <- paste0(format(as.Date(date_start), "%Y"), "\t//tstart (start year of simulation)")
  dat_out[5] <- paste0(format(as.Date(date_end), "%Y"), "\t//tstop (end year of simulation)")
  dat_out[6] <- paste0(format(as.Date(date_start), "%m"), "\t//mstart (start month of simulation)")
  dat_out[7] <- paste0(format(as.Date(date_end), "%m"), "\t//mstop (end month of simulation)")
  dat_out[14] <- ".t.\t//doreservoir: do reservoir calculation"
  dat_out[15] <- ".t.\t//doacudes:includes dam calculations"
  dat_out[24] <- "1 10 0\t//kfkorr:  hydraulic conductivity factor (for daily model version) (kfkorr)"
  writeLines(dat_out, paste0(dir_out, "/do.dat"))

  # create gauges_catchment_area.txt for calibration (information about which subbasin to use)
  if(!is.na(gage_ids[i])) {
    dat_out <- data.frame(GAUGE = dbname_new[i],
                          FOREIGN_ID = gage_ids[i],
                          SUBBAS_ID = sub_extract[i],
                          AREA_SUBBAS_KM2 = dat_sub[which(dat_sub[,"id"] == sub_extract[i]), "area"],
                          AREA_UPSTREAM_KM2 = sum(dat_sub[,"area"]))
    write.table(dat_out, paste0(dir_out, "/gauges_catchment_area.txt"), row.names = F, sep="\t", quote = F)

    # copy prepared file with observations for calibration (compliant with gauges_catchment_area.txt)
    if (outfiles[[i]] == "River_flow") {
      file.copy(as.character(obs_file$discharge), paste0(dir_out, "Time_series/"))
    } else if(outfiles[[i]] == "res_watbal") {
      dat <- read.table(as.character(obs_file$volume), header = T, check.names = F)
      dat_obs <- xts(dat[,paste0(dat_out$FOREIGN_ID)]*1000, as.Date(dat$date)) # in m3
      # calculate volume increase for timestep i as vol(i+1) - vol(i)
      vol_diff <- diff(dat_obs)
      vol_diff <- ifelse(vol_diff > 0, vol_diff, 0)
      # relevant time frame
      dat_obs_out <- vol_diff[seq.Date(as.Date(date_start), as.Date(date_end), by="day"),]
      # output
      dat_out <- list(YYYY = format(index(dat_obs_out), "%Y"),
                            MM = format(index(dat_obs_out), "%m"),
                            DD = format(index(dat_obs_out), "%d"),
                            HH = 0)
      dat_out[[dbname_new[i]]] <- as.numeric(coredata(dat_obs_out))
      write.table(dat_out, paste0(dir_out, "Time_series/resvoldiff_obs.txt"), row.names = F, quote = F, sep="\t")
    }
  }

  # create additional calibration files
  dir.create(paste0(dir_out, "/Others"))
  writeLines(c("Soil-ID	Ksat-calib-factor", "-1\t1"), paste0(dir_out, "/Others/calibration.dat"))
  writeLines("1", paste0(dir_out, "/Others/calib_wind.dat"))
  writeLines("1", paste0(dir_out, "/Hillslope/frac_direct_gw.dat"))

  # outfiles.dat
  writeLines(c("This files describe which output files are generated",
               "put any character before the files you don't want to be created",
               outfiles[[i]]), paste0(dir_out, "outfiles.dat"))
}
