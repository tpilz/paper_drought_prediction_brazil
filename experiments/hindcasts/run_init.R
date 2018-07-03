# This file run_init.R is part of the experiment scripts for the
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


# This script runs the WASA-SED model over several sub-catchments (respecting upstream dependencies) and
# years of a given time period. For every sub-catchment and year, a separate output directory is created
# with model states and the simulated time series of river discharge and reservoir volumes. At each year
# (1 Jan), the model is re-initialised with observed strategic reservoir volumes.

library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)

# requires subdirectories: <subcatchment>/<wasa_input_files>
dir_setup <- "../setup"

# output dir; subdirectories will be created: <subcatchment>/<year>/<wasa_output>
dir_out <- "../runs_init"

# start and end dates of model runs
date_start <- as.Date("1980-01-01")
date_end <- as.Date("2014-06-30")

# name of WASA-SED executabe
wasa_app <- "wasa"

# processing order of sub-catchments (some may depend on others)
subcatch <- c("Banabuiu", "Oros", "Salgado", "Castanhao", "Jaguaribe")

# dependencies: which sub-catchment gets inflows from which other sub-catchments + subbasin IDs
depends <- list(Banabuiu = NULL,
                Oros = NULL,
                Salgado = NULL,
                Castanhao = data.frame(Oros = 30, Salgado = 25),
                Jaguaribe = data.frame(Castanhao = 17, Banabuiu = 10))


### CALCULATIONS ###

# sequence of years to be processed (as 1 Jan)
yrs <- seq.Date(date_start, date_end, by="year")

# loop over sub-catchments
for(s in subcatch[c(4,5)]) {
  
  # create sub-dir in dir_out
  dir.create(paste(dir_out, s, sep="/"), recursive = T, showWarnings = F)
  
  # prepare dependencies
  if(!is.null(depends[[s]])) {
    
    # derive river flow from dependencies
    dat_d <- lapply(colnames(depends[[s]]), function(x) {
      # loop over years
      dat_t <- dir(paste(dir_out, x, sep="/"), full.names = T) %>%
        lapply(function(z) {
          # get river flow data
          read.table(paste(z, "River_Flow.out", sep="/"), skip=1, header=T, check.names=F) %>%
            mutate(date = as.Date(paste(year, day, sep="-"), "%Y-%j")) %>%
            select(date, as.character(!!!depends[[s]][[x]])) %>%
            melt(id.vars = "date", variable.name = "subbas")
        }) %>%
        rbind.fill()
      
      # check that all years are available
      r_yrs_miss <- which(!(format(yrs, "%Y") %in% unique(format(dat_t$date, "%Y"))))
      if(length(r_yrs_miss) > 0)
        stop(paste("For sub-catchment", s, "the following years are missing from sub-catchment", x, ":",
                   paste(format(yrs[r_yrs_miss], "%Y"), collapse = ", ")))
      
      return(dat_t)
    }) %>%
      rbind.fill()
    
    # write into file
    dat_out <- dat_d %>%
      dcast(date ~ subbas) %>%
      mutate(doy=as.integer(format(date, "%j")), date = sprintf(as.integer(format(date, "%d%m%Y")), fmt="%08d")) %>%
      select(date, doy, matches("[0-9]*"))
    colnames(dat_out)[c(1,2)] <- c("0", "0")
    writeLines(c("pre-specified	mean	daily	river	flow	[m3/s]	for	selected	sub-basins	(MAP-IDs)",
                 "Date	No.	of	days	Subbasin-ID."), paste(dir_setup, s, "Time_series", "subbasin_out.dat", sep="/"))
    write.table(dat_out, paste(dir_setup, s, "Time_series", "subbasin_out.dat", sep="/"), row.names = F, sep="\t", quote = F, append = T)
  }
  
  # loop over years
  for (yt in seq_along(yrs)) {
    y <- yrs[yt]
    
    # create sub-dir
    dir.create(paste(dir_out, s, year(y), sep="/"), recursive = T, showWarnings = F)
    
    # prepare reservoir input for this iteration
    file_res <- paste(dir_setup, s, "Reservoir", paste0("reservoir_", year(y), ".dat"), sep="/")
    if(!file.exists(file_res))
      stop(paste0("Could not find ", file_res, "!"))
    file.copy(file_res, paste(dir_setup, s, "Reservoir", "reservoir.dat", sep="/"), overwrite = T)
    # check if measurement of vol0 is available and use simulation value from last year if necessary
    dat_res <- read.table(paste(dir_setup, s, "Reservoir", "reservoir.dat", sep="/"), skip=2, header=F)
    dat_res_vol0 <- apply(dat_res, 1, function(dat_t) {
      if(dat_t[4] == -999 & dat_t[9] < year(y) & yt > 1) {
        dat_vol0_t <- read.table(paste0(dir_out, "/", s, "/", year(y)-1, "/", "res_", dat_t[1], "_watbal.out"), header = F, skip = 1)
        dat_vol0 <- last(dat_vol0_t[,16])/1000
      } else {
        dat_vol0 <- dat_t[4]
      }
      return(dat_vol0)
    })
    dat_res[,4] <- dat_res_vol0
    write(c("Specification of reservoir parameters",
            "Subasin-ID, minlevel[m], maxlevel[m], vol0([1000m**3]; unknown=-999), storcap[1000m**3], damflow[m**3/s], damq_frac[-], withdrawal[m**3/s], damyear[YYYY], maxdamarea[ha], damdead[1000m**3], damalert[1000m**3], dama[-], damb[-], qoutlet[m**3/s], fvol_bottom[-], fvol_over[-], damc[-], damd[-], elevbottom[m]"),
          file = paste(dir_setup, s, "Reservoir", "reservoir.dat", sep="/"), sep="\n")
    write.table(dat_res, paste(dir_setup, s, "Reservoir", "reservoir.dat", sep="/"), row.names=F, col.names = F, sep="\t", quote=F, append = T)
    
    # prepare do.dat
    dodat <- readLines(paste(dir_setup, s, "do.dat", sep="/"))
    dodat[2] <- "./"
    dodat[3] <- paste("../", dir_out, s, year(y), "", sep="/")
    dodat[4] <- paste0(year(y), "\t//tstart (start year of simulation)")
    dodat[5] <- paste0(year(y), "\t//tstop (end year of simulation)")
    dodat[6] <- paste0(format(y, "%m"), "\t//mstart (start month of simulation)")
    dodat[7] <- "12\t//mstop (end month of simulation)"
    dodat[36] <- ".t. //load state of storages from files (if present) at start (optional)"
    dodat[37] <- ".t.\t//save state of storages to files after simulation period (optional)"
    writeLines(dodat, paste(dir_setup, s, "do.dat", sep="/"))
    
    # make sure required output is generated
    outfiles <- c("This files describe which output files are generated",
                  "res_watbal",
                  "River_Flow")
    writeLines(outfiles, paste(dir_setup, s, "outfiles.dat", sep="/"))
    
    # make sure intake is used
    if(!file.exists(paste(dir_setup, s, "Time_series/intake.dat", sep="/"))) {
      if(file.exists(paste(dir_setup, s, "Time_series/intake_t.dat", sep="/"))) {
        file.copy(paste(dir_setup, s, "Time_series/intake_t.dat", sep="/"), paste(dir_setup, s, "Time_series/intake.dat", sep="/"), overwrite = T)
      } else {
        stop(paste0("File intake.dat is missing at ", dir_setup, "/", s, "!"))
      }
    }
    
    # copy storage files from previous year to output directory
    if(yt > 1) {
      files_stor <- grep("*.stat$", dir(paste(dir_out, s, format(yrs[yt-1], "%Y"), sep="/"), full.names = T), value = T)
      file.copy(files_stor, paste(dir_out, s, year(y), sep="/"), overwrite = T)
    }
    
    # run WASA
    run_log <- system(command = paste0(wasa_app, " ", dir_setup, "/", s, "/do.dat"), intern = T)
    if(any(grepl("error", run_log, ignore.case = T))) {
      writeLines(run_log, paste0("run_", s, "_", year(y), ".log"))
      stop(paste("WASA returned a runtime error, see log file:", paste0("run_", s, "_", year(y), ".log")))
    }
  }
}
