# This file function_wraper.R is part of the analysis scripts for the
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


# function called by cal_*.R scripts
calib_wasa <- function(pars, init_dir, period, modus, rundir = NULL, keep_rundir = FALSE, outfiles = NULL) {
  require(xts)
  # initialisation of variables (can also be added as function arguments if it is more convenient)
  wasa_cmd <- "wasa"
  sim_start <- c(format(period[1], "%Y"), format(period[1], "%m"))
  sim_end <- c(format(period[2], "%Y"), format(period[2], "%m"))
  prerun_length <- 365 #length of prerun phase [days] (use 365 for a year and 28 for a month)
  max_pre_runs=20 # max number (years) of pre-run to achieve storage equilibrium (abortion criteria for pre-run phase)
  storage_tolerance=0.01 # max allowed relative difference in storage volume between successive pre-runs (abortion criteria for pre-run phase)
  
  # create temporary directory for WASA run
  if (modus == "calibration") {
    
    rundir <- ifelse(is.null(rundir), tempfile(pattern = "run"), tempfile(pattern = "run", tmpdir = rundir))
    dir_run <-  paste(rundir, "wasa_input", sep="/")
    dir.create(dir_run, recursive = T)
    # copy initial directory to dir_run
    file.copy(paste0(init_dir, "/."), dir_run, recursive = T)
    
    # output directory
    dir_out <- paste(rundir, "wasa_output", sep="/")
    dir.create(dir_out, recursive = T)
    
  } else if (modus == "analysis") {
    
    dir_run <- paste(rundir, "wasa_input", sep="/")
    dir.create(dir_run, recursive = T)
    # copy initial directory to dir_run
    file.copy(paste0(init_dir, "/."), dir_run, recursive = T)
    
    # output directory
    dir_out <- paste(rundir, "wasa_output", sep="/")
    dir.create(dir_out, recursive = T)
  }
  
  ### MODIFY WASA INPUT ###
  # outfiles.dat
  content <- "This files describe which output files are generated"
  write(content, file = paste(dir_run, "outfiles.dat", sep="/"))
  # do.dat
  do_dat <- readLines(paste(dir_run, "do.dat", sep="/"))
  do_dat[2] <- "../wasa_input/"
  do_dat[3] <- "../wasa_output/"
  do_dat[4] <- paste0(sim_start[1], "\t//tstart (start year of simulation)")
  do_dat[5] <- paste0(sim_end[1], "\t//tstop (end year of simulation)")
  do_dat[6] <- paste0(sim_start[2], "\t//mstart (start month of simulation)")
  do_dat[7] <- paste0(sim_end[2], "\t//mstop (end month of simulation)")
  do_dat[14] <- ".t.\t//doreservoir: do reservoir calculations"
  do_dat[15] <- ".t.\t//doacudes:includes dam calculations"
  do_dat[36]=".t. .f.\t//load state of storages from files (if present) at start (optional)"
  do_dat[37] <- ".t. .f.\t//save state of storages to files after simulation period (optional)"
  writeLines(do_dat, paste(dir_run, "do.dat", sep="/"))
  # replace log values by normal values for use in WASA (log is just a matter of sampling)
  if(any(grepl(names(pars), pattern = "^log_"))) {
    log_trans = grepl(names(pars), pattern = "^log_") #find log-transformed parameters
    pars[log_trans]=exp(pars[log_trans]) #transform back to non-log scale
    names(pars) = sub(names(pars), pattern = "^log_", rep="") #remove "log_" from name
  }
  # initialise temporary parameter vector (keep track which parameters have been processed)
  pars_t <- pars
  # parameters in vegetation.dat
  if (any(names(pars_t) %in% c("stomr_f", "rootd_f", "albedo_f"))) {
    # file that hold the parameters to be changed
    target_file <- paste(dir_run,"Hillslope/vegetation.dat",sep="/")
    # read data
    file_content <- read.table(target_file, skip=2, header = FALSE, sep = "\t", dec = ".", fill = TRUE)
    if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
    # multiply somatal resistance in vegetation.dat by factor
    nn= which(names(pars_t)=="stomr_f")
    if (length(nn)>0) {
      file_content[,2]=file_content[,2]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    # multiply rootdepth in vegetation.dat by factor (all 4 values)
    nn= which(names(pars_t)=="rootd_f")
    if (length(nn)>0) {
      file_content[,c(9:12)]=file_content[,c(9:12)]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    # multiply albedo in vegetation.dat by factor (all 4 values)
    nn= which(names(pars_t)=="albedo_f")
    if (length(nn)>0) {
      file_content[,c(17:20)]=pmin(1,file_content[,c(17:20)]*pars_t[nn])
      pars_t <- pars_t[-nn]
    }
    #re-write file
    content=paste("Specification of vegetation parameters\nVeg-ID	Stomata_Resistance[s/m]	minsuction[hPa]	maxsuction[hPa]	height1[m]	height2[m]	height3[m]	height4[m]	rootdepth1[m]	rootdepth2[m]	rootdepth3[m]	rootdepth4[m]	LAI1[-]	LAI2[-]	LAI3[-]	LAI4[-]	albedo1[-]	albedo2[-]	albedo3[-]	albedo4[-]",sep = "")
    write(content, file = target_file)     #write header
    write.table(round(file_content, 3), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
  }
  # parameters in soil.dat
  if (any(names(pars_t) %in% c("n_f", "cfr"))) {
    # file that hold the parameters to be changed
    target_file=paste(dir_run,"Hillslope/soil.dat",sep="/")
    # read data
    file_content = read.table(target_file, skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
    if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
    
    # multiply porosity in soil.dat by factor (equally for every horizon and soil)
    nn= which(names(pars_t)=="n_f")
    if (length(nn)>0) {
      # iterate over soils (lines in data)
      for (s in 1:nrow(file_content)) {
        # get number of horizons
        nhor = file_content[s,2]
        # update parameter
        file_content[s,12+(1:nhor-1)*13] = pmin(1,file_content[s,12+(1:nhor-1)*13]*pars_t[nn])
      }
      pars_t <- pars_t[-nn]
    }
    # add 'cfr' to coarse fragments in soil.dat (additive!, equally for every horizon and soil)
    nn= which(names(pars_t)=="cfr")
    if (length(nn)>0) {
      # iterate over soils (lines in data)
      for (s in 1:nrow(file_content)) {
        # get number of horizons
        nhor = file_content[s,2]
        # update parameter
        file_content[s,14+(1:nhor-1)*13] = pmin(1,pmax(0,file_content[s,14+(1:nhor-1)*13]+pars_t[nn]))
      }
      pars_t <- pars_t[-nn]
    }
    #re-write file
    content=paste("Specification of soil parameters\nSoil-ID[-]	number(horizons)[-]	(n_res[Vol-]	n_PWP[-]	n_FK2.6[-]	n_FK1.8[-]	n_nFK[-]	n_saturated[-]	n_thickness[mm]	n_ks[mm/d]	n_suction[mm]	n_pore-size-index[-]	n_bubblepressure[cm]	n_coarse_frag[-]*n	n_shrinks[0/1])	bedrock[0/1]	alluvial[0/1]",sep = "")
    write(content, file = target_file)     #write header
    write.table(round(file_content,4), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
  }
  # parameters in response.dat
  if (any(names(pars_t) %in% c("uhg_f"))){
    # file that hold the parameters to be changed
    target_file=paste(dir_run,"River/response.dat",sep="/")
    # read data
    file_content = read.table(target_file, skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
    if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
    # multiply lag and response times in response.dat by factor
    nn= which(names(pars_t)=="uhg_f")
    if (length(nn)>0) {
      file_content[,2:3] <- file_content[,2:3]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    #re-write file
    content=paste("Specification of routing parameter\nSubbasin-ID	lag time [d]	retention [d]",sep = "")
    write(content, file = target_file)     #write header
    write.table(round(file_content,2), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
  }
  # parameters in soter.dat
  if (any(names(pars_t) %in% c("gw_delay_f","soildepth_f","kf_bedrock_f","riverdepth_f"))) {
    # file that hold the parameters to be changed
    target_file=paste(dir_run,"Hillslope/soter.dat", sep="/")
    # read data
    file_content = read.table(target_file, skip=2,header = FALSE, sep = "\t", dec = ".", fill = TRUE)
    if (all(!is.finite(file_content[,ncol(file_content)]))) file_content[,ncol(file_content)]=NULL  #discard last column if empty
    # consider shorter lines (LUs with less TCs) and bring fields to consistent position in matrix
    max_n_tcs=max(file_content[,2])
    shorter_lines=which(file_content[,2] != max_n_tcs)
    n_fields=ncol(file_content)-max_n_tcs -2 #number of fields after specification of TC-IDs
    if(length(shorter_lines) > 0) {
      for (ll in shorter_lines) {
        n_tcs = file_content[ll,2]
        file_content[ll, 2+max_n_tcs+(1:n_fields)] = file_content[ll, 2+n_tcs    +(1:n_fields)]
      }
    }
    nn= which(names(pars_t)=="gw_delay_f")
    if (length(nn)>0) {
      file_content[,ncol(file_content)]=file_content[,ncol(file_content)]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    nn= which(names(pars_t)=="soildepth_f")
    if (length(nn)>0) {
      not_minus1=file_content[,ncol(file_content)-4]!=-1   #only modify entries not having flag=-1
      file_content[not_minus1,ncol(file_content)-4]=file_content[not_minus1,ncol(file_content)-4]*pars_t[nn]
      not_minus1=file_content[,ncol(file_content)-5]!=-1
      file_content[not_minus1,ncol(file_content)-5]=file_content[not_minus1,ncol(file_content)-5]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    nn= which(names(pars_t)=="kf_bedrock_f")
    if (length(nn)>0) {
      file_content[,ncol(file_content)-7]=file_content[,ncol(file_content)-7]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    nn= which(names(pars_t)=="riverdepth_f")
    if (length(nn)>0) {
      file_content[,ncol(file_content)-3]=file_content[,ncol(file_content)-3]*pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    #consider shorter lines (LUs with less TCs) and bring fields to "spars_te" representation (unequal number of fields)
    if(length(shorter_lines) > 0) {
      for (ll in shorter_lines) {
        file_content[ll,]
        n_tcs = file_content[ll,2]
        file_content[ll, 2+n_tcs    +(1:n_fields)] =
          file_content[ll, 2+max_n_tcs+(1:n_fields)]
        file_content[ll, ncol(file_content)+1-1:(max_n_tcs-n_tcs)] = NA #mask obsolete fields with NA
      }
    }
    #re-write file
    content=paste("Specification of landscape units\nLU-ID[id]  No._of_TC[-]	TC1[id]	TC2[id]	TC3[id]	kfsu[mm/d]	length[m]	meandep[mm]	maxdep[mm]	riverbed[mm]	gwflag[0/1]	gw_dist[mm]	frgw_delay[day]",sep = "")
    write(content, file = target_file)     #write header
    write.table(round(file_content, 2), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t", na = "")
  }
  # params in do.dat
  if (any(names(pars_t) %in% c("kfcorr","kfcorr0","kfcorr_a", "intcf", "dosediment"))) {
    # adjust kfcorr in do.dat
    target_file=paste(dir_run,"do.dat",sep="/") #file that hold the parameters to be changed
    # read data
    file_content = read.table(target_file, skip=0,header = FALSE, sep = "$",stringsAsFactors=FALSE)
    nn= which(names(pars_t)=="kfcorr")
    if (length(nn)>0) {
      file_content[24,1]=paste(round(pars_t[nn],2),"  //kfcorr:  hydraulic conductivity factor (for daily model version) (kfcorr)",sep="")
      pars_t <- pars_t[-nn]
    }
    if(any(names(pars_t)=="kfcorr0") || any(names(pars_t)=="kfcorr_a")) {
      nn= which(names(pars_t)=="kfcorr0")
      if (length(nn)>0) {
        file_content[24,1]=paste(round(pars_t[nn],2)," ",sep="")
        pars_t <- pars_t[-nn]
      } else { # default value
        file_content[24,1]="1 "
      }
      nn= which(names(pars_t)=="kfcorr_a")
      if (length(nn)>0) {
        file_content[24,1]=paste(file_content[24,1],round(pars_t[nn],2)," 0	//kfcorr:  hydraulic conductivity factor (for daily model version) (kfcorr0) [optional: a <tab> b for kfcorr=kfcorr0*(a*1/daily_precip+b+1) ",sep="")
        pars_t <- pars_t[-nn]
      } else { # default value
        file_content[24,1]=paste(file_content[24,1],"0 0	//kfcorr:  hydraulic conductivity factor (for daily model version) (kfcorr0) [optional: a <tab> b for kfcorr=kfcorr0*(a*1/daily_precip+b+1) ",sep="")
      }
    }
    nn= which(names(pars_t)=="intcf")
    if (length(nn)>0) {
      file_content[25,1]=paste(round(pars_t[nn], 2)," //intcf: interception capacity per unit LAI (mm)",sep="")
      pars_t <- pars_t[-nn]
    }
    nn= which(names(pars_t)=="dosediment")
    if (length(nn)>0) {
      file_content[31,1]=paste(pars_t[nn],"  //dosediment")
      pars_t <- pars_t[-nn]
    }
    #rewrite file
    write.table(file_content, file = target_file, append = F, quote = F,row.names=F,col.names=F,sep="\t")
  }
  # params in calibration.dat
  if (any(names(pars_t) %in% c("ksat_factor"))) {
    # file that holds the parameters to be changed
    target_file=paste(dir_run,"Others/calibration.dat",sep="/")
    # read data
    file_content = read.table(target_file, skip=1,row.names=NULL, header = FALSE, sep = "\t", dec = ".")
    nn= which(names(pars_t)=="ksat_factor")
    if (length(nn)>0) {
      file_content[,2]=pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    content=paste("Soil-ID","Ksat-calib-factor",sep="\t")
    write(content, file = target_file)     #write header
    write.table(round(file_content,3), file = target_file, append = TRUE, quote = F,row.names=F,col.names=F,sep="\t")
  }
  # params in frac_direct_gw.dat
  if (any(names(pars_t) %in% c("f_direct_gw"))) {
    # file that holds the parameters to be changed
    target_file=paste(dir_run,"Hillslope/frac_direct_gw.dat",sep="/")
    # read data
    file_content = read.table(target_file, header = FALSE, sep = "\t")
    nn= which(names(pars_t)=="f_direct_gw")
    if (length(nn)>0) {
      file_content=pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    write.table(round(file_content,3), file = target_file, quote = F,row.names=F,col.names=F,sep="\t")
  }
  # volumes in lake.dat / lake_maxvol.dat
  if (any(names(pars_t) %in% c("f_lakemaxvol"))) {
    nn= which(names(pars_t)=="f_lakemaxvol")
    # file that holds the parameters to be changed
    target_file=paste(dir_run,"Reservoir/lake_maxvol.dat",sep="/")
    if(file.exists(target_file)) {
      # read data
      file_content = read.table(target_file, header = FALSE, sep = "\t", skip=2)
      # update parameters
      file_content[,2:ncol(file_content)] <- file_content[,2:ncol(file_content)] * pars_t[nn]
      # write updated parameters
      write(c("Specification of water storage capacity for the reservoir size classes",
              "Sub-basin-ID, maxlake[m**3] (five reservoir size classes)"),
            file = target_file, sep="\n")
      write.table(round(file_content, 2), target_file, append = T, row.names=F, col.names=F, quote=F, sep="\t")
    }
    # file that holds the parameters to be changed
    target_file=paste(dir_run,"Reservoir/lake.dat",sep="/")
    if(file.exists(target_file)) {
      # read data
      file_content = read.table(target_file, header = FALSE, sep = "\t", skip=2)
      # update parameters
      file_content[,2] <- file_content[,2] * pars_t[nn]
      # write updated parameters
      write(c("Specification of parameters for the reservoir size classes",
              "Reservoir_class-ID, maxlake0[m**3], lake_vol0_factor[-], lake_change[-], alpha_Molle[-], damk_Molle[-], damc_hrr[-], damd_hrr[-]"),
            file = target_file, sep="\n")
      write.table(round(file_content, 2), target_file, append = T, row.names=F, col.names=F, quote=F, sep="\t")
    }
    pars_t <- pars_t[-nn]
  }
  # param in calib_wind.dat
  if (any(names(pars_t) %in% c("f_wind"))) {
    # file that holds the parameters to be changed
    target_file=paste(dir_run,"Others/calib_wind.dat",sep="/")
    # read data
    file_content = read.table(target_file, header = FALSE, sep = "\t")
    nn= which(names(pars_t)=="f_wind")
    if (length(nn)>0) {
      file_content=pars_t[nn]
      pars_t <- pars_t[-nn]
    }
    write.table(round(file_content,3), file = target_file, quote = F,row.names=F,col.names=F,sep="\t")
  }
  # all parameters used?
  if(length(pars_t) > 0)
    stop(paste("The following parameter(s) was/were not used (no instruction implemented):", paste(names(pars_t), collapse = ", ")))
  
  ### WARMUP RUNS ###
  # modify do.dat for pre-run-phase
  target_file=paste(dir_run,"do.dat",sep="/") #file that hold the parameters to be changed
  file.copy(target_file,paste(target_file,".full_time",sep=""))   #save original do.dat
  file_content = scan(target_file, what=character(), sep="\n", quiet = T)
  start_year=as.numeric(strsplit(file_content[4], "\t")[[1]][1])
  start_month=as.numeric(strsplit(file_content[6], "\t")[[1]][1])
  end_date_prerun=as.POSIXlt(ISOdate(start_year+1, start_month, 1, hour = 0, min = 0, sec = 0, tz = "GMT")-3600*24 )      #assemble date vector
  end_date_prerun=as.POSIXlt(ISOdate(start_year, start_month, 1, hour = 0, min = 0, sec = 0, tz = "GMT")+(prerun_length-1)*3600*24 )      #assemble date vector
  file_content[5]=end_date_prerun$year+1900  #configure model to run 1 year for prerun-phase
  file_content[7]=end_date_prerun$mon+1
  file_content[36]=".t. .f. //load state of storages from files (if present) at start (optional)"
  file_content[37]=".t. .f. //save state of storages to files after simulation period (optional)"
  write.table(file_content, file = target_file, append = F, quote = F, row.names=F, col.names=F, sep="\t")   #rewrite file
  # do pre-runs until equilibrium or maximum number achieved
  storage_file <- paste(dir_out,"storage.stats",sep="/") # WASA storage file
  storage_before <- 0 # initialise water storage tracker
  for (i in 1:max_pre_runs) {
    # run WASA
    run_log <- system(command = paste0(wasa_cmd, " ", dir_run, "/do.dat"), intern = T)
    if(any(grepl("error", run_log, ignore.case = T))) {
      writeLines(run_log, paste(init_dir, "run.log", sep="/"))
      stop(paste("WASA returned a runtime error during warm-up, see log file:", paste(init_dir, "run.log", sep="/")))
    }
    # water storage
    storage_after =read.table(storage_file, skip=1,header = F,row.names=1)
    rel_storage_change= abs(sum(storage_after)-sum(storage_before))
    if (sum(storage_before)!=0) rel_storage_change=rel_storage_change/sum(storage_before) #avoid NaNs sum(storage_before)==0
    # check if storage changes are below tolerance limit
    if (rel_storage_change < storage_tolerance) break
    storage_before <- storage_after
  }
  if(i == max_pre_runs) warning(paste0("Maximum no of warm-up runs exceeded! Rel. storage change is ", rel_storage_change))
  
  ### ACTUAL MODEL RUN ###
  # which target variable?
  obsdat <- dir(paste(dir_run, "Time_series", sep="/"), pattern = "obs")
  obstype <- unlist(strsplit(obsdat, "_"))[1]
  # restore original do.dat
  file.rename(paste(target_file,".full_time",sep=""),target_file)
  # make sure the needed output is produced
  if(obstype == "resvoldiff")
    content <- "res_watbal"
  else if(obstype == "discharge")
    content <- "River_flow"
  content <- c(content, outfiles)
  write(content, file = paste(dir_run, "outfiles.dat", sep="/"), append = T, sep="\n")
  # run WASA
  run_log <- system(command = paste0(wasa_cmd, " ", dir_run, "/do.dat"), intern = T)
  if(any(grepl("error", run_log, ignore.case = T))) {
    writeLines(run_log, paste(init_dir, "run.log", sep="/"))
    stop(paste("WASA returned a runtime error during the simulation, see log file:", paste(init_dir, "run.log", sep="/")))
  }
  
  ### GET MODEL OUTPUT AND CALC DIAGNOSTICS ###
  # get simulations
  obs_meta <- read.table(paste(dir_run,"gauges_catchment_area.txt", sep="/"), header = T)
  if(obstype == "resvoldiff") {
    # get simulation results
    dat_sim <- read.table(paste0(dir_out, "/res_", obs_meta$SUBBAS_ID, "_watbal.out"), header = T)
    # calculate volume increase for timestep i as vol(i+1) - vol(i) as in observations
    vol_diff <- diff(dat_sim$volume.m..3.)
    vol_diff <- ifelse(vol_diff > 0, vol_diff, 0)
    sim <- xts(vol_diff, head(as.Date(paste(dat_sim$year., dat_sim$day., sep="-"), format = "%Y-%j"), n=-1))
    # get observations
    dat <- read.table(paste(dir_run, "Time_series", obsdat, sep="/"), header = T, check.names = F)
    obs <- xts(dat[,as.character(obs_meta$GAUGE)], as.Date(paste(dat$YYYY, dat$MM, dat$DD, sep="-")))
    # merge data
    dat_xts <- merge(sim, obs)
    dat_xts <- dat_xts[seq.Date(as.Date(period[1]), as.Date(period[2]), by="day"),]
  } else if(obstype == "discharge") {
    # get simulation results
    dat_sim <- read.table(paste0(dir_out, "/River_Flow.out"), header = T, skip=1, check.names = F)
    sim <- xts(dat_sim[,as.character(obs_meta$SUBBAS_ID)], as.Date(paste(dat_sim$year, dat_sim$day, sep="-"), format = "%Y-%j"))
    # get observations
    dat <- read.table(paste(dir_run, "Time_series", obsdat, sep="/"), header = T, check.names = F)
    obs <- xts(dat[,as.character(obs_meta$GAUGE)], as.Date(paste(dat$YYYY, dat$MM, dat$DD, sep="-")))
    # merge data
    dat_xts <- merge(sim, obs)
    dat_xts <- dat_xts[seq.Date(as.Date(period[1]), as.Date(period[2]), by="day"),]
  }
  
  # nash sutcliffe value against mean day-of-year values as benchmark instead of mean of observations, see Schaefli and Gupta (2007), 10.1002/hyp.6825
  # sort out NAs
  nas <- which(is.na(dat_xts), arr.ind = T)
  dat_xts_narm <- dat_xts[-nas[,1],]
  # create benchmark model
  bench <- aggregate(coredata(dat_xts_narm[,"obs"]), list(doy = format(index(dat_xts_narm), "%j")), mean)
  bench <- merge(data.frame(date = index(dat_xts_narm), doy = format(index(dat_xts_narm), "%j")), bench, by="doy")
  bench <- bench$obs[order(bench$date)]
  # calc Nash-Sutcliffe Benchmark value
  nsb <- 1 - sum((dat_xts_narm[,"sim"] - dat_xts_narm[,"obs"])^2) / sum((dat_xts_narm[,"obs"]-bench)^2)
  
  # clean up
  if(!keep_rundir) unlink(rundir, recursive = T)
  # output
  return(-1 * nsb)
}
