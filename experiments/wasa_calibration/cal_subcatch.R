# This file cal_subcatch.R is part of the analysis scripts for the
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


# main calibration script
# defines parameter ranges and calls the calibration function
# to be applied individually to each subcatchment

library(ppso)

working_dir = "/mnt/scratch/tpilz/wasa_calib_jaguaribe/gw_on/Banabuiu"
code_dir = "/mnt/scratch/tpilz/wasa_calib_jaguaribe/R_code" # directory with R scripts (wrapper functions)
source(paste(code_dir, "function_wrapper.R", sep="/")) # external R file with wrapper function
max_number_function_calls = 5000
period = c("2003-01-01", "2010-12-31") # calibration period

setwd(working_dir)

#define parameter ranges
param_ranges <- rbind(
  log_f_wind = log(c(0.1,10)),
  f_lakemaxvol = c(0.5,1.5),
  log_gw_delay_f=log(c(0.1,5)),
  f_direct_gw = c(0,1),
  log_kf_bedrock_f=log(c(1e-2,1e2)),
  log_riverdepth_f=log(c(1e-1,2)),
  kfcorr_a=c(0,100),
  intcf=c(0.1,2),
  log_ksat_factor=log(c(1e-2,1e2)),
  n_f=c(0.75,1.25),
  cfr=c(-0.25,0.25), # additive!
  stomr_f=c(0.5,1.5),
  rootd_f=c(0.5,2),
  albedo_f=c(0.6,1.4),
  uhg_f=c(0.5,1.5) # lag time and retention in response.dat will both be multiplied by uhg_f
)
#initial estimates
starting.values=rbind(
  log_f_wind = log(1),
  f_lakemaxvol = 1,             
  log_gw_delay_f=log(1),
  f_direct_gw=1,
  log_kf_bedrock_f=log(c(1)),
  log_riverdepth_f=log(c(1)),
  kfcorr_a=c(1),
  intcf=c(0.3),
  log_ksat_factor=log(c(1)),
  n_f=c(1),
  cfr=(0), # additive!
  stomr_f=c(1),
  rootd_f=c(1),
  albedo_f=c(1),
  uhg_f=c(1)
)


set.seed(100)
#full call
res <- optim_pdds_robust(objective_function=calib_wasa,
                         # additional (i.e., + params) arguments to objective function
                         init_dir=paste(working_dir, "wasa_input", sep="/"),
                         period=as.Date(period),
                         modus="calibration",
                         rundir="/mnt/scratch/tpilz/wasa_calib_jaguaribe/tmp_calc",
                         keep_rundir=F,
                         # algorithm parameters
                         number_of_particles=31,
                         number_of_parameters=NROW(param_ranges),
                         parameter_bounds=param_ranges,
                         initial_estimates=starting.values,
                         lhc_init=TRUE,
                         part_xchange=3,
                         logfile="dds.log",
                         projectfile="dds.pro",
                         load_projectfile="try",
                         break_file="stop.pso",
                         nslaves=31,
                         max_wait_iterations=100,
                         max_number_function_calls=max_number_function_calls,
                         tryCall=T,
                         verbose = F,
                         execution_timeout=10
)
