# This file cal_analysis.R is part of the analysis scripts for the
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




# This script analyses the WASA-SED calibration. It has to be applied
# directly after running calib_subcatch.R
# Needs to be run individually for each subcatchment.

working_dir = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/WASA_calib/dds/runs/gw_on/Banabuiu"
target_component = "res_voldiff" # res_inflow, res_vol, res_voldiff or River_Flow
code_dir = "/home/tobias/Promotion/Modellierung/Jaguaribe_new/WASA_calib/dds/R_code" # directory with R scripts (wrapper functions)

setwd(working_dir)

# external R file
source(paste(code_dir, "calib_progress.R", sep="/"))
source(paste(code_dir, "function_wrapper.R", sep="/"))

# calibration period
period = c("2003-01-01", "2010-12-31") 

# initial parameters used for calibration
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

# analyse calibration progress
progress_calib(workdir = working_dir,
               init_dir = paste(working_dir, "wasa_input", sep="/"),
               target_component = target_component,
               period = as.Date(period),
               init_pars = starting.values[,1],
               outfiles="River_Flow")
