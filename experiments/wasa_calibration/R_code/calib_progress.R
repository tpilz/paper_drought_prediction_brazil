# This file calib_progress.R is part of the analysis scripts for the
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



#WASA calibration using PSO / DDS
#display progress
#run best and uncalibrated parameter set


progress_calib <- function(workdir, init_dir, target_component, period, init_pars, outfiles = NULL) {
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(ggplot2)
  library(ppso)
  
  setwd(workdir)
  
  # plot
  #plot_optimization_progress(logfile = "dds.log", projectfile = "dds.pro", verbose = TRUE, cutoff_quantile = .9, progress_plot_filename="plot_progress.png", goodness_plot_filename="plot_goodness.png")
  #savePlot(filename = "dds_progress1.png", type = "png", device = dev.prev())
  #savePlot(filename = "dds_progress2.png", type = "png", device = dev.cur())
  
  # extract best parameter set
  dds_res = read.table("dds.log", sep="\t", header=TRUE)
  if(is.factor(dds_res$objective_function)) dds_res$objective_function=as.numeric(as.character(dds_res$objective_function), as.is=FALSE)
  best = which.min(dds_res$objective_function)[1]
  best_set = dds_res[best, !(names(dds_res) %in% c("time", "objective_function", "worker"))]
  best_set_v <- c(t(best_set))
  names(best_set_v) <- names(best_set)
  # replace log values by normal values for use in WASA (log is just a matter of sampling)
  log_trans = grepl(names(best_set_v), pattern = "^log_") #find log-transformed parameters
  best_set_v[log_trans]=exp(best_set_v[log_trans]) #transform back to non-log scale
  names(best_set_v) = sub(names(best_set_v), pattern = "^log_", rep="") #remove "log_" from name
  
  # run best parameter set
  obj_cal <- calib_wasa(best_set_v, init_dir, period, modus = "analysis", rundir = paste(workdir, "best_param_run", sep="/"), keep_rundir=T, outfiles = outfiles)

  # run with uncalibrated parameters
  obj_uncal <- calib_wasa(init_pars, init_dir, period, modus = "analysis", rundir = paste(workdir, "init_param_run", sep="/"), keep_rundir =T, outfiles = outfiles)
  
  # PLOT simulation results #
  # load observation data
  gauges_info <- read.table(paste(init_dir, "gauges_catchment_area.txt", sep="/"), header = T)
  dat_obs <- read.table(dir(paste(init_dir, "/Time_series", sep="/"), pattern = "_obs", full.names = T),
                        header=T, sep="\t")
  dat_obs <- dat_obs %>%
    mutate(date=as.Date(paste(YYYY, MM, DD, sep="-"))) %>%
    select(-YYYY, -MM, -DD, -HH) %>%
    melt(id.vars = "date", value.name = "value", variable.name ="gage") %>%
    filter(gage %in% gauges_info$GAUGE)
  # make sure, data are on continuous time scale (i.e. each day without gaps, change "days" below to whatever is appropiate)
  dat_obs_cont <- dat_obs %>%
    # make sure to have continuous time series (every day)
    group_by(gage) %>%
    complete(date = seq(period[1], period[2], by = "day")) %>%
    mutate(group = "obs") %>%
    ddply("gage", function(x) {
      x %>%
        mutate(subbas = gauges_info$SUBBAS_ID[which(gauges_info$GAUGE == unique(as.character(gage)))])
    }) %>%
    select(date, subbas, gage, value, group)
  # get simulation data
  wasa_dir <- c(paste(workdir, "best_param_run", "wasa_output", sep="/"),
                paste(workdir, "init_param_run", "wasa_output", sep="/"))
  if(target_component %in% c("res_vol", "res_inflow")) {
    dat_sim <- wasa_dir %>%
      lapply(function(s) {
        paste0("res_", gauges_info$SUBBAS_ID, "_watbal.out") %>%
          lapply(function(f) {
            dat <- read.table(paste(s, f, sep="/"), skip=1) %>%
              mutate(date = as.POSIXct(paste(V2, V3, sep="-"), format="%Y-%j", tz="UTC"),
                     target_component=target_component,
                     subbas = V1, gage = gauges_info$GAUGE[which(gauges_info$SUBBAS_ID == unique(V1))],
                     value = if_else(target_component == "res_vol",  V16/1e6, V6), group = head(tail(unlist(strsplit(s, "/")),2),1)) %>% # volume: hm3, inflow: m3/s
              select(date, subbas, gage, value, group)
          }) %>%
          rbind.fill()
      }) %>%
      rbind.fill()
  } else if(target_component == "River_Flow") {
    dat_sim <- wasa_dir %>%
      lapply(function(s) {
        dat <- read.table(paste(s, "River_Flow.out", sep="/"), skip=1, header=T, check.names=F) %>%
          select(year, day, one_of(as.character(gauges_info$SUBBAS_ID))) %>%
          melt(id.vars=c("year", "day"), variable.name="subbas") %>%
          mutate(date = as.POSIXct(paste(year, day, sep="-"), format="%Y-%j", tz="UTC"),
                 gage = gauges_info$GAUGE[which(gauges_info$SUBBAS_ID == unique(subbas))],
                 group = head(tail(unlist(strsplit(s, "/")),2),1)) %>%
          select(date, subbas, gage, value, group)
      }) %>%
      rbind.fill()
  } else if(target_component == "res_voldiff") {
    dat_sim <- wasa_dir %>%
      lapply(function(s) {
        paste0("res_", gauges_info$SUBBAS_ID, "_watbal.out") %>%
          lapply(function(f) {
            dat <- read.table(paste(s, f, sep="/"), skip=1) %>%
              mutate(date = as.Date(paste(V2, V3, sep="-"), format = "%Y-%j"),
                     target_component=target_component,
                     subbas = V1, gage = gauges_info$GAUGE[which(gauges_info$SUBBAS_ID == unique(V1))],
                     value = c(diff(V16), NA),
                     group = head(tail(unlist(strsplit(s, "/")),2),1)) %>%
              mutate(value = if_else(value > 0, value, 0)) %>%
              select(date, subbas, gage, value, group)
          }) %>%
          rbind.fill()
      }) %>%
      rbind.fill()
  } else 
    stop(paste0("Plot of 'target_component' ", target_component, " not yet supported!"))
  # merge sim and obs
  dat_all <- rbind(dat_obs_cont, dat_sim)
  
  # bench-NSE
  benchmark <- dat_all %>%
    filter(group == "obs") %>%
    mutate(date_grp = format(date, "%j")) %>%
    group_by(subbas, gage, date_grp) %>%
    summarise(value_bench = mean(value, na.rm=T))
    
  metrics <- left_join(dat_all %>%
                           dcast(date ~ group, value.var = "value") %>%
                           mutate(date_grp = format(date, "%j")),
                         benchmark %>%
                           ungroup() %>%
                           select(-subbas, -gage), # there should be only one factor of gage and subbas!
                         by = c("date_grp")) %>%
    mutate(nse_best_t1 = (best_param_run - obs)^2, nse_uncal_t1 = (init_param_run - obs)^2,
           nse_t2 = (obs-value_bench)^2) %>%
    summarise(nseb_uncal = 1 - sum(nse_uncal_t1, na.rm=T) / sum(nse_t2, na.rm=T), nseb_cal = 1 - sum(nse_best_t1, na.rm=T) / sum(nse_t2, na.rm=T),
              pbias_uncal = 100 * sum(init_param_run-obs, na.rm=T) / sum(obs, na.rm=T), pbias_cal = 100 * sum(best_param_run-obs, na.rm=T) / sum(obs, na.rm=T))
    
  
  
  # plot
  gp <- ggplot(dat_all, aes(x=date, y=value, group=group, colour=group)) +
    geom_line(size=0.1) +
    labs(caption = paste0("Benchmark NSE: best = ", round(metrics$nseb_cal, 2), ", uncal = ", round(metrics$nseb_uncal, 2),
                          "; Percent bias [%]: best = ", round(metrics$pbias_cal, 2), ", uncal = ", round(metrics$pbias_uncal, 2))) +
    scale_x_date("Time", date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y") +
    facet_grid(gage ~ ., scales="free_y")
  ggsave(paste0("init_vs_calib_", target_component, ".pdf"), plot = gp, width=15, height=5, units="in", dpi=300)
}
