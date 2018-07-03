# This file prep_res_strat_vol0.R is part of the experiment scripts for the
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

# Script creates various reservoir.dat files with varying initial conditions ("vol0") for different years, named
# 'reservoir_<year>.dat'. Initial conditions read from re-calculated reservoir volume time series based on reservoir
# level measurements obtained from FUNCEME. Iteration over various setups of sub-catchments.

library(plyr)
library(dplyr)
library(reshape2)

# requires subdirectories: <subcatchment>/Reservoir/reservoir.dat
dir_setup <- "../setup"

# start and end dates of time period
date_start <- as.Date("1980-01-01")
date_end <- as.Date("2014-06-30")

# sub-catchments (sub-dir names within dir_setup)
subcatch <- c("Banabuiu", "Oros", "Salgado", "Castanhao", "Jaguaribe")

# file with several IDs of strategic reservoirs (FUNCEME ID, subbas ID as in reservoir.dat, GRASS vector IDs)
file_ids <- "../../data/reservoir/param_update/reservoir_id_keys.dat"

# file with updated reservoir volumes (based on new CAV relations, obtained in 2017), ids refer to funceme ids
file_res_obs <- "../../data/reservoir/volume/calc_vol_1000m3_02_2018.dat"


### CALCULATIONS ###

# read IDs (should contain all strategic reservoirs of all sub-catchments)
dat_ids <- read.table(file_ids, header=T, sep="\t")

# find value of vol0 to be filled in for all reservoirs
dat_vol0_t <- read.table(file_res_obs, header=T, sep="\t", check.names = F, stringsAsFactors = F) %>%
  filter(grepl("-01-01$", date)) %>%
  melt(id.vars = "date", variable.name = "gage") %>%
  mutate(date = gsub("-01-01", "", date), gage = as.integer(as.character(gage)), value = replace(value, is.na(value), -999))
dat_vol0 <- left_join(dat_vol0_t, dat_ids, by=c("gage" = "res_funceme_id"))

# years object
yrs <- seq.Date(date_start, date_end, by="year")

# loop over sub-catchments
for(s in subcatch) {
  
  # get reservoir.dat
  dat_res_head <- readLines(paste(dir_setup, s, "Reservoir", "reservoir.dat", sep="/"), n=2)
  dat_res <- read.table(paste(dir_setup, s, "Reservoir", "reservoir.dat", sep="/"), skip=2, sep="\t")
  
  # merge with dat_vol0
  dat_res_cur <- inner_join(data.frame(res_cur=dat_res$V1), dat_vol0, by=c("res_cur" = "sub_id")) %>%
    select(res_cur, date, value) %>% 
    group_by(res_cur) %>%
    tidyr::complete(date = format(yrs, "%Y"), fill=list(value=-999))
  
  # create a reservoir.dat for each year
  ddply(dat_res_cur, "date", function(d) {
    # file name (incl path)
    file_res_out <- paste(dir_setup, s, "Reservoir", paste0("reservoir_", unique(d$date), ".dat"), sep="/")
    
    # write header
    writeLines(dat_res_head, file_res_out)
    
    # output data.frame
    dat_out <- left_join(dat_res %>%
                           select(-V4),
                         d %>%
                           select(-date) %>%
                           rename(V4 = value),
                         by=c("V1" = "res_cur"))
    dat_out <- dat_out[,paste0("V", seq(1, ncol(dat_out)))]
    
    # write data
    write.table(dat_out, file_res_out, append = T, quote = F, sep = "\t", row.names = F, col.names = F)
  })
}
