
# This file analysis_hindcasts.R is part of the analysis scripts for the
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


# Script analyses hindcast results from the WASA-SED and a statistical model from
# Delgado et al. (2018), HESS. Several forecast verification metrics (RMSE, Brier
# Skill Score, ROC Skill Score) are calculated and plotted.

### INPUT ###------------------------------------------------------------------

# tidy R and visualisation
library(tidyverse)
library(zoo)
library(lubridate)
# ggplot stuff
library(scales) # colour scales
library(gtable) # add annotation outside plot region
library(grid)
library(gridExtra) # arrange sub-plots to one plot

# observed reservoir level time series
file_obs <- "../data/calc_vol_hm3_02_2018.dat"

# hydrol. model forecasts raw results; RData file
file_hydraw <- "../data/res_vol_hindcasts_1981-2014_Jan-Jun.RData"

# regression model index values
file_reg <- "../data/DI_all_input_plot_3_high_MDI_1_12_36_no_bias_corr.txt"

# regression model index values, forcing with bias correction
file_reg_biascor <- "../data/DI_all_input_plot_3_high_MDI_1_12_36_bias_corr.txt"

# wasa rainfall input file
file_prec <- "../data/rain_daily.dat"

# relations region/subcatchment vs. subbasins (incl. area)
file_reg_sub <- "../data/region_subbasin_relations.dat"

# reservoir meta data (region affiliation, storage capacity etc.)
file_res_meta <- "../data/res_meta_sub_02_2018.dat"


# output: attribute plot
file_out_atts <- "plots/attribute_plot.pdf"

# output: realiability plot with different number of bins
file_out_reliab_bins<- "plots/reliability_plot_bins.pdf"
# output: realiability plot with different drought thresholds
file_out_reliab_drought<- "plots/reliability_plot_drought.pdf"
# output: combination of two plots above
file_out_reliab_merged <- "plots/reliability_plot_bins_drought.pdf"

# output: rmse plot for observed forcing
file_out_rmse_perf <- "plots/accuracy_rmse_perf.pdf"
# output: rmse plot for different precipitation conditions
file_out_rmse_prec <- "plots/accuracy_rmse_median_prec_boxplots.pdf"
# output: rmse with increasing lead time
file_out_rmse_monthly <- "plots/accuracy_rmse_med_perf_monthly.pdf"
# output: rmse distribution for single reservoirs vs. regionally aggregated
file_out_rmse_single_res <- "plots/accuracy_rmse_perf_ens_single_res.pdf"

# output: rmse, bss, rocss
file_out_scores_mixed <- "plots/scores_mixed_region.pdf"
# output: rmse, bss, rocss comparison bias corrected vs. uncorrected forcing
file_out_scores_mixed_biascor <- "plots/scores_mixed_region_biascor.pdf"

# output: average regional storange changes
file_out_storage_change <- "plots/storage_changes.pdf"


### CALCULATIONS ###-----------------------------------------------------------

# data import #---------------------------------------------------------------- 

# reservoir meta data
dat_res_meta <- read.table(file_res_meta, header=T, sep="\t")

# observations
# read data, get value of last day of month and merge reservoir storage and regional affiliation
dat_obs_single <- read.table(file_obs, header = T, check.names = F, stringsAsFactors = F) %>%
  # tidy df
  gather(key = res_funceme_id, value = value, -date) %>%
  mutate(date = ymd(date)) %>%
  group_by(res_funceme_id) %>%
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  ungroup() %>%
  # get last value of the month (+/- five days if this is NA)
  mutate(year = year(date), month = month(date),
         res_funceme_id = as.integer(res_funceme_id),
         month_t = lag(month, 5), year_t = lag(year, 5)) %>%
  group_by(res_funceme_id, year_t, month_t) %>%
  filter(row_number() >= (n()-10)) %>%
  drop_na(value) %>%
  group_by(res_funceme_id, year_t, month_t) %>%
  summarise(value = value[first(which.min(
    # compare dates with last day of month and take the first occurrence of dates which are closest
    abs(difftime( ceiling_date(ymd(paste(unique(year_t), unique(month_t), "01", sep="-")), unit="months") - days(1), 
                  date, units="days"))
  ))]) %>%
  rename(month = month_t, year=year_t) %>%
  # get region and storage capacity
  left_join(.,
            dat_res_meta %>%
              select(id, capacity_hm_3, region),
            by = c("res_funceme_id" = "id")) %>%
  mutate(value_abs = value,
         value = value / capacity_hm_3 * 100,
         region = as.character(region)) %>%
  ungroup()

# regional aggregation of observed data + compute relative filling = drought index
dat_obs_reg <- dat_obs_single %>%
  group_by(region, year, month) %>%
  summarise(value = sum(value_abs),
            capacity_hm_3 = sum(capacity_hm_3)) %>%
  mutate(value = value / capacity_hm_3 * 100) %>%
  ungroup()

# load and sort hydmod hindcast data
load(file_hydraw)
dat_hydraw_single <- dat_all %>%
  # get last value of the month
  mutate(year = year(date), month = month(date)) %>%
  group_by(subcatch, subbas, res_funceme_id, realis, year, month) %>%
  summarise(value = last(vol)) %>%
  # join reservoir meta information (capacity)
  left_join(.,
            dat_res_meta %>%
              select(id, capacity_hm_3),
            by = c("res_funceme_id" = "id")) %>%
  # calculate drought index
  mutate(value_abs = value,
         value = value / capacity_hm_3 * 100) %>%
  # merge observations
  left_join(.,
            dat_obs_single %>%
              select(res_funceme_id, year, month, value) %>%
              rename(obs = value),
            by = c("res_funceme_id", "year", "month")) %>%
  rename(region = subcatch) %>%
  ungroup() %>%
  mutate(realis_t = suppressWarnings(as.integer(realis)),
         group = if_else(is.na(realis_t), realis, "ensemble"),
         realis = realis_t,
         realis = if_else(grepl("resamp_|biascor", group), as.integer(str_extract(group, "[0-9]+")), realis),
         group = gsub("_[0-9]+", "", group)) %>%
  select(-realis_t)

dat_hydraw_reg <- dat_hydraw_single %>%
  # calculate sums of storage and capacity over regions/subcatchments
  group_by(region, year, month, group, realis) %>%
  summarise(value = sum(value_abs), capacity = sum(capacity_hm_3)) %>%
  # calculate drought index: relative regional storage
  mutate(value = value / capacity *100) %>%
  # sort data
  mutate(model = "hydmod") %>%
  select(-capacity) %>%
  # add observations
  left_join(., rename(dat_obs_reg, obs = value), by = c("year", "month", "region")) %>%
  ungroup()

# read and select regression model results
# dat_reg <- read.table(file_reg, header = T, stringsAsFactors = F) %>%
#   select(-value_meas) %>%
#   filter( (grepl("eqm_echam_", model) | model == "measured") & variable == "res_perc_diff_on_meas") %>%
dat_reg <- read.table(file_reg, header = T, stringsAsFactors = F) %>% 
  rename(region = name, year = Y, month = M, forcing = model) %>%
  separate(forcing, into = c("ds", "gcm", "realis"), sep = "_", fill = "left") %>%
  mutate(model = "regression",
         realis_t = suppressWarnings(as.integer(realis)),
         group = if_else(is.na(realis_t), realis, "ensemble"),
         group = replace(group, group == "measured", "perfect"),
         realis = realis_t) %>%
  select(-variable, -realis_t, -ds, -gcm) %>%
  drop_na(value) %>%
  left_join(., rename(dat_obs_reg, obs = value), by = c("year", "month", "region"))

# read and select regression model results, forcing with bias correction
# dat_reg_biascor <- read.table(file_reg_biascor, header = T, stringsAsFactors = F) %>%
#   select(-value_meas) %>%
#   filter( grepl("eqm_echam_", model) & variable == "res_perc_diff_on_meas") %>%
dat_reg_biascor <- read.table(file_reg_biascor, header = T, stringsAsFactors = F) %>% 
  rename(region = name, year = Y, month = M, forcing = model) %>%
  separate(forcing, into = c("ds", "gcm", "realis"), sep = "_", fill = "left") %>%
  mutate(model = "regression",
         realis_t = suppressWarnings(as.integer(realis)),
         group = if_else(is.na(realis_t), realis, "biascor"),
         realis = realis_t) %>%
  select(-variable, -realis_t, -ds, -gcm) %>%
  drop_na(value) %>%
  left_join(., rename(dat_obs_reg, obs = value), by = c("year", "month", "region"))

# combine data
dat_all <- bind_rows(dat_hydraw_reg, dat_reg, dat_reg_biascor)

rm(dat_hydraw_reg, dat_reg, dat_reg_biascor)
gc(); gc()

# information about region vs. subbasin links
dat_reg_sub <- read.table(file_reg_sub, header=T, sep="\t")

# rainfall; calculate area-weighted catchment and regional sums for every timestep
dat_prec <- left_join(
  # read data and preoduce tidy df
  read.table(file_prec, header = T, skip=2, check.names = F, sep="\t")[,-2] %>%
    rename(date = "0") %>%
    mutate(date = dmy(date)) %>%
    gather(key = "subbas", value = "value", num_range(prefix = "", range = 1:10000)) %>%
    mutate(subbas = as.integer(subbas)),
  dat_reg_sub,
  by = c("subbas" = "sub")
)


# extend dataset by further calculations #-------------------------------------

# calculate ensemble median
dat_median_t <- dat_all %>%
  filter(grepl("ensemble|biascor", group)) %>%
  group_by(model, region, year, month, group) %>%
  summarise(value = median(value), obs = unique(obs)) %>%
  mutate(group = paste("median", group, sep="_"))
dat_all <- bind_rows(dat_all, dat_median_t)
rm(dat_median_t)

# ensemble median for single reservoirs
dat_median_t <- dat_hydraw_single %>%
  filter(grepl("ensemble|biascor", group)) %>%
  group_by(region, subbas, res_funceme_id, year, month, group) %>%
  summarise(value = median(value), obs = unique(obs)) %>%
  mutate(group = paste("median", group, sep="_"))
dat_hydraw_single <- bind_rows(dat_hydraw_single, dat_median_t)
rm(dat_median_t)


# calculation of predictors #--------------------------------------------------

# monthly precipitation per region (classified)
dat_prec_reg <- dat_prec %>%
  filter(date < ymd("2017-01-01")) %>%
  group_by(region, date) %>%
  mutate(weight = area / sum(area),
         value_t = value * weight) %>%
  summarise(value = sum(value_t)) %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(region, year, month) %>%
  summarise(value = sum(value)) %>%
  group_by(region, month) %>%
  mutate(value_class = cut(value, c(-Inf, quantile(value, c(.25,.75)), Inf), labels = c("dry", "normal", "wet"))) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character)

# wetness in terms of precipitation over different time horizons (months before actual month)
dat_wetness <- dat_prec_reg %>%
  group_by(region) %>%
  mutate(prec_prev36 = c(rep(NA, 35), zoo::rollapply(value, width = 36, FUN = sum)),
         prec_prev12 = c(rep(NA, 11), zoo::rollapply(value, width = 12, FUN = sum))) %>%
  select(region, year, month, prec_prev12, prec_prev36) %>%
  group_by(region, month) %>%
  mutate_at(vars(matches("prec_prev")), funs(cut(., c(-Inf, quantile(., c(.25, .75), na.rm = T), Inf),
                                                 labels = c("dry", "normal", "wet")))) %>%
  ungroup()

# combine predictors
dat_pred <- left_join(
  dat_prec_reg %>%
    select(-value) %>%
    rename(prec_prev1 = value_class),
  dat_wetness, by = c("region", "year", "month"))


# calc. drought occurrences #--------------------------------------------------

# calculate occurrences of drought and non drought
drought_thresh <- c(0.2, 0.3, 0.4) # drought threshold, i.e. index values falling below that quantile are droughts; VERY SENSITIVE PARAMETER!
# calculate index value based on the given threshold (individually for each region)
drought_index_thresh <- dat_obs_reg %>%
  crossing(., data.frame(drought_thresh = drought_thresh)) %>%
  group_by(region, drought_thresh) %>%
  summarise(drought_thresh_index = quantile(value, unique(drought_thresh), na.rm = T)) %>%
  ungroup()

# merge with data
dat_all <- left_join(dat_all, drought_index_thresh, by = "region")

# calculate simulated and observed droughts (1=drought, 0=no drought)
dat_all <- dat_all %>%
  mutate(drought_obs = if_else(obs < drought_thresh_index, 1, 0),
         drought_sim = if_else(value < drought_thresh_index, 1, 0))


# calc monthly rate of change of drought index #-------------------------------

dat_stor_change <- dat_all %>%
  # select vars
  select(model, group, realis, region, year, month, value) %>%
  distinct() %>%
  # include observations
  bind_rows(.,
            dat_obs_reg %>%
              filter(month %in% 1:6) %>%
              select(region, year, month, value) %>%
              mutate(model = "obs", group = "obs", realis = NA)) %>%
  # join initial values for every year = december observation of last year
  complete(month = 0:6, nesting(model, group, realis, region, year)) %>%
  left_join(.,
            dat_obs_reg %>%
              filter(month == 12) %>%
              select(region, year, value, -month) %>%
              mutate(year = year+1) %>%
              rename(init = value),
            by = c("region", "year")) %>%
  mutate(value = if_else(month == 0, init, value)) %>%
  select(-init) %>%
  # calculate monthly change
  group_by(model, group, realis, region, year) %>%
  mutate(value_change = c(NA,diff(value)))


# calc. contingency table #----------------------------------------------------

# first bin will be exactly zero and last bin exactly 1! E.g. for 10 % steps set to 12
# VERY SENSITIVE PARAMETER!
no_bins <- c(5, 7, 12)
dat_ctab_t <- dat_all %>%
  # only respect ensemble members (disregard perfect forcing etc.)
  filter(grepl("^ensemble|^biascor", group) & !is.na(drought_sim) & !is.na(drought_obs)) %>%
  # calculate forecast probabilities for each year x month x region x model combination
  group_by(model, region, year, month, group, drought_thresh) %>%
  summarise(forecast_prob = sum(drought_sim)/n(), drought_obs = unique(drought_obs)) %>%
  # create given number of bins and sort forecast probabilities into these bins
  crossing(., data.frame(group_bins = no_bins)) %>%
  group_by(model, region, year, month, group, drought_thresh, group_bins) %>%
  mutate(bin = as.integer(cut(forecast_prob, breaks=c(-1, seq(0,0.99,by=1/(unique(group_bins)-2)), 0.999, 1)))-1) %>%
  ungroup()
# join predictors
dat_ctab_pred <- left_join(dat_ctab_t %>%
                             mutate(month = match(month, unique(dat_all$month))),
                           dat_pred %>%
                             mutate_if(is.factor, as.character) %>%
                             gather(key = predictor, value = value_pred, -year, -region, -month) %>%
                             drop_na(),
                           by=c("region", "year", "month")) %>%
  ungroup() %>%
  # single reservoir as predictor
  mutate(predictor = if_else(grepl("Reservoir_", region), "single_reservoir", predictor),
         value_pred = if_else(grepl("Reservoir_", region), str_split_fixed(region, "_", n=2)[,2], value_pred)) %>%
  drop_na(predictor, value_pred)

# derive number of observed (non-)occurrences aggregated over various grouping variables
dat_ctab <- bind_rows(dat_ctab_pred %>% # model x region x predictor value
                        group_by(drought_thresh, group_bins, group, model, region, predictor, value_pred, bin) %>%
                        summarise(o = sum(drought_obs), no = sum(abs(drought_obs-1))) %>%
                        ungroup() %>%
                        complete(bin, nesting(drought_thresh, group_bins, group, model, region, predictor, value_pred), fill=list(o = 0, no = 0)),
                      dat_ctab_pred %>% # model x predictor value (all regions)
                        filter(predictor != "single_reservoir") %>% # exclude single reservoirs from "all" regions
                        group_by(drought_thresh, group_bins, group, model, predictor, value_pred, bin) %>%
                        summarise(o = sum(drought_obs), no = sum(abs(drought_obs-1))) %>%
                        ungroup() %>%
                        complete(bin, nesting(drought_thresh, group_bins, group, model, predictor, value_pred), fill=list(o = 0, no = 0)) %>%
                        mutate(region = "all"),
                      dat_ctab_pred %>% # model x region x month (added as predictor lead_time)
                        distinct(drought_thresh, group_bins, group, model, region, year, month, forecast_prob, drought_obs, bin) %>%
                        group_by(drought_thresh, group_bins, group, model, region, month, bin) %>%
                        summarise(o = sum(drought_obs), no = sum(abs(drought_obs-1))) %>%
                        ungroup() %>%
                        complete(bin, nesting(drought_thresh, group_bins, group, model, region, month), fill=list(o = 0, no = 0)) %>%
                        rename(lead_time = month) %>%
                        gather(key = predictor, value = value_pred, lead_time) %>%
                        mutate(value_pred = as.character(value_pred)),
                      dat_ctab_pred %>% # model x month (added as predictor lead_time) over all regions
                        filter(predictor != "single_reservoir") %>%  # exclude single reservoirs from "all" regions
                        distinct(drought_thresh, group_bins, group, model, region, year, month, forecast_prob, drought_obs, bin) %>%
                        group_by(drought_thresh, group_bins, group, model, month, bin) %>%
                        summarise(o = sum(drought_obs), no = sum(abs(drought_obs-1))) %>%
                        ungroup() %>%
                        complete(bin, nesting(drought_thresh, group_bins, group, model, month), fill=list(o = 0, no = 0)) %>%
                        rename(lead_time = month) %>%
                        gather(key = predictor, value = value_pred, lead_time) %>%
                        mutate(region = "all", value_pred = as.character(value_pred)),
                      dat_ctab_pred %>% # model x region over all months
                        filter(predictor != "single_reservoir") %>%  # exclude single reservoirs from "all" regions
                        distinct(drought_thresh, group_bins, group, model, region, year, month, forecast_prob, drought_obs, bin) %>%
                        group_by(drought_thresh, group_bins, group, model, region, bin) %>%
                        summarise(o = sum(drought_obs), no = sum(abs(drought_obs-1))) %>%
                        ungroup() %>%
                        complete(bin, nesting(drought_thresh, group_bins, group, model, region), fill=list(o = 0, no = 0)) %>%
                        mutate(predictor = "lead_time", value_pred = "all"),
                      dat_ctab_pred %>% # model over all regions and months
                        filter(predictor != "single_reservoir") %>%  # exclude single reservoirs from "all" regions
                        distinct(drought_thresh, group_bins, group, model, region, year, month, forecast_prob, drought_obs, bin) %>%
                        group_by(drought_thresh, group_bins, group, model, bin) %>%
                        summarise(o = sum(drought_obs), no = sum(abs(drought_obs-1))) %>%
                        ungroup() %>%
                        complete(bin, nesting(drought_thresh, group_bins, group, model), fill=list(o = 0, no = 0)) %>%
                        mutate(region = "all", predictor = "lead_time", value_pred = "all")) %>%
  # calculate o and no sums for each model x region x predictor combination
  left_join(.,
            group_by(., drought_thresh, group_bins, group, model, region, predictor, value_pred) %>%
              summarise(o_total = sum(o), no_total = sum(no)),
            by = c("drought_thresh", "group_bins", "group", "model", "region", "predictor", "value_pred"))


# calc. roc #------------------------------------------------------------------

# calculate hit rate and false alarm rate for each bin
dat_hit_alarm <- lapply(no_bins, function(j) {
  lapply(0:(j-1), function(i) {
    dat_ctab %>%
      filter(group_bins == j & between(bin, i, j)) %>%
      group_by(drought_thresh, group_bins, group, model, region, predictor, value_pred) %>%
      summarise(hr = sum(o) / unique(o_total), far = sum(no) / unique(no_total)) %>%
      mutate(bin = i)
  }) %>%
    bind_rows()
}) %>%
  bind_rows() %>%
  # by definition, roc curve must originate in (0,0) for cases > 100 % (which in reality never occur)
  group_by(drought_thresh, group_bins) %>%
  complete(bin=0:(unique(group_bins)), nesting(drought_thresh, group_bins, group, model, region, predictor, value_pred), fill = list(hr=0, far=0)) %>%
  # arrange data (hr and far in strictly increasing, i.e. bin in strictly decreasing order)
  group_by(drought_thresh, group_bins, group, model, region, predictor, value_pred) %>%
  arrange(desc(bin), .by_group = T)

# calculate roc skill score via area under curve
dat_roc <- dat_hit_alarm %>%
  group_by(drought_thresh, group_bins, group, model, region, predictor, value_pred) %>%
  summarise(auc = sum(diff(far) * rollmean(hr, 2))) %>%
  mutate(rocss = 2*auc - 1)


# brier score #----------------------------------------------------------------

# observed occurrence of droughts per region
dat_droughts_obs <- dat_all %>%
  filter(!is.na(obs)) %>%
  group_by(drought_thresh, year, month, region) %>%
  distinct(drought_obs) %>%
  group_by(drought_thresh, region) %>%
  summarise(value = sum(drought_obs) / n()) %>%
  bind_rows(.,
            group_by(., drought_thresh) %>%
              filter(!grepl("Reservoir_", region)) %>%
              summarise(value = mean(value)) %>%
              mutate(region = "all"))

# Brier score without decomposition (use forecast probabilities directly)
# NOTE: this version of calculation is NOT affected by number of bins, in contrast to version below
dat_bs_t <- left_join(dat_ctab_t %>%
                        ungroup(),
                      dat_droughts_obs %>%
                        rename(climatology = value),
                      by = c("drought_thresh", "region"))
dat_bs <- bind_rows(group_by(dat_bs_t, drought_thresh, group_bins, group, model, region) %>%
                      summarise(bs = mean((forecast_prob-drought_obs)^2),
                                bs_clim = mean((unique(climatology)-drought_obs)^2),
                                bss = 1 - (bs / bs_clim)) %>%
                      ungroup(),
                    group_by(dat_bs_t, drought_thresh, group_bins, group, model) %>%
                      filter(!grepl("Reservoir_", region)) %>%
                      summarise(bs = sum((forecast_prob-drought_obs)^2)/n(),
                                bs_clim = mean((mean(unique(climatology))-drought_obs)^2),
                                bss = 1 - (bs / bs_clim)) %>%
                      ungroup() %>%
                      mutate(region = "all"))

# Brier score with uncertainty, reliability, and resolution (includes binning of forecast probabilities)
# notation similar to Wilks, 2006, chap. 7.4.3
# NOT USED IN PAPER, only kept for demonstration purposes!
dat_bs_tab <- dat_ctab %>%
  # for monthly values there are too many cases where Ni is zero -> score cannot be calculated
  filter( (predictor == "lead_time" & value_pred == "all") | predictor == "single_reservoir") %>%
  group_by(drought_thresh, group_bins) %>%
  # number of values for each bin; central probability value of forecast bins
  mutate(Ni = o + no, yi = if_else(bin == 0, 0, if_else(bin == unique(group_bins)-1, 1, (bin-0.5)/(unique(group_bins)-2)))) %>% # bin/(no_bins-1)) %>% # 
  # there might be NaNs due to limited number of values for some bins -> o and no are zero -> NaN produced -> just remove lines
  filter(Ni != 0) %>%
  left_join(.,
            group_by(., drought_thresh, group_bins, group, model, region, predictor) %>%
              # eq. 7.36
              summarise(n = sum(Ni)),
            by = c("drought_thresh", "group_bins", "group", "model", "region", "predictor")) %>%
  # eq. 7.37 = marginal forecast distribution, 7.38 = conditional frequency = frequency of occurrences per bin
  mutate(pyi = Ni /n, o_mean_i = o / Ni) %>%
  left_join(.,
            group_by(., drought_thresh, group_bins, group, model, region, predictor) %>%
              # eq. 7.39, unconditional frequency = sample climatology = observed drought frequency
              summarise(o_mean = sum(Ni * o_mean_i) / unique(n),
                        # average forecasting value
                        y_mean = sum(Ni * yi) / unique(n)),
            by = c("drought_thresh", "group_bins", "group", "model", "region", "predictor")) %>%
  ungroup() %>%
  mutate(region = factor(region, c("all", "Jaguaribe", "Oros", "Salgado", "Castanhao", "Banabuiu"),
                         labels = c("Combined", "Lower\n Jaguaribe", "Orós", "Salgado", "Castanhão", "Banabuiú")),
         model = factor(model, c("hydmod", "regression"), labels = c("Process-based", "Statistical")))

dat_bs_decomp <- dat_bs_tab %>%
  group_by(drought_thresh, group_bins, group, model, region, predictor) %>%
  # eq. 7.40, term reliability, calibration / conditional bias, optimal value: small as possible
  summarise(reliability = sum(Ni * (yi-o_mean_i)^2) / unique(n),
            # eq. 7.40, term resolution, deviation from climatology should be large to indicate distinction between (non-)events, optimal value: large as possible
            resolution = sum(Ni * (o_mean_i-o_mean)^2) / unique(n),
            # eq. 7.40, term uncertainty, solely depends on variability of observations
            uncertainty = unique(o_mean) * (1-unique(o_mean)) ) %>%
  # eq. 7.40, Brier score via decomposition approach
  mutate(bs = reliability - resolution + uncertainty, bss = (resolution - reliability) / uncertainty,
         rel_score = reliability/uncertainty, res_score = resolution / uncertainty)


# rmse #-----------------------------------------------------------------------

dat_rmse_t <- left_join(dat_all %>%
                          filter(!is.na(value) & !is.na(obs)) %>%
                          distinct(region, year, month, group, realis, model, value, obs) %>%
                          unite(group, realis, col="group", sep="_") %>%
                          mutate(group = gsub("_NA", "", group)) %>%
                          select(group, model, region, year, month, value, obs),
                        #mutate(month = match(month, unique(dat_all$month))),
                        dat_pred %>%
                          mutate_if(is.factor, as.character) %>%
                          gather(key = predictor, value = value_pred, -year, -region, -month) %>%
                          drop_na(),
                        by = c("region", "year", "month")) %>%
  bind_rows(.,
            dat_hydraw_single %>%
              filter(!is.na(value) & !is.na(obs)) %>%
              unite(group, realis, col="group", sep="_") %>%
              mutate(group = gsub("_NA", "", group),
                     predictor = "single_reservoir", value_pred = as.character(res_funceme_id),
                     model = "hydmod") %>%
              select(group, model, region, year, month, value, obs, predictor, value_pred)) %>%
  # single reservoir as predictor
  mutate(predictor = if_else(grepl("Reservoir_", region), "single_reservoir", predictor),
         value_pred = if_else(grepl("Reservoir_", region), str_split_fixed(region, "_", n=2)[,2], value_pred)) %>%
  drop_na(predictor, value_pred)

# rmse in dependence of all predictors per model x region (and all regions) combinations
dat_rmse <- bind_rows(# ens. member X model X region X single res. / wetness cond.
                      dat_rmse_t %>%
                        group_by(group, model, region, predictor, value_pred) %>%
                        summarise(rmse = sqrt(mean((value-obs)^2))),
                      # ens. member X model X wetness cond.
                      dat_rmse_t %>%
                        filter(predictor != "single_reservoir") %>%
                        group_by(group, model, predictor, value_pred) %>%
                        summarise(rmse = sqrt(mean((value-obs)^2))) %>%
                        mutate(region = "all"),
                      # ens. member X model X region X month
                      dat_rmse_t %>%
                        filter(predictor != "single_reservoir") %>%
                        group_by(group, model, region, month) %>%
                        distinct(group, model, region, year, month, value, obs) %>%
                        summarise(rmse = sqrt(mean((value-obs)^2))) %>%
                        rename(lead_time = month) %>%
                        gather(key = predictor, value = value_pred, lead_time) %>%
                        mutate(value_pred = as.character(value_pred)),
                      # ens. member X model X month
                      dat_rmse_t %>%
                        filter(predictor != "single_reservoir") %>%
                        group_by(group, model, month) %>%
                        distinct(group, model, region, year, month, value, obs) %>%
                        summarise(rmse = sqrt(mean((value-obs)^2))) %>%
                        rename(lead_time = month) %>%
                        gather(key = predictor, value = value_pred, lead_time) %>%
                        mutate(region = "all", value_pred = as.character(value_pred)),
                      # ens. member X model X region
                      dat_rmse_t %>%
                        filter(predictor != "single_reservoir") %>%
                        group_by(group, model, region, year, month, predictor) %>%
                        distinct(group, model, region, year, month, value, obs) %>%
                        group_by(group, model, region) %>%
                        summarise(rmse = sqrt(mean((value-obs)^2))) %>%
                        mutate(lead_time = "all") %>%
                        gather(key = predictor, value = value_pred, lead_time) %>%
                        mutate(value_pred = as.character(value_pred)),
                      # ens. member X model
                      dat_rmse_t %>%
                        filter(predictor != "single_reservoir") %>%
                        group_by(group, model, region, year, month, predictor) %>%
                        distinct(group, model, region, year, month, value, obs) %>%
                        group_by(group, model) %>%
                        summarise(rmse = sqrt(mean((value-obs)^2))) %>%
                        mutate(lead_time = "all") %>%
                        gather(key = predictor, value = value_pred, lead_time) %>%
                        mutate(region = "all", value_pred = as.character(value_pred))
) %>%
  ungroup() %>%
  mutate(region = factor(region, c("all", "Jaguaribe", "Oros", "Salgado", "Castanhao", "Banabuiu"),
                         labels = c("Combined", "Lower\n Jaguaribe", "Orós", "Salgado", "Castanhão", "Banabuiú")),
         model = factor(model, c("hydmod", "regression"), labels = c("Process-based", "Statistical")))



### GRAPHICS ###---------------------------------------------------------------

# attribute plots #------------------------------------------------------------

# hindcasts
dat_plot_t <- subset(dat_bs_tab, region=="Combined" & group != "biascor" & drought_thresh == 0.3 & group_bins == 7)
gp <- ggplot(subset(dat_plot_t, group == "ensemble"),
             aes(x=yi, y=o_mean_i, colour = model)) +
  # area positively contributing to Brier skill score
  geom_area(data = subset(dat_plot_t, group == "ensemble"), aes(x = seq(0, first(o_mean), length.out = length(o_mean)),
                                                                y = seq(first(o_mean)/2, first(o_mean), length.out = length(o_mean))),
            colour = "gray", alpha=0.1) +
  geom_ribbon(data = subset(dat_plot_t, group == "ensemble"), aes(x = seq(first(o_mean), 1, length.out = length(o_mean)),
                                                                  ymax=1, ymin = seq(first(o_mean), (1+first(o_mean))/2, length.out = length(o_mean))),
              colour = "gray", alpha=0.1) +
  # plot poin-lines for both models
  geom_point(size=3) +
  geom_line(aes(linetype = "Observed drought occurrence"), size=1.2) +
  #geom_line(size=1.2) +
  # frequency of forecast probabilities
  geom_point(aes(y=pyi, group = model), size=3) +
  geom_line(aes(y=pyi, linetype = "Forecast probability bin"), size=1.2) +
  #geom_line(aes(y=pyi, group=model), size=1.2) +
  # Climatology / no resolution lines
  geom_hline(aes(yintercept = o_mean), linetype="dashed") +
  geom_vline(aes(xintercept = o_mean), linetype="dashed") +
  annotate("text", x = 0.75, y = 0.32, label = "Climatology / no resolution", size =7) +
  # perfect reliability line
  geom_abline(slope = 1, intercept = 0, colour = "gray") +
  # no skill line
  geom_abline(aes(slope = 1/2 , intercept = o_mean/2)) +
  annotate("text", x = 0.75, y = 0.55, label = "No skill", size =7, angle=27) +
  # adjust appearance (colour, linetypes, labels, etc.)
  scale_colour_manual(values = hue_pal()(2), name ="Model: ") +
  scale_linetype_manual(values = c("Observed drought occurrence" = "solid", 
                                   "Forecast probability bin" = "dotted"), name = "Relative frequency: ") +
  labs(x = "Forecast probability", y = "Relative frequency") +
  scale_x_continuous(breaks = seq(0,1,1/(7-2)), limits = c(0,1)) +
  scale_y_continuous(breaks = seq(0,1,1/(7-2)), limits = c(0,1)) +
  coord_fixed(expand = F) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", legend.box = "vertical")
ggsave(file_out_atts, height = 10.5, width = 10)


# no of bins + drought threshold parameter sensitivity
dat_plot_t <- subset(dat_bs_tab, region=="Combined" & group != "biascor" & drought_thresh == 0.3) %>%
  mutate(group_bins = factor(group_bins))
#dat_bs_lab <- subset(dat_bs, region=="all" & group != "biascor" & drought_thresh == 0.3)
gp_bins <- ggplot(dat_plot_t,
                  aes(x=yi, y=o_mean_i, colour = model)) +
  # plot poin-lines for both models
  geom_point(aes(group=interaction(group_bins, model), shape=group_bins), size=3) +
  geom_line(aes(group=interaction(group_bins, model), linetype=group_bins), size=1.2) +
  # perfect reliability line
  geom_abline(slope = 1, intercept = 0, colour = "gray") +
  # # labels (BSS values)
  # geom_label(data = dat_bs_lab, mapping = aes(label = round(bss,2)),
  #            x = rep(c(0.7, 0.9), 3), y = c(0.5, 0.5, 0.3, 0.3, 0.1, 0.1), fill=rep(hue_pal()(2), 3),
  #            size=7, inherit.aes = F) +
  # adjust appearance (colour, linetypes, labels, etc.)
  scale_colour_manual(values = hue_pal()(2), name ="Model: ") +
  scale_linetype_manual(values = c("5" = "dotted", "12" = "dashed", "7" = "solid"), name = "Number of bins: ") +
  scale_shape_manual(values = c("5" = 15, "12" = 16, "7" = 17), guide = 'none') +
  labs(x = "Forecast probability", y = "Observed drought occurrence") +
  scale_x_continuous(breaks = seq(0,1,1/(7-2)), limits = c(0,1)) +
  scale_y_continuous(breaks = seq(0,1,1/(7-2)), limits = c(0,1)) +
  coord_fixed(expand = F) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.key.width = unit(40, "pt"),
        plot.margin = unit(c(10,0,10,10), "pt"))
ggsave(file_out_reliab_bins, gp_bins, height = 10.5, width = 10)

dat_plot_t <- subset(dat_bs_tab, region=="Combined" & group != "biascor" & group_bins == 7) %>%
  mutate(drought_thresh = factor(drought_thresh))
gp_drought <- ggplot(dat_plot_t,
                     aes(x=yi, y=o_mean_i, colour = model)) +
  # plot poin-lines for both models
  geom_point(aes(group=interaction(drought_thresh, model), shape=drought_thresh), size=3) +
  geom_line(aes(group=interaction(drought_thresh, model), linetype=drought_thresh), size=1.2) +
  # perfect reliability line
  geom_abline(slope = 1, intercept = 0, colour = "gray") +
  # adjust appearance (colour, linetypes, labels, etc.)
  scale_colour_manual(values = hue_pal()(2), name ="Model: ") +
  scale_linetype_manual(values = c("0.2" = "dotted", "0.4" = "dashed", "0.3" = "solid"), name = "Drought threshold: ") +
  scale_shape_manual(values = c("0.2" = 15, "0.4" = 16, "0.3" = 17), guide = 'none') +
  labs(x = "Forecast probability", y = "Observed drought occurrence") +
  scale_x_continuous(breaks = seq(0,1,1/(7-2)), limits = c(0,1)) +
  scale_y_continuous(breaks = seq(0,1,1/(7-2)), limits = c(0,1)) +
  coord_fixed(expand = F) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.key.width = unit(40, "pt"),
        plot.margin = unit(c(10,0,10,10), "pt"))
ggsave(file_out_reliab_drought, gp_drought, height = 10.5, width = 10)

# merge drouht and bin plot
gpgr_drought <- ggplotGrob(gp_drought)
annot <- textGrob(label = "(a)", just=c("left", "top"), x=0, y=0, gp = gpar(cex = 2))
gpgr_drought <- gtable_add_grob(gpgr_drought, annot, t=1, l=1, r=1, clip = "off")

gpgr_bins <- ggplotGrob(gp_bins)
annot <- textGrob(label = "(b)", just=c("left", "top"), x=0, y=0, gp = gpar(cex = 2))
gpgr_bins <- gtable_add_grob(gpgr_bins, annot, t=1, l=1, r=1, clip = "off")

pdf(file_out_reliab_merged, width=15, height=8)
grid.arrange(gpgr_drought, gpgr_bins, ncol=2, nrow=1)
dev.off()


# rmse plots #-----------------------------------------------------------------

# observed forcing
dat_plot_t <- dat_rmse %>%
  subset(group == "perfect" & predictor == "lead_time" & value_pred == "all")
gp <- ggplot(dat_plot_t, aes(y = rmse, x = region, group = model, fill=model)) +
  geom_bar(position = "dodge", stat = "identity", width=0.70, colour="black") +
  geom_vline(xintercept = 1.5, linetype = "dashed", size=1.5) +
  labs(x = "Region", y = "RMSE (percent point)", fill = "Model: ") +
  lims(y=c(0,50)) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom")
ggsave(file_out_rmse_perf, width = 10, height = 8)

# precipitation, wetness conditions
dat_plot_t <- dat_rmse %>%
  subset(grepl("prec_prev", predictor) & region == "Combined" & !grepl("resamp|biascor", group)) %>%
  ungroup() %>%
  mutate(predictor = gsub("prec_prev", "P_", predictor),
         predictor = gsub("_", "['", paste0(predictor, "']")),
         group = gsub("^median_ensemble", "Hindcast", group),
         group = replace(group, group == "perfect", "Simulation"))
gp <- ggplot(dat_plot_t,
             aes(y = rmse, x = value_pred, group = interaction(group, model),
                 colour=model, linetype = group, shape = group)) +
  geom_boxplot(data = subset(dat_plot_t, grepl("^ensemble", group)),
               mapping = aes(x = value_pred, y = rmse, group = interaction(value_pred, model), fill = model),
               inherit.aes=F, show.legend = F, outlier.colour="gray") +
  geom_point(data = subset(dat_plot_t, !grepl("^ensemble", group)), size=5) +
  geom_line(data = subset(dat_plot_t, !grepl("^ensemble", group)), size=1.2) +
  labs(x = "", y = "RMSE (percent point)", colour = "Model: ", linetype = "Mode: ", shape = "Mode: ") +
  lims(y=c(0,50)) +
  facet_grid(. ~ predictor, scales = "free_x", labeller = label_parsed) +
  theme_bw(base_size = 22) +
  theme(legend.key.width = unit(50, "pt"))
ggsave(file_out_rmse_prec, height = 8, width = 18)

# monthly
dat_plot_t <- dat_rmse %>%
  filter(predictor == "lead_time" & region == "Combined", !grepl("resamp|biascor", group)) %>%
  mutate(value_pred = suppressWarnings(as.integer(as.character(value_pred)))) %>% # ignore NA warning!
  drop_na(value_pred) %>%
  mutate(value_pred = month(value_pred, label = T, locale = "en_GB.UTF-8"),
         group = gsub("^median_ensemble", "Hindcast", group),
         group = replace(group, group == "perfect", "Simulation"))
gp <- ggplot(dat_plot_t,
             aes(y = rmse, x = value_pred, group = interaction(group,model),
                 colour=model, linetype = group, shape = group)) +
  geom_boxplot(data = subset(dat_plot_t, grepl("^ensemble", group)),
               mapping = aes(x = value_pred, y = rmse, group = interaction(value_pred, model), fill = model),
               inherit.aes=F, show.legend = F, outlier.colour="gray") +
  geom_point(data = subset(dat_plot_t, !grepl("^ensemble", group)), size = 5) +
  geom_line(data = subset(dat_plot_t, !grepl("^ensemble", group)), size = 1.2) +
  lims(y = c(0,50)) +
  labs(x = "Aggregation month", y = "RMSE (percent point)", colour = "Model: ", linetype = "Mode: ", shape = "Mode: ") +
  theme_bw(base_size = 22) +
  theme(legend.key.width = unit(50, "pt"))
ggsave(file_out_rmse_monthly, height = 8, width = 18)

# single reservoirs
dat_plot_t <- dat_rmse %>%
  subset(grepl("single_reservoir", predictor) & !grepl("biascor", group)) %>%
  bind_rows(.,
            dat_rmse %>%
              filter(model == "Process-based" & region != "Combined" & !grepl("biascor", group) &
                       predictor == "lead_time" & value_pred == "all")) %>%
  separate(col=group, into=c("group", "realis"), sep="_", fill="right") %>%
  filter(grepl("^ensemble|perfect", group)) %>%
  mutate(group = replace(group, group == "ensemble", "Hindcasts"),
         group = replace(group, group == "perfect", "Observed"),
         predictor = replace(predictor, predictor == "lead_time", "Regional"),
         predictor = replace(predictor, predictor == "single_reservoir", "Individual reservoir"))
gp <- ggplot(dat_plot_t, aes(y=rmse, x=group, group=interaction(group, predictor), fill = predictor)) +
  geom_boxplot(position=position_dodge(0.8)) +
  expand_limits(y=0) +
  scale_fill_manual(values = c("Individual reservoir" = hue_pal()(4)[3], "Regional" = hue_pal()(4)[4])) +
  labs(x = "Forcing", y = "RMSE (percent point)", fill = "Aggregation level: ") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom")
ggsave(file_out_rmse_single_res, width = 9, height = 9)


# mixed scores #---------------------------------------------------------------

# prepare data
dat_plot_t <- bind_rows(
  # get RMSE hindcasts
  subset(dat_rmse, predictor == "lead_time" & value_pred == "all" & grepl("median_", group)) %>%
    rename(value = rmse) %>%
    mutate(score = "RMSE", group = gsub("[a-z]+_biascor", "Hindcasts_biascor", group),
           group = gsub("[a-z]+_ensemble", "Hindcasts", group)) %>%
    select(model, region, score, value, group),
  # get RMSE with observed forcing
  subset(dat_rmse, predictor == "lead_time" & value_pred == "all" & group == "perfect") %>%
    rename(value = rmse) %>%
    mutate(score = "RMSE", group = "Perfect") %>%
    select(model, region, score, value, group),
  # get ROCSS
  subset(dat_roc, group_bins == 7 & drought_thresh == 0.3 & predictor == "lead_time" & value_pred == "all") %>%
    ungroup() %>%
    rename(value = rocss) %>%
    mutate(score = "ROCSS", group = gsub("biascor", "Hindcasts_biascor", group),
           group = gsub("ensemble", "Hindcasts", group)) %>%
    select(model, region, score, value, group) %>%
    mutate(region = factor(region, c("all", "Jaguaribe", "Oros", "Salgado", "Castanhao", "Banabuiu"),
                           labels = c("Combined", "Lower\n Jaguaribe", "Orós", "Salgado", "Castanhão", "Banabuiú")),
           model = factor(model, c("hydmod", "regression"), labels = c("Process-based", "Statistical"))),
  # get BSS
  dat_bs %>%
    ungroup() %>%
    filter(!grepl("Reservoir_", region) & group_bins == 7 & drought_thresh == 0.3) %>%
    select(group, model, region, bss) %>%
    gather(key = score, value = score_val, bss) %>%
    ungroup() %>%
    mutate(region = factor(region, c("all", "Jaguaribe", "Oros", "Salgado", "Castanhao", "Banabuiu"),
                           labels = c("Combined", "Lower\n Jaguaribe", "Orós", "Salgado", "Castanhão", "Banabuiú")),
           model = factor(model, c("hydmod", "regression"), labels = c("Process-based", "Statistical"))) %>%
    rename(value = score_val) %>%
    select(model, region, value, group, -score) %>%
    mutate(score = "BSS", group = gsub("biascor", "Hindcasts_biascor", group),
           group = gsub("ensemble", "Hindcasts", group))) %>%
  mutate(score = factor(score, c("RMSE", "ROCSS", "BSS"), labels = c("RMSE (percent point)", "ROCSS (-)", "BSS (-)")))

# without bias correction
gp <- ggplot(subset(dat_plot_t, !grepl("Perfect|biascor", group)), 
             aes(x = region, y = value, group = model, fill = model)) +
  # plot RMSE, ROCSS, BSS
  geom_bar(position = "dodge", stat = "identity", width=0.70, colour = "black") +
  geom_vline(xintercept = 1.5, linetype = "dashed", size=1.5) +
  # plot RMSE with 'perfect' forcing as errorbar in bars; NEEDS MANUAL ADJUSTMENT IF ADJUSTING DATASET!
  geom_errorbarh(data = subset(dat_plot_t, group == "Perfect"),
                 mapping = aes(xmin=if_else(model == "Process-based", as.numeric(region)-0.35, as.numeric(region)), 
                               xmax=if_else(model == "Process-based", as.numeric(region), as.numeric(region)+0.35)),
                 linetype = "dashed") +
  # workaround to define axes limits for facets (as far as I know there is no simpler solution)
  geom_blank(data=data.frame(region="Combined", value=c(0,50, 0,1, 0,1),
                             score=c("RMSE (percent point)", "RMSE (percent point)", "ROCSS (-)", "ROCSS (-)", "BSS (-)", "BSS (-)"),
                             model="Process-based", group = "Hindcasts")) +
  labs(x = "Region", y = "", fill = "Model: ") +
  facet_grid(score ~ ., scales = "free_y") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom")
ggsave(file_out_scores_mixed, height=10, width = 10)

# with bias correction
gp <- ggplot(subset(dat_plot_t, group != "Perfect"), 
             aes(x = region, y = value, group = interaction(group, model), linetype = group, fill = model)) +
  # plot RMSE, ROCSS, BSS
  geom_bar(position = "dodge", stat = "identity", width=0.70, colour = "black") +
  geom_vline(xintercept = 1.5, linetype = "dashed", size=1.5) +
  # plot RMSE with 'perfect' forcing as errorbar in bars; NEEDS MANUAL ADJUSTMENT IF ADJUSTING DATASET!
  geom_errorbarh(data = subset(dat_plot_t, group == "Perfect"),
                 mapping = aes(xmin=if_else(model == "Process-based", as.numeric(region)-0.35, as.numeric(region)), 
                               xmax=if_else(model == "Process-based", as.numeric(region), as.numeric(region)+0.35)),
                 linetype = "dashed") +
  # workaround to define axes limits for facets (as far as I know there is no simpler solution)
  geom_blank(data=data.frame(region="Combined", value=c(0,50, 0,1, 0,1),
                             score=c("RMSE (percent point)", "RMSE (percent point)", "ROCSS (-)", "ROCSS (-)", "BSS (-)", "BSS (-)"),
                             model="Process-based", group = "Hindcasts")) +
  labs(x = "Region", y = "", fill = "Model: ", linetype = "Hindcasts: ") +
  scale_linetype_manual(labels = c("Uncorrected", "Bias corrected"), values = c("solid", "dashed")) +
  facet_grid(score ~ ., scales = "free_y") +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom")
ggsave(file_out_scores_mixed_biascor, height=10, width = 10)


# drought index time series #--------------------------------------------------

dat_plot_t <- dat_rmse_t %>%
  filter(predictor != "single_reservoir" & grepl("ensemble|perfect", group)) %>%
  group_by(group, model, region, year, month) %>%
  distinct(group, model, region, year, month, value, obs) %>%
  ungroup() %>%
  mutate(month = month+1) %>%
  complete(month = 1:12, nesting(group, model, region, year), fill = list(value = NA, obs=NA)) %>%
  unite(col=date, year, month, sep="-") %>%
  mutate(date = ymd(paste(date, "1", sep="-")) - 1)
dat_plot_t <- bind_rows(dat_plot_t %>%
                          select(-obs),
                        dat_plot_t %>%
                          select(-value, -group) %>%
                          rename(value = obs) %>%
                          distinct(region, date, value) %>%
                          mutate(group = "obs", model = "obs")) %>%
  mutate(main_group = interaction(factor(group), model)) %>%
  separate(group, into = c("group", "member"), sep="_", fill = "right")
  

gp <- ggplot(dat_plot_t, aes(x = date, y = value, group = main_group,
                       colour = model, size = group, linetype = group, alpha = group)) +
  geom_line() +
  scale_x_date(limits = c(ymd("1986-01-01"), ymd("2014-06-30")), date_breaks = "1 year", date_minor_breaks = "1 month") +
  scale_size_manual(values = c("obs" = 1, "perfect" = 1, "median" = 1, "ensemble" = 0.2)) +
  scale_linetype_manual(values = c("obs" = "solid", "perfect" = "dotted", "median" = "dashed", "ensemble" = "solid")) +
  scale_alpha_manual(values = c("obs" = 1, "perfect" = 1, "median" = 1, "ensemble" = 0.2)) +
  scale_colour_manual(values = c("obs" = "black", "hydmod" = hue_pal()(2)[1], "regression" = hue_pal()(2)[2])) +
  ylim(c(0,150)) +
  facet_grid(region ~ .)
ggsave("test.pdf", gp, width = 20, height = 10)


# average regional storage changes #-------------------------------------------
dat_plot_t <- dat_stor_change %>%
  filter(month != 0 & !grepl("biascor|resamp|^ensemble", group)) %>%
  group_by(model, group, realis, month) %>%
  summarise(mean_change = mean(value_change, na.rm = T)) %>%
  mutate(month = month(month, label = T, locale = "en_GB.UTF-8")) %>%
  ungroup() %>%
  mutate(model = factor(model, c("hydmod", "regression", "obs"), labels = c("Process-based model", "Statistical model", "Observations")),
         group = factor(group, c("median_ensemble", "perfect", "obs"), labels = c("Hindcast", "Simulation", "Observation")))

gp <- ggplot(dat_plot_t,
       aes(x = month, y = mean_change, group = interaction(group, model),
           colour = model, linetype = group, shape = group)) +
  geom_point(size = 5) +
  geom_line(size = 1.2) +
  lims(y = c(-5,20)) +
  labs(x = "Aggregation month", y = "Regional storage change (percent point)",
       colour = "Data: ", linetype = "Mode: ", shape = "Mode: ") +
  scale_color_manual(values = c("Process-based model" = hue_pal()(4)[1], "Statistical model" = hue_pal()(4)[3], "Observations" = hue_pal()(4)[2])) +
  scale_linetype_manual(values = c("Hindcast" = "solid", "Simulation" = "dashed", "Observation" = "dotted")) +
  theme_bw(base_size = 22) +
  theme(legend.key.width = unit(50, "pt"))
ggsave(file_out_storage_change, height = 8, width = 18)
