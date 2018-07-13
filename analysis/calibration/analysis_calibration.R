
# This file analysis_calibration.R is part of the analysis scripts for the
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


# Script analyses calibration results of the WASA-SED model together with potential
# predictors influencing model performance, such as reservoir parameters and rainfall
# statistics.
# Constructs regression trees for several response variables measuring model goodness
# and analyses predictor importance.

### INPUT ###------------------------------------------------------------------

# tidy R and visualisation
library(tidyverse)
library(lubridate)
library(scales)
# regression trees
library(rpart)
library(partykit)
library(rpart.plot)
library(RColorBrewer)
# latex table
library(knitr)
library(kableExtra)
library(tools)

# RData file with calibrated reservoir volume simulations + observations
file_calib_vol_obs <- "../data/res_vol_obs_cal.RData"

# WASA-SED strategic reservoir parameters file
file_respar <- "../data/reservoir_02_2018.dat"

# WASA-SED rainfall input file
file_prec <- "../data/rain_daily.dat"

# relations region/subcatchment vs. subbasins (incl. area and drainage information)
file_reg_sub <- "../data/region_subbasin_relations.dat"

# output: Latex table with predictor importances
file_latex_imp <- "plots/predictor_importance.tex"

# output: pdf plot of predictor occurrences in regression trees' leaf nodes
file_plot_pred_leaf <- "plots/predictor_realis.pdf"

# output: individual regression trees (<files_plot_regtrees>_<response>.pdf)
files_plot_regtrees <- "plots/tree"


### INTERNALS ###--------------------------------------------------------------

# Function for calutation of total drainage area of a subbasin #---------------
area_up_sum <- function(i, dat_sub) {
  area_sum <- dat_sub$area[which(dat_sub$sub == i)]
  while(TRUE) {
    if(any(dat_sub$drains_to %in% i)) {
      i_t <- which(dat_sub$drains_to %in% i)
      area_sum <- area_sum + sum(dat_sub$area[i_t])
      i <- i_t
    } else {
      break
    }
  }
  return(area_sum)
}

# Function determining number of upstream reservoirs (via subbas ids) #--------
res_no_up <- function(i, dat_sub, dat_res_pars) {
  # upstream subbasin IDs
  sub_up <- NULL
  while(TRUE) {
    if(any(dat_sub$drains_to %in% i)) {
      i_t <- dat_sub$sub[which(dat_sub$drains_to %in% i)]
      sub_up <- c(sub_up, i_t)
      i <- i_t
    } else {
      break
    }
  }
  # number of reservoirs
  if(is.null(sub_up)) {
    res_no <- 0
  } else {
    r_res_up <- which(dat_res_pars$subbas %in% sub_up)
    res_no <- length(r_res_up)
  }
  return(as.integer(res_no))
}

# Function for analysis of regression trees #----------------------------------
# function takes an object of class 'tree' and
# returns a df of extracted rel. predictor importances and
# classified predictor realisations of the nodes with highest and lowest median responses
get_tree_dat <- function(tree_obj) {
  # get importance df
  imp <- tree_obj$variable.importance
  imp_rel <- round(imp/sum(imp)*100)
  vars <- names(imp_rel)
  imp_df <- imp_rel %>%
    as_tibble() %>%
    mutate(predictor = vars) %>%
    complete(predictor = names(tree_obj$ordered), fill=list(value = 0)) %>%
    rename(importance_rel = value)
  
  # get response and predictors for best and worst node
  dat <- data.frame(y = tree_obj$y, node = tree_obj$where) %>%
    group_by(node) %>%
    summarise(y_med = median(y), count = n()) %>%
    mutate(max = node[which.max(y_med)], min = node[which.min(y_med)], count_sum= sum(count)) %>%
    gather(key = group, value = node_val, min, max) %>%
    mutate(count_rel = count/count_sum*100) %>%
    group_by(node_val) %>%
    filter(node %in% unique(node_val)) %>%
    ungroup() %>%
    select(node, y_med, count_rel, group) %>%
    left_join(.,
              bind_cols(data.frame(node = tree_obj$where), as.data.frame(tree_obj$x)),
              by = "node") %>%
    # classify predictor values
    mutate(diff_class = factor(diff_class, c(1,2), c("fall", "rise"), ordered = T),
           capacity = cut(capacity, breaks = c(-Inf, 20, 100, 1000, Inf),
                          labels = c("small", "medium", "large", "huge"), ordered_result = T),
           area_up = cut(area_up, breaks = c(-Inf, 200, 1500, 10000, Inf),
                         labels = c("small", "medium", "large", "huge"), ordered_result = T),
           res_no_up = cut(res_no_up, breaks = c(-Inf, 0, 5, 10, Inf),
                           labels = c("headwater", "low", "medium", "large"), ordered_result = T),
           prec_reg = cut(prec_reg, breaks = c(-Inf, quantile(dat_response$prec_reg, c(.2,.5,.8)), Inf),
                          labels = c("--", "-", "+", "++"), ordered_result = T),
           prec_maxday = cut(prec_maxday, breaks = c(-Inf, quantile(dat_response$prec_maxday, c(.2,.5,.8)), Inf),
                             labels = c("--", "-", "+", "++"), ordered_result = T),
           prec_prev12 = cut(prec_prev12, breaks = c(-Inf, quantile(dat_response$prec_prev12, c(.2,.5,.8)), Inf),
                             labels = c("--", "-", "+", "++"), ordered_result = T),
           prec_prev36 = cut(prec_prev36, breaks = c(-Inf, quantile(dat_response$prec_prev36, c(.2,.5,.8)), Inf),
                             labels = c("--", "-", "+", "++"), ordered_result = T)
    ) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = predictor, value = pred_val, -y_med, -node, -group, -count_rel) %>%
    # count of predictor value occurrences
    group_by(node, predictor) %>%
    mutate(pred_count = n()) %>%
    group_by(node, predictor, pred_val) %>%
    mutate(pred_val_count = n(),
           pred_val_count_rel = pred_val_count/pred_count*100) %>%
    distinct()
  
  # merge data for output
  left_join(dat, imp_df, by = "predictor")
}


### CALCULATIONS ###-----------------------------------------------------------

# data import #---------------------------------------------------------------- 

# get data from file
load(file_calib_vol_obs)

# ignore values with subatch == NA (should be Lower Jaguaribe which was not calibrated)
dat_all <- dat_all %>%
  drop_na(subcatch)

# information about region vs. subbasin links
dat_reg_sub <- read.table(file_reg_sub, header=T, sep="\t") %>%
  arrange(sub)

# get information about maximum capacity for the current reservoirs
dat_res_pars <- read.table(file_respar, skip=2) %>%
  mutate(subbas = V1, capacity = V5/1e3) %>%
  select(subbas, capacity)

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


# calculation of predictors #--------------------------------------------------

# calculate total upstream area for each subbasin
dat_sub_pars <- dat_reg_sub %>%
  rowwise() %>%
  mutate(area_up = area_up_sum(sub, dat_reg_sub))

# determine number of upstream strategic reservoirs for each reservoir
dat_res_pars <- dat_res_pars %>%
  rowwise() %>%
  mutate(res_no_up = res_no_up(subbas, dat_reg_sub, dat_res_pars))

# precipitation time series per region
dat_prec_reg <- dat_prec %>%
  group_by(region, date) %>%
  mutate(weight = area / sum(area),
         value_t = value * weight) %>%
  summarise(value = sum(value_t)) %>%
  ungroup() %>%
  mutate(region = as.character(region))

# wetness in terms of precipitation over different time horizons
dat_wetness <- dat_prec_reg %>%
  mutate(yr = year(date)) %>%
  group_by(region, yr) %>%
  summarise(value = sum(value)) %>%
  group_by(region) %>%
  mutate(prec_prev36_t = c(NA, NA, zoo::rollapply(value, width = 3, FUN = sum)),
         prec_prev12 = c(NA, value[-n()]),
         prec_prev36 = c(NA, prec_prev36_t[-n()])) %>%
  select(region, yr, prec_prev12, prec_prev36) %>%
  ungroup()

# join information
dat_res_pars <- left_join(dat_res_pars, select(dat_sub_pars, sub, area_up), by = c("subbas" = "sub"))


# calculation of responses #---------------------------------------------------

# raw data needed for further analysis
dat_analysis_vals <- left_join(dat_all,
                               # join reservoir parameters
                               dat_res_pars,
                               by = "subbas") %>%
  # limit value to storage capacity
  mutate(value = pmin(value, capacity)) %>%
  # value_sim and value_obs as columns for further calculation needed
  spread(key = group, value = value) %>%
  # determine rising and falling periods for every time step
  mutate(diff_obs = c(NA, diff(obs)), diff_sim = c(NA, diff(sim))) %>%
  # exclude all NA values
  filter(!(is.na(diff_obs) | is.na(obs) | is.na(sim))) %>%
  # classification into rising and falling periods
  mutate(diff_class = if_else(diff_obs >= 0, "rise", "fall"),
         # year and month
         yr = year(date), month = month(date),
         # observed and simulated relative filling and deviation of both
         relfill_sim = 100* sim / capacity, relfill_obs = 100* obs / capacity, relfill_dev = relfill_sim - relfill_obs) %>%
  # join precipitation data
  left_join(., rename(dat_prec_reg, prec_reg = value), by = c("subcatch" = "region", "date")) %>%
  left_join(., dat_wetness, by = c("subcatch" = "region", "yr"))

# correlation coef (pearson), bias, variability (sd comparison), Kling-Gupta Efficiency
dat_response <- dat_analysis_vals %>%
  group_by(subcatch, subbas, resout_id, res_funceme_id, capacity, res_no_up, area_up, yr, diff_class) %>%
  summarise(COR = cor(obs, sim), bias_t = mean(sim)/mean(obs), var_t = sd(sim)/sd(obs),
            BIAS = bias_t-1, VAR = var_t-1,
            prec_maxday = max(prec_reg), prec_reg = sum(prec_reg),
            prec_prev12 = unique(prec_prev12), prec_prev36 = unique(prec_prev36)) %>%
  mutate(a = COR-1, b = VAR, c = BIAS, KGE = 1 - sqrt( a^2 + b^2 + c^2)) %>% # all in one formula produces erroneous results for some reason
  select(-a, -b, -c)


# regression tree #------------------------------------------------------------

# get and prepare relevant data
dat_analysis_tree_numeric <- dat_response %>%
  ungroup() %>%
  select(KGE, COR, BIAS, VAR, diff_class, area_up, capacity, res_no_up, prec_reg, prec_maxday, prec_prev12, prec_prev36) %>%
  mutate_if(~ !is.numeric(.), as.factor) %>%
  # set all "unacceptable" KGE to zero (easier for regression tree as we are not interested in how bad bad values are exactly)
  # cut bias and variab in same way to [-1,1]
  mutate(KGE = if_else(KGE <= 0, 0, KGE),
         BIAS = if_else(BIAS > 1, 1, if_else(BIAS < -1, -1, BIAS)),
         VAR = if_else(VAR > 1, 1, if_else(VAR < -1, -1, VAR))) %>%
  # remove NaNs (there are a few cases originating from lack of data)
  mutate_all(funs(replace(., is.nan(.), NA))) %>%
  drop_na()

# construct regression trees and extract relevant data using helper function (see above)
dat_trees_raw <- dat_analysis_tree_numeric %>%
  gather(key = response, value = resp_val, KGE, COR, BIAS, VAR) %>%
  # create list of data.frames (one data.frame for each response variable)
  nest(-response) %>%
  # fit regression tree for each data.frame
  mutate(tree = map(data, ~ rpart(resp_val ~ diff_class + area_up + capacity + res_no_up +
                                    prec_reg+prec_maxday+prec_prev12+prec_prev36,
                                  data = .,
                                  method = "anova", x = T, control = rpart.control(cp = 0.01, minbucket = 40))),
         # extract results from each tree
         res = map(tree, get_tree_dat)) 

dat_trees <- dat_trees_raw %>%
  unnest(res)


### VISUALISATION ###----------------------------------------------------------

# variable importance; select data to be displayed
dat_tab_imp <- dat_trees %>%
  select(response, predictor, importance_rel) %>%
  filter(response %in% c("KGE", "COR", "BIAS", "VAR")) %>%
  unique() %>%
  spread(key = predictor, value = importance_rel) %>%
  mutate(Response = as.factor(response)) %>%
  arrange(match(Response, c("KGE", "COR", "BIAS", "VAR"))) %>%
  select(Response, capacity, area_up, res_no_up, diff_class, prec_maxday, prec_reg, prec_prev12, prec_prev36, -response)
# name of the tex and pdf document
file_imp_tex <- file_latex_imp
# latex preamble (not automatically inserted)
writeLines(c("\\documentclass{article}",
             # packages possibly employed by kable
             "\\usepackage{booktabs}",
             "\\usepackage{longtable}",
             "\\usepackage{array}",
             "\\usepackage{multirow}",
             "\\usepackage[table]{xcolor}",
             "\\usepackage{wrapfig}",
             "\\usepackage{float}",
             "\\usepackage{colortbl}",
             "\\usepackage{pdflscape}",
             "\\usepackage{tabu}",
             "\\usepackage{threeparttable}",
             "\\usepackage{threeparttablex}",
             "\\usepackage[normalem]{ulem}",
             "\\usepackage{makecell}",
             "\\begin{document}"), con = file_imp_tex)
# generate latex code; incl. adjustments of dataset
dat_tex <- kable(dat_tab_imp %>%
                   # replace "_" by "." as otherwise it is interpreted within latex (escape = F as otherwise specs don't work)
                   rename_at(vars(contains("_")), funs(gsub("_", ".", .))) %>%
                   mutate(Response = gsub("_", ".", Response)) %>%
                   # font size according ti value of variable importance
                   mutate_if(is.numeric, function(x) {
                     cell_spec(x, "latex", bold = F, align="r", font_size = spec_font_size(x, scale_from = c(0,max(dat_tab_imp[,-1]))))
                   }), format = "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c(" ", "Relative predictor importance (%)" = 4+4)) %>%
  kable_styling(latex_options = c("striped")) %>%
  landscape()
# write latex code into file
write(dat_tex, file = file_imp_tex, append = T)
write("\\end{document}", file = file_imp_tex, append = T)
# compile latex code and generate pdf
d <- texi2pdf(file_imp_tex)
file.copy(dir(pattern = file_path_sans_ext(basename(file_imp_tex))), dirname(file_imp_tex))
file.remove(dir(pattern = file_path_sans_ext(basename(file_imp_tex))))


# predictor realisations for highest/lowest leaf nodes
dat_plot_t <- dat_trees %>%
  # relevant columns and rows (filter relevant response variables)
  select(response, group, predictor, pred_val, pred_val_count_rel) %>%
  filter(response %in% c("KGE", "COR", "BIAS", "VAR")) %>%
  # determine max number of predictor values per predictor
  left_join(.,
            group_by(., predictor) %>%
              summarise(no_vals_max = length(unique(pred_val))),
            by = "predictor") %>%
  # complete count of predictor value occurrences (so far, pred_vals omitted if count was zero)
  group_by(response, group, predictor) %>%
  complete(pred_val=1:max(no_vals_max), nesting(response, group, predictor), fill = list(pred_val_count_rel = 0)) %>%
  ungroup() %>%
  select(-no_vals_max) %>%
  mutate(
    # rename variables
    predictor = replace(predictor, predictor == "area_up", "A['up']"),
    predictor = replace(predictor, predictor == "capacity", "V['cap']"),
    predictor = replace(predictor, predictor == "res_no_up", "n['resup']"),
    predictor = replace(predictor, predictor == "diff_class", "paste(Delta['vol'])"),
    predictor = replace(predictor, predictor == "prec_prev12", "P['12']"),
    predictor = replace(predictor, predictor == "prec_prev36", "P['36']"),
    predictor = if_else(grepl("SPI", predictor), gsub("_", "['", paste0(predictor, "']")), predictor),
    response = replace(response, response == "bias_abs", "BIAS['abs']"),
    response = replace(response, response == "variab_abs", "VAR['abs']")
  ) %>%
  filter(predictor %in% c("V['cap']", "A['up']", "P['12']", "P['36']"))

gp <- ggplot(dat_plot_t, aes(x = factor(pred_val, 1:4, labels = c("min", "low", "high", "max")),
                             y = group, fill = pred_val_count_rel)) +
  geom_tile(colour="white", size=1) +
  scale_fill_gradient(low='white', high='grey20') +
  facet_grid(factor(response, c("KGE", "COR", "BIAS", "VAR")) ~ factor(predictor, c("V['cap']", "A['up']", "P['12']", "P['36']")),
             scales = "free", labeller = label_parsed) +
  labs(y= "Leaf node", x = "Predictor class", fill = "Relative predictor class occurrence (%): ") +
  theme_bw(base_size = 20) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom", legend.key.width = unit(50, "pt"))
ggsave(file_plot_pred_leaf, gp, width=14, height=9)


# trees
# Info:
# - green boxes are the individual nodes of the regression tree
# - leaf/terminal nodes are the undermost nodes
# - small white numbered boxes above green boxes refer to the node number
# - plot heading: response variable
# - bold labels at each node: split predictor variable incl. threhsold
# - numers in green boxes:
#   - fitted value of resonse at the node
#   - colour: based on fitted value at the node (the darker the green the more favourable the fitted value)
#   - n: number of values in this node
#   - precent value: relative share of values in this node
for (i in 1:length(dat_trees_raw$tree)) {
  dat_plot_t <- dat_trees_raw$tree[[i]]
  # adjust label of predictor variables in plots
  names(dat_plot_t$variable.importance) <- replace(names(dat_plot_t$variable.importance),names(dat_plot_t$variable.importance) == "prec_prev36", "P36")
  pdf(paste0(files_plot_regtrees, "_", dat_trees_raw$response[i], ".pdf"), width = 10, height = 8)
  if (dat_trees_raw$response[i] %in% c("VAR", "BIAS")) {
    # center darkest hue at around zero
    clrs_t <- c(brewer_pal(palette = "Greens")(9)[1:5], rev(brewer_pal(palette = "Greens")(9)[1:4]))
    max_val <- max(abs(dat_plot_t$frame$yval))
    clrs <- clrs_t[cut(sort(dat_plot_t$frame$yval), breaks=seq(-max_val, max_val, length.out = 10), labels = F, include.lowest = T)]
  } else {
    clrs <- "Greens"
  }
  split.labs <- function(x, labs, digits, varlen, faclen) {
    sapply(labs, function(lab) {
      lab <- sub("prec_prev36", "P[36]", lab)
      lab <- sub("prec_prev12", "P[12]", lab)
      lab <- sub("prec_maxday", "P[max]", lab)
      lab <- sub("prec_reg", "P[reg]", lab)
      lab <- sub("area_up", "A[up]", lab)
      lab <- sub("capacity", "V[cap]", lab)
      lab <- sub("res_no_up", "n[resup]", lab)
      lab <- sub("diff_class", "D[vol]", lab)
      })
  }
  prp(dat_plot_t, main=dat_trees_raw$response[i], type=2, extra=101, box.palette = clrs,
      nn=T, varlen=0, faclen=0, shadow.col="gray", fallen.leaves=T, branch.lty=3,
      split.fun = split.labs, cex=1.3, cex.main=2)
  dev.off()
}
