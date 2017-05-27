library(dplyr)
library(ggplot2)
library(parallel)

options(stringsAsFactors = F)
verbose <- T
wd_data <- "../.."
ncores <- 4  #go up to 8

source("2.1-features.R")
source("2.2-loc_functions.R")

