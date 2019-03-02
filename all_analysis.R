
library(splitstackshape)  
library(reshape2)
library(data.table)  
library(compete)
library(PlayerRatings)
library(gridExtra)
library(stringr)
library(brms)
library(coda)
library(broom)
library(multcomp)
library(bayesplot)
library(tidyverse)
library(ggthemes)
library(ggridges)
library(viridis)
source("code_functions/functions.R")

#1. Clean up raw data into clean data frames 
source("code_carpentry/01_list_behavior_dataframes.R")
source("code_carpentry/02_clean_bodyweight_data.R")
source("code_carpentry/03_clean_autoradiography_data.R")

#2. Social hierarchy characteristics  f
source("code_analysis/01_observation_descriptive.R")
source("code_analysis/02_dominance.R")
source("code_analysis/03_bodyweight.R")

#3. Oxytocin receptor (OTR) density & Vasopressin subtype 1a receptor (V1aR) density 
source("code_analysis/04_otr_v1ar_density.R")


