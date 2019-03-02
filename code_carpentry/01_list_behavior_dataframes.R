cohort006 <- read.csv("data_raw/behavior/cohort006_wl_startend.csv", stringsAsFactors = F)
cohort007 <- read.csv("data_raw/behavior/cohort007_wl_startend.csv", stringsAsFactors = F)
cohort011 <- read.csv("data_raw/behavior/cohort011_jc_startend.csv", stringsAsFactors = F)
cohort012 <- read.csv("data_raw/behavior/cohort012_jc_startend.csv", stringsAsFactors = F)
cohort052 <- read.csv("data_raw/behavior/cohort052_wl_startend.csv", stringsAsFactors = F)
cohort073 <- read.csv("data_raw/behavior/cohort073_wl_startend.csv", stringsAsFactors = F)
cohort074 <- read.csv("data_raw/behavior/cohort074_wl_startend.csv", stringsAsFactors = F)
cohort083 <- read.csv("data_raw/behavior/cohort083_wl_startend.csv", stringsAsFactors = F)



df.l <- list('A' = cohort006, 
             'B' = cohort007, 
             'C' = cohort011, 
             'D' = cohort012, 
             'E' = cohort052, 
             'F' = cohort073, 
             'G' = cohort074, 
             'H' = cohort083)


