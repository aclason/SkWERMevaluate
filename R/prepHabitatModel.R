

#---------------------- Load libraries---------------------------------
ls <- c("bcdata")
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("terra","sf")) # geo comp.
# Install if needed -- then load.
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

Hab_lay <- read_vri_bem(dsn = "./Inputs/SkWERM_Lakes.gdb",
                        layer="Lakes_10Mar2023_forVerification")

