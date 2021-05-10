rm(list = ls())
## change your path to the root directory of the project
root.dir <- "/Users/ztian/OneDrive - The Pennsylvania State University/research/database/pulse_household"
setwd(root.dir)

## load packages and create directories
source("cmd_initiate.R")


## download the PUF file and read into R

## Before running this file, make sure that you change
## neweeks and oldweeks. For example, for the first time running this file
## neweeks should be 1:T, where T is the latest week that the PUF is available
## and oldweeks should be NULL. After running, you need to change neweeks to NULL
## and oldweeks to 1:T. Whenever, we have new weeks, we can change neweeks to (T+i):(T+I).
neweeks <- NULL
oldweeks <- 1:27

## increase timeout if needed
output.format <- "rds" # "csv"
source("cmd_download_puf.R")


