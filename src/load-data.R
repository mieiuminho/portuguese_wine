#!/usr/bin/env Rscript

DATA_PATH <- "../data"

# load data
red_wine <- read.csv(paste(DATA_PATH, "winequality-red.csv", sep = "/"), sep = ";")
white_wine <- read.csv(paste(DATA_PATH, "winequality-white.csv", sep = "/"), sep = ";")

print("Data loaded into `red_wine` and `white_wine` dataframe objects")
