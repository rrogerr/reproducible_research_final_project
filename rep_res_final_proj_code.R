######################### IMPORTS #########################

path <- "/home/rogelio/Desktop/datasciencecoursera/"
p <- "reproducible_research_final_project"

path <- paste0(path, p)

setwd(path)

storm_data <- read.csv("./repdata%2Fdata%2FStormData.csv.bz2")