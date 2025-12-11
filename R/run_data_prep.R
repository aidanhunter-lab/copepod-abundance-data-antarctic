#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Run this script to clean copepod prevalence data sets then compile them into
#' a single table.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' The R working directory must be the project R directory: `dir.project.R`
dir.project.R <- 'copepod-abundance-data-antarctic/R'
if(!grepl(dir.project.R, getwd())){
  stop(paste("Working directory must be project R directory:", dir.project.R))}

# Clean -------------------------------------------------------------------

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Clean raw data from various sources and save the outputs as separate tables.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Run the 'download copepod occurrence records.R' script first [hmmm, maybe the raw data should just be provided in the ./data directory]


#' If `auto.select.data.sets` is FALSE then user input will be requested to
#' select between data stored on disk.
auto.select.data.sets <- TRUE

#' Sourcing `clean copepod occurrence records.R` loads original, raw data sets,
#' cleans them, then saves output tables (with '_cleaned' appended to the file
#' name) into the directories holding the original data.
source('clean copepod occurrence records.R')

#' It may be useful to clear RAM by restarting R before running the data
#' compilation script in the next code section.

rm(list=ls()); gc() #' flushing the R memory should be done inside the cleaning script

# Compile -----------------------------------------------------------------

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile cleaned data sets from various sources into a single table and save
#' the output.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' If `species.selection` is 'copepods' then data related to all copepods is
#' returned by comparing reported species names to a comprehensive list of
#' copepod genera. Otherwise, one or more particular species (separated by
#' underscores) may be selected, e.g, by setting `species.selection` to
#' 'Calanoides acutus_Calanus propinquus'. This feature is somewhat redundant as
#' species can simply be filtered out from the complete copepod data set.
species.selection <- 'copepods'

#' Sourcing `compile copepod occurrence records.R` loads the cleaned data sets 
#' (produced in the above step), harmonises field names and within-field
#' notation, combines the data into a single table, removes duplicated records,
#' then saves the output.
source('compile copepod occurrence records.R')

