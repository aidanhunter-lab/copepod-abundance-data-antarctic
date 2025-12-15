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

#' The `clean.data()` function loads original, raw data sets, cleans them, then
#' saves output tables (with '_cleaned' appended to the file name) into the
#' directories holding the original data.
source('functions/clean_copepod_occurrence_records.R')
clean.data()

# Compile -----------------------------------------------------------------

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Compile cleaned data sets from various sources into a single table and save
#' the output.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' The `compile.data()` function loads the cleaned data sets (produced in the
#' above step), harmonises field names and within-field notation, combines the
#' data into a single table, removes duplicated records, then saves the output.

#' If `species.selection` is 'copepods' then data related to all copepods is
#' returned by comparing reported species names to a comprehensive list of
#' copepod genera. Otherwise, one or more particular species (separated by
#' underscores) may be selected, e.g, by setting `species.selection` to
#' 'Calanoides acutus_Calanus propinquus'. This feature is somewhat redundant as
#' species can simply be filtered out from the complete copepod data set.

source('functions/compile_copepod_occurrence_records.R')
compile.data(species.selection = 'copepods')

# Plot --------------------------------------------------------------------

#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Examine the plots and basic summary statistics of the compiled data that
#' feature in the associated data paper.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' By default, the `plot.data()` function generates and save plots, but does not
#' return output. Set the function arguments `return.plots` and
#' `display.summary.stats` as TRUE to view plots in the active R session and for
#' a print-out of basic data summary stats. The plots saved to disk may be easier
#' to view because font sizes are not fixed by the R session.

source('functions/plot_copepod_occurrence_records.R')
plot.data(return.plots = TRUE, display.summary.stats = TRUE)




