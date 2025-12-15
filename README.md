# A database of copepod prevalence in the Southern Ocean and neighbouring waters

## Overview
<img src="misc/BIOPOLE_Logo_Colour.png" align="right" width="200">
This repository contains the code to required reproduce the compiled dataset of copepod prevalence measurements stored at the Polar Data Centre (doi link) and detailed in the associated publication <i>A database of copepod prevalence in the Southern Ocean</i> (citation).
The study objective was to unify disparate datasets of Southern Ocean copepod abundance into a single data product adhering to the FAIR principles for scientific data.
This work is output from the <a href="https://biopole.ac.uk/">BIOPOLE</a> project, supported by National Capability Multicentre Round 2 funding from the Natural Environment Research Council (grant no. NE/W004933/1)

## Code summary
#### Scripts
Code scripts are contained in the `R` directory. The entire procedure for cleaning and compiling the data, and producing some output plots & summary statistics, is implemented through `run_data_prep.R`, which is the only script that needs to be opened. This simple script sources three functions, contained in the `R/functions` directory, that should be run in succession to clean the original data, compile it into a single table, then produce plots.

#### Data
The compiled data product is not stored in this repository but is accessible from the above link.
In the interest of reproducability, the raw, unprocessed data sets are included in the `data/zooplankton` directory. These were compressed as gzip files and stored in this repository using Git LFS. If cloning the repository ensure that Git LFS is installed on your computer so that the download properly handles the data files. Please note that there is a monthly download limit on LFS files, so if the code downloads successfully but the original data files do not (and Git LFS is installed on your computer) then it's likely that this threshold has been exceeded.

#### Software
Code scripts were written in R version 4.5.2.

#### Packages
* Requried R packages and the version numbers used: Cairo 1.6.2, cowplot 1.1.3, data.table 1.17.0, dplyr 1.1.4, ggh4x 0.3.1, ggplot2 3.5.2, ggpmisc 0.6.2, ggnewscale 0.5.1, ggpp 0.5.9, ggrepel 0.9.6, ggtext 0.1.2, ggthemes 5.1.0, patchwork 1.3.0, R.utils 2.13.0, RColorBrewer 1.1.3, reshape2 1.4.4, sf 1.0.20, sp 2.2.0.
* Other software dependencies (needed only for ploting a map of the data) and the version numbers used: GEOS 3.10.2, GDAL 3.4.1, PROJ 8.2.1.
