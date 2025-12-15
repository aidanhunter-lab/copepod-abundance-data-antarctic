#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Clean the original copepod prevalence data sets by running simple input-error
#' checks, renaming fields, setting within-field notations, and converting into
#' 'long format' data tables.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean.data <- function(lat_lim = c(-90,-30), save.cleaned.data = TRUE,
                       clear.on.completeion = TRUE, propRecords2Keep = 0.8,
                       auto.select.data.sets = TRUE
){
  
  # Load packages -----------------------------------------------------------
  library(data.table)
  library(dplyr)
  library(reshape2)
  # library(rgbif)
  
  # Set directories ---------------------------------------------------------
  dir.root <- dirname(getwd())
  
  dir.data.base <- file.path(dir.root, 'data')
  dir.data.zoo <- file.path(dir.data.base, 'zooplankton')
  dir.functions <- file.path(getwd(), 'functions')
  
  data.sources <- list.dirs(dir.data.zoo, recursive = FALSE, full.names = FALSE)
  data.sources <- data.sources[!grepl('compiled', data.sources)]
  message('\nData sources: ', paste(data.sources, collapse = ', '))
  
  #' Set copepod data directories for each source and for the cleaned/compiled data
  dir.data.all <- list()
  
  dir.data.all$OBIS                       <- file.path(dir.data.zoo, 'OBIS', 'copepoda')
  dir.data.all$GBIF                       <- file.path(dir.data.zoo, 'GBIF', 'copepoda')
  dir.data.all$BAS_bongo                  <- file.path(dir.data.zoo, 'BAS', 'bongo nets')
  dir.data.all$BAS_rmt                    <- file.path(dir.data.zoo, 'BAS', 'rmt nets')
  dir.data.all$BAS_mocness                <- file.path(dir.data.zoo, 'BAS', 'MOCNESS')
  dir.data.all$Schnack.Schiel             <- file.path(dir.data.zoo, 'Schnack-Schiel', 'datasets')
  dir.data.all$CHINARE                    <- file.path(dir.data.zoo, 'CHINARE')
  dir.data.all$Palmer.LTER_MOCNESS        <- file.path(dir.data.zoo, 'Palmer_LTER', 'zooplankton 2009-2017')
  dir.data.all$Palmer.LTER_non_stratified <- file.path(dir.data.zoo, 'Palmer_LTER', 'zooplankton 1993-2008')
  
  dir.COPEPOD.portal               <- file.path(dir.data.zoo, 'COPEPOD')
  dir.data.all$AtlantNIRO          <- file.path(dir.COPEPOD.portal, 'AtlantNIRO', 'copepod__ru-05301', 'data_src', 'short-format')
  dir.data.all$ELTANIN             <- file.path(dir.COPEPOD.portal, 'ELTANIN', 'copepod__us-04101', 'data_src', 'short-format')
  dir.data.all$Foxton_1956         <- file.path(dir.COPEPOD.portal, 'Foxton_1956', 'copepod__uk-04102', 'data_src', 'short-format')
  dir.data.all$JARE                <- file.path(dir.COPEPOD.portal, 'JARE', 'copepod__jp-05101', 'data_src', 'short-format')
  dir.data.all$OB_1955_1957        <- file.path(dir.COPEPOD.portal, 'OB_1955-1957', 'copepod__ru-01003', 'data_src', 'short-format')
  dir.data.all$Operation_HIGHJUMP  <- file.path(dir.COPEPOD.portal, 'Operation HIGHJUMP', 'copepod__us-01028', 'data_src', 'short-format')
  dir.data.all$Professor_Siedlecki <- file.path(dir.COPEPOD.portal, 'Professor Siedlecki', 'copepod__pl-01001', 'data_src', 'short-format')
  dir.data.all$YugNIRO             <- file.path(dir.COPEPOD.portal, 'YugNIRO', 'copepod__ru-05501', 'data_src', 'short-format')
  
  n.data.sources <- length(dir.data.all)
  for(i in 1:n.data.sources){
    for(j in 1:length(dir.data.all[[i]])){
      if(!dir.data.all[[i]][j] %in% list.dirs(dir.root, recursive = TRUE)){
        stop(paste0('The directory specified for ', names(dir.data.all)[i], 
                    ' data (', dir.data.all[[i]][j], ') is not in project root directory! Check directory names.'))}}}
  rm(i,j)
  
  # Source functions --------------------------------------------------------
  
  #' Load all .R files from 'functions' directory into global environment
  omit.funs <- c('clean_copepod_occurrence_records.R',
                 'compile_copepod_occurrence_records.R',
                 'plot_copepod_occurrence_records.R')
  R_functions <- list.files(dir.functions, pattern = "*.R$", ignore.case = TRUE)
  R_functions <- R_functions[!R_functions %in% omit.funs]
  get.functions <- function(dir, n, e){
    invisible(sapply(paste(dir, n, sep = '/'), source, local = e))}
  env <- environment(get.functions)
  get.functions(dir.functions, R_functions, env)
  

  # Load mapping data -------------------------------------------------------
  
  # mapData <- getMapData(dataDirectory = dir.map, hemisphere = hemisphere,
  #                       lat_lim = lat_lim, returnPlot = TRUE)
  # if(displayPlots) mapData$map_plot
  
  # Load copepod occurrence records ----------------------------------------
  
  #' Pull file names of original data (all with .gz or .zip extensions)
  all.data.file.names <- lapply(dir.data.all, function(z){
    f <- list.files(z)
    f[{grepl('.gz', f) | grepl('.zip', f)} & !grepl('cleaned', f)]})
  
  message('\nAll data file names:')
  print(all.data.file.names)
  
  data.file.names <- setNames(vector('list', length = n.data.sources),
                              names(dir.data.all))
  
  #' Automate data selection by default
  if(!exists('auto.select.data.sets')) auto.select.data.sets <- TRUE
  if(auto.select.data.sets){
    #' 0 = all files (for data sources providing multiple files)
    #' 1 = single file
    which.data.files <- setNames(vector('list', length = n.data.sources), names(data.file.names))
    which.data.files$OBIS <- 1
    which.data.files$GBIF <- 1
    which.data.files$BAS_bongo <- 1
    which.data.files$BAS_rmt <- 1
    which.data.files$BAS_mocness <- 0
    which.data.files$Schnack.Schiel <- 0
    which.data.files$CHINARE <- 0
    which.data.files$Palmer.LTER_MOCNESS <- 1
    which.data.files$Palmer.LTER_non_stratified <- 1
    which.data.files$AtlantNIRO <- 0
    which.data.files$ELTANIN <- 0
    which.data.files$Foxton_1956 <- 0
    which.data.files$JARE <- 0
    which.data.files$OB_1955_1957 <- 0
    which.data.files$Operation_HIGHJUMP <- 0
    which.data.files$Professor_Siedlecki <- 0
    which.data.files$YugNIRO <- 0
  }
  
  for(i in 1:n.data.sources){
    s <- names(data.file.names)[i]
    f <- all.data.file.names[[s]]
    x <- matrix(vgrepl(c('\\.csv\\.gz', '\\.tab\\.gz', '\\.txt\\.gz'), f), ncol = 3)
    x <- apply(x, 1, any)
    # x <- vgrepl(c('\\.csv\\.gz', '\\.tab\\.gz', '\\.txt\\.gz'), f, SIMPLIFY = FALSE)
    # x <- unlist(x[sapply(x, any)])
    if(sum(x) == 1) data.file.names[[s]] <- f[x] else{
      o <- f[x] # multiple options to choose from
      if(auto.select.data.sets){
        p <- which.data.files[[s]]
        if(any(p == 0)) data.file.names[[s]] <- o else data.file.names[[s]] <- o[as.numeric(p)]
      }else{
        ff <- NA
        while(any(is.na(ff))){
          preprint <- setNames(list(data.frame(number = 1:length(o), data.file = o)), s)
          Prompt <- 'Enter one number to select a single data file; multiple numbers separated by single spaces to select multiple data files; or 0 to select all data files: \n'
          p <- prompt.user.input(Prompt, preprint)
          p <- as.numeric(strsplit(p, ' ')[[1]])
          if(any(p == 0)) ff <- o else ff <- o[as.numeric(p)]
          if(!any(is.na(ff))) data.file.names[[s]] <- ff else message('\n', 'Invalid number entered. Choose from the listed options.', '\n')
        }}}}
  suppressWarnings(rm(i, s, f, x, o, p, ff, preprint, Prompt))
  
  message('\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n',
          'Selected data files:\n')
  print(data.file.names)
  message('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
  
  #' Some tables (from COPEPOD data base) are not ideally formatted for machine
  #' reading and require skipping over initial rows, and also require omitting the
  #' headers then specifying them after loading.
  COPEPOD.data.sets <- c("AtlantNIRO", "ELTANIN", "Foxton_1956", "JARE", "OB_1955_1957",
                         "Operation_HIGHJUMP", "Professor_Siedlecki", "YugNIRO")
  skip.rows <- setNames(rep(0, n.data.sources), names(data.file.names))
  skip.rows[COPEPOD.data.sets] <- 17 # checked for every data set
  include.header <- setNames(rep(TRUE, n.data.sources), names(data.file.names))
  include.header[COPEPOD.data.sets] <- FALSE
  
  n.data.files <- sapply(data.file.names, length)
  
  DATA <- setNames(lapply(1:n.data.sources, function(z){
    if(n.data.files[z] == 1) NULL else vector('list', length = n.data.files[z])}),
    names(data.file.names))
  
  for(i in 1:n.data.sources){
    Source <- names(DATA)[i]
    p <- paste('Loading data:', Source)
    message('\n', p)
    Dir <- dir.data.all[[i]]
    multiple.files <- n.data.files[i] > 1
    for(j in 1:n.data.files[i]){
      file.name <- data.file.names[[Source]][j]
      file.path <- paste(Dir, file.name, sep = '/')
      file.type <- paste(tail(strsplit(file.name, '\\.')[[1]], -1), collapse = '.')
      # file.type <- strsplit(file.name, '\\.')[[1]][2]
      switch(file.type,
             csv.gz = {
               if(!multiple.files){
                 DATA[[i]] <- tryCatch(
                   read.csv(
                     gzfile(file.path), header = include.header[Source],
                     skip = skip.rows[Source]),
                   error = function(e) NA)
               }else{
                 DATA[[i]][[j]] <- tryCatch(
                   read.csv(
                     gzfile(file.path), header = include.header[Source],
                     skip = skip.rows[Source]),
                   error = function(e) NA)}
             },
             txt.gz = {
               if(!multiple.files){
                 DATA[[i]] <- tryCatch(
                   read.delim(gzfile(file.path), header = include.header[Source],
                              quote = ''), error = function(e) NA)
               }else{
                 DATA[[i]][[j]] <- tryCatch(
                   read.delim(gzfile(file.path), header = include.header[Source],
                              quote = ''), error = function(e) NA)}
             },
             tab.gz = {
               if(!multiple.files){
                 DATA[[i]] <- read.delim(gzfile(file.path), header = include.header[Source],
                                         skip = skip.rows[Source])
               }else{
                 DATA[[i]][[j]] <- read.delim(gzfile(file.path), header = include.header[Source],
                                              skip = skip.rows[Source])}
             }
      )
      rm(file.name, file.path, file.type)
    }
    rm(Source, Dir, multiple.files)
  }
  
  suppressWarnings(rm(skip.rows, include.header, i, j, p, tempDir, txtFile))
  
  #' Remove `NA` values from the COPEPOD portal data sets -- there's at least one
  #' empty data set.
  DATA <- lapply(DATA, function(z) if(is.data.frame(z)) return(z) else return(z[which(sapply(z, is.data.frame))]))
  
  #' Create column names for COPEPOD portal data sets
  COPEPOD.data.fields <- c(
    'SHP-CRUISE','YEAR','MON','DAY','TIMEgmt','TIMEloc','LATITUDE','LONGITDE','UPPER_Z',
    'LOWER_Z','T','GEAR','MESH','NMFS_PGC','ITIS_TSN','MOD','LIF','PSC','SEX','V',
    'Water Strained', 'Original-VALUE','Orig-UNITS', 'VALUE-per-volu','UNITS','F1',
    'F2','F3','F4', 'VALUE-per-area','UNITS','F1','F2','F3','F4','SCIENTIFIC NAME -[ modifiers ]-',
    'RECORD-ID','DATASET-ID', 'SHIP', 'PROJ', 'INST','Orig-CRUISE-ID','Orig-STATION-ID',
    'Taxa-Name','Taxa-Modifiers',' ')
  
  for(i in 1:length(COPEPOD.data.sets)){
    d <- DATA[[COPEPOD.data.sets[i]]]
    if(!is.data.frame(d)){
      n <- length(d)
      for(j in 1:length(d)){
        dd <- d[[j]]
        names(dd) <- COPEPOD.data.fields
        DATA[[COPEPOD.data.sets[i]]][[j]] <- dd
      }
    }else{
      names(d) <- COPEPOD.data.fields
      DATA[[COPEPOD.data.sets[i]]] <- d
    }
  }
  
  rm(i, d, n, j, dd)
  
  # Utility functions -------------------------------------------------------
  omitEmptyColumns <- function(x){
    # omit empty columns (all NAs or all '') of data frame x
    emptyColumns <- sapply(1:ncol(x), function(z){
      y <- x[,z]
      isChar <- class(y) == 'character'
      if(!all(isChar)) return(all(is.na(y))) else return(all(y == '' | is.na(y)))
    })
    x[,!emptyColumns]
  }
  omitEmptyRows <- function(x){
    # omit empty rows (all NAs or all '') of data frame x
    colClasses <- sapply(names(x), function(z) class(x[[z]]))
    isPOSIX <- sapply(colClasses, function(z) any(grepl('POSIX', z)))
    x[,isPOSIX] <- sapply(x[,isPOSIX], as.character)
    emptyRows <- apply(is.na(x) | x == '', 1, all)
    x[!emptyRows,]
  }
  removeWhiteSpacePadding <- function(x){
    y <- x
    if(length(x) == 0) return(y)
    u <- unique(x)
    u <- u[!is.na(u)]
    for(i in 1:length(u)){
      ui <- u[i]
      j <- x == ui
      while(substr(ui,1,1) == ' ') ui <- substr(ui,2,nchar(ui))
      while(substr(ui,nchar(ui),nchar(ui)) == ' ') ui <- substr(ui,1,nchar(ui)-1)
      y[j] <- ui
    }
    return(y)
  }
  countRecordsPerSpecies <- function(x, n){
    # return vector of number of occurrence records per species contained in data
    # frame x, where n is the column of species names.
    y <- as.data.frame(x)[,n]
    allSpecies <- unique(y)
    recordsPerSpecies <- sapply(allSpecies, function(z) sum(y == z))
    # Sort by number of records
    o <- order(recordsPerSpecies, decreasing = TRUE)
    return(recordsPerSpecies[o])
  }
  cumulativeProportion <- function(x){
    y <- cumsum(x)
    return(y / sum(x))}
  chooseSpeciesBySampleSize <- function(
    recordsPerSpecies, propRecords, method = 'proportion',
    propRecords2Keep = 0.75){
    speciesSelected <- switch(
      method,
      proportion = {
        i <- propRecords <= propRecords2Keep
        n <- sum(i)
        names(recordsPerSpecies)[i]
      })
    return(speciesSelected)}
  filterDataBySpecies <- function(x, f, n) x[as.data.frame(x)[,n] %in% f,]
  
  setCRS <- function(x, CRS = mapData$crs, baseCRS = mapData$crs_base){
    st_crs(x) <- baseCRS
    x <- st_transform(x, crs = CRS)
    return(x)}
  
  
  # Load supplementary tables -----------------------------------------------
  
  #' Load COPEPOD taxa modifier table
  COPEPOD.taxa.modifiers <- read.csv(gzfile(paste(dir.COPEPOD.portal, 'copecode-taxameta.modifier.csv.gz', sep = '/')))[,-3]
  #' Omit white space
  COPEPOD.taxa.modifiers$Taxa.Modifier.Description <- removeWhiteSpacePadding(COPEPOD.taxa.modifiers$Taxa.Modifier.Description)
  COPEPOD.taxa.modifiers$Taxa.Modifier.Description <- gsub('cf .', 'cf.', COPEPOD.taxa.modifiers$Taxa.Modifier.Description)
  #' Extend table for easy indexing
  m <- max(COPEPOD.taxa.modifiers$Taxa.Modifier.Code)
  x <- data.frame(Taxa.Modifier.Code = 1:m, Taxa.Modifier.Description = character(m))
  x$Taxa.Modifier.Description[COPEPOD.taxa.modifiers$Taxa.Modifier.Code] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description
  x$Taxa.Modifier.Description[x$Taxa.Modifier.Description == ''] <- NA
  COPEPOD.taxa.modifiers <- x
  rm(x, m)
  
  #' Load COPEPOD life stage code table
  COPEPOD.life.stage.codes <- read.csv(gzfile(paste(dir.COPEPOD.portal, 'copecode-taxameta.life_stage.csv.gz', sep = '/')))[,-3]
  #' Omit white space
  COPEPOD.life.stage.codes$Life.Stage.Description <- removeWhiteSpacePadding(COPEPOD.life.stage.codes$Life.Stage.Description)
  #' Extend table for easy indexing
  m <- max(COPEPOD.life.stage.codes$Life.Stage.Code)
  x <- data.frame(Life.Stage.Code = 1:m, Life.Stage.Description = character(m))
  x$Life.Stage.Description[COPEPOD.life.stage.codes$Life.Stage.Code] <- COPEPOD.life.stage.codes$Life.Stage.Description
  x$Life.Stage.Description[x$Life.Stage.Description == ''] <- NA
  COPEPOD.life.stage.codes <- x
  COPEPOD.life.stage.codes$Life.Stage.Description <- tolower(COPEPOD.life.stage.codes$Life.Stage.Description)
  for(i in paste0('c', 1:6)) COPEPOD.life.stage.codes$Life.Stage.Description <- gsub(i, toupper(i), COPEPOD.life.stage.codes$Life.Stage.Description)
  for(i in paste0('n', 1:6)) COPEPOD.life.stage.codes$Life.Stage.Description <- gsub(i, toupper(i), COPEPOD.life.stage.codes$Life.Stage.Description)
  j <- grepl('-', COPEPOD.life.stage.codes$Life.Stage.Description) & grepl('C', COPEPOD.life.stage.codes$Life.Stage.Description)
  COPEPOD.life.stage.codes$Life.Stage.Description[j] <- gsub('-','-C',COPEPOD.life.stage.codes$Life.Stage.Description[j])
  j <- grepl('-', COPEPOD.life.stage.codes$Life.Stage.Description) & grepl('N', COPEPOD.life.stage.codes$Life.Stage.Description)
  COPEPOD.life.stage.codes$Life.Stage.Description[j] <- gsub('-','-N',COPEPOD.life.stage.codes$Life.Stage.Description[j])
  j <- grepl('\\+', COPEPOD.life.stage.codes$Life.Stage.Description)
  k <- j & grepl('C', COPEPOD.life.stage.codes$Life.Stage.Description)
  COPEPOD.life.stage.codes$Life.Stage.Description[k] <- gsub('\\+', '-C6', COPEPOD.life.stage.codes$Life.Stage.Description[k])
  COPEPOD.life.stage.codes$Life.Stage.Description[j] <- gsub(' ','',COPEPOD.life.stage.codes$Life.Stage.Description[j])
  COPEPOD.life.stage.codes$Life.Stage.Description[j] <- gsub('\\+', ' and ', COPEPOD.life.stage.codes$Life.Stage.Description[j])
  rm(x, m, i, j, k)
  
  #' Load COPEPOD sex code table
  COPEPOD.sex.codes <- read.csv(gzfile(paste(dir.COPEPOD.portal, 'copecode-taxameta.sex.csv.gz', sep = '/')))
  COPEPOD.sex.codes[,2] <- paste(COPEPOD.sex.codes[,2], COPEPOD.sex.codes[,3])
  COPEPOD.sex.codes <- COPEPOD.sex.codes[,-3]
  #' Omit white space
  COPEPOD.sex.codes$Taxa.Sex.Description <- removeWhiteSpacePadding(COPEPOD.sex.codes$Taxa.Sex.Description)
  
  # Filter/clean data -------------------------------------------------------
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Work through each data source individually
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  keep.vars <- c(ls(), 'keep.vars')
  
  # OBIS --------------------------------------------------------------------
  
  Source <- 'OBIS'
  
  message('\n------------------\n',
          'Cleaning OBIS data',
          '\n------------------')
  
  #' Omit empty rows and records missing basic information
  message('\n', 'Omitting empty rows/columns')
  dat <- DATA[[Source]]
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  noName <- is.na(dat$scientificName) #' missing species name
  noPosition <- is.na(dat$decimalLongitude) | is.na(dat$decimalLatitude) #' missing location info
  noDepth <- is.na(dat$depth) & {is.na(dat$minimumDepthInMeters) | is.na(dat$maximumDepthInMeters)}
  dat$marine[is.na(dat$marine)] <- FALSE
  notMarine <- !dat$marine #' not known to be marine records
  noDate <- apply(is.na(dat[,grep('date', names(dat), ignore.case = TRUE)]), 1, all) #' missing dates
  omitRecords <- noName | noPosition | noDepth | noDate | notMarine
  dat <- dat[!omitRecords,]
  rm(noName, noPosition, noDepth, noDate, notMarine, omitRecords)
  
  #' Filter data by species to omit those with only a few records
  names(dat)[names(dat) == 'scientificName'] <- 'species'
  
  recordsPerSpecies <- countRecordsPerSpecies(dat, 'species')
  propRecords <- cumulativeProportion(recordsPerSpecies)
  
  #' Data may be reduced by retaining only the species that have the most records.
  #' This is controlled by `propRecords2Keep`, which retains the most sampled
  #' species accounting for `propRecords2Keep` * 100% of the records.
  
  # propRecords2Keep <- 0.8
  if(propRecords2Keep < 1) message('\n', 'Omit poorly sampled species')
  
  speciesSelected <- chooseSpeciesBySampleSize(
    recordsPerSpecies, propRecords, propRecords2Keep = propRecords2Keep)
  
  #' Reduce data according to `propRecords2Keep`
  dat <- filterDataBySpecies(dat, speciesSelected, 'species')
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Omit records lacking information on depth. Either a single sample depth or a
  #' depth range.
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  recordDepth <- !is.na(dat$depth)
  recordDepthMin <- !is.na(dat$minimumDepthInMeters)
  recordDepthMax <- !is.na(dat$maximumDepthInMeters)
  noDepthInfo <- !{recordDepth | {recordDepthMin & recordDepthMax}}
  dat <- dat[!noDepthInfo,]
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise the sex field
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~
  message('\n', "Standardise 'sex'")
  
  sex <- dat$sex
  # unique(sex)
  isUnspecifiedSex <- is.na(sex)
  sex <- tolower(sex)
  isUnspecifiedSex <- isUnspecifiedSex | 
    sex %in% c('i', 'unknown', 'juvenile', 'undetermined', 'intersex', '`', 'j',
               'u', 'female;unknown')
  sex[isUnspecifiedSex] <- 'unknown'
  sex <- gsub(';', '', sex)
  isMale <- sex == 'm' | sex == 'male'
  sex[isMale] <- 'male'
  isFemale <- sex == 'f' | sex == 'female'
  sex[isFemale] <- 'female'
  isMaleAndFemale <- sex == 'male female' | sex == 'female male'
  sex[isMaleAndFemale] <- 'male and female'
  #' Check updated names
  # d <- unique(data.frame(old = dat$sex, new = sex))
  # print(d[order(d$new),], row.names = FALSE)
  dat$sex <- sex #' assign cleaned variable to data frame
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise the life stage field
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message('\n', "Standardise 'life stage'")
  
  lifeStage <- dat$lifeStage
  # unique(lifeStage)
  
  #' There's varying degrees of detail in reports of life stage, so capture this
  #' within three fields: (1) maturity = c('juvenile', 'adult')
  #'                      (2) lifeStage = c('nauplius', 'copepodite', 'adult')
  #'                      (3) copepoditeStage = c('C1','C2','C3','C4','C5', 'C6', ...)
  n <- names(dat)
  ni <- which(n == 'lifeStage')
  n <- c(n[1:{ni-1}], 'maturity', 'lifeStage', 'copepoditeStage', n[-{1:ni}])
  dat$maturity <- NA
  dat$lifeStage <- NA
  dat$copepoditeStage <- NA
  dat <- dat[,n]
  
  #' Collect records with unknown life stage under 'unspecified'
  lifeStage[is.na(lifeStage)] <- 'unspecified' #' replace NA values
  lifeStage <- tolower(lifeStage) #' all lower case
  lifeStage <- gsub(';', '', lifeStage) #' omit needless semi-colons
  initialSpaces <- any(substr(lifeStage, 1, 1) == ' ') #' omit opening spaces
  while(initialSpaces){
    whichInitialSpaces <- substr(lifeStage, 1, 1) == ' '
    lifeStage[whichInitialSpaces] <- substr(lifeStage[whichInitialSpaces],
                                            2, nchar(lifeStage[whichInitialSpaces]))
    initialSpaces <- any(substr(lifeStage, 1, 1) == ' ')
  }
  
  #' Standardise unspecified life stages
  unspecifiedLifeStages <- c('unspecified', 'not specified', 'undetermined',
                             'not stated', 'undefined', 'indeterminable', 'other',
                             'gt_15mm')
  lifeStage[lifeStage %in% unspecifiedLifeStages] <- 'unspecified'; rm('unspecifiedLifeStages')
  isUnspecifiedLifeStage <- lifeStage == 'unspecified' #' records lacking info on development stage
  
  #' Use the sex field to fill in missing values where possible. Records classed
  #' as male/female/male and female are all life stage C6 unless recorded
  #' otherwise.
  knownSex <- dat$sex != 'unknown'
  # unique(lifeStage[knownSex])
  lifeStage[isUnspecifiedLifeStage & knownSex] <- 'adult'
  isUnspecifiedLifeStage <- lifeStage == 'unspecified'
  
  isEggs <- lifeStage == 'eggs'
  isNauplius <- grepl('naupli' ,lifeStage) | grepl('larva' ,lifeStage)
  
  
  isC1 <- lifeStage %in% c('c1', 'c1: copepodite i',   'ci',   'c i',   'st. i',   'copepodites c1', 'copepodid i',   'i',   'stage i',   '1')
  isC2 <- lifeStage %in% c('c2', 'c2: copepodite ii',  'cii',  'c ii',  'st. ii',  'copepodites c2', 'copepodid ii',  'ii',  'stage ii',  '2')
  isC3 <- lifeStage %in% c('c3', 'c3: copepodite iii', 'ciii', 'c iii', 'st. iii', 'copepodites c3', 'copepodid iii', 'iii', 'stage iii', '3')
  isC4 <- lifeStage %in% c('c4', 'c4: copepodite iv',  'civ',  'c iv',  'st. iv',  'copepodites c4', 'copepodid iv',  'iv',  'stage iv',  '4' )
  isC5 <- lifeStage %in% c('c5', 'c5: copepodite v',   'cv',   'c v',   'st. v',   'copepodites c5', 'copepodid v',   'v',   'stage v',   '5')
  isC1toC3 <- grepl('c1-c3', lifeStage) | grepl('i-iii', lifeStage) | grepl('c1-3', lifeStage)
  isC3toC5 <- grepl('c3-c5', lifeStage) | lifeStage == 'immature stage v and iii'
  isC4toC5 <- grepl('c4-c5', lifeStage) | grepl('iv-v', lifeStage)
  isC4toAdult <- grepl('c4-6', lifeStage) | grepl('c4-adult', lifeStage)
  isC5toAdult <- grepl('c5-adult', lifeStage)
  
  isCopepodite <- isC1 | isC2 | isC3 | isC4 | isC5 | isC1toC3 | isC3toC5 | isC4toC5 | 
    grepl('copepodite', lifeStage) | grepl('copepodid', lifeStage) | lifeStage == 'c' | 
    lifeStage == 'c i-v'
  
  isCopepoditeAndAdult <- isC4toAdult | isC5toAdult
  
  lifeStage <- gsub('juvenille', 'juvenile', lifeStage) #' correct spelling
  lifeStage[grepl('juvenile', lifeStage)] <- 'juvenile'
  isJuvenile <- isEggs | isNauplius | isCopepodite | 
    grepl('juvenile', lifeStage) | grepl('immature', lifeStage)
  
  isAdult <- !{isUnspecifiedLifeStage | isEggs | isNauplius | isCopepodite | 
      isCopepoditeAndAdult | isJuvenile}
  
  isJuvenilesAndAdults <- !{isJuvenile | isAdult | isUnspecifiedLifeStage}
  
  #' Maturity: juvenile, adult, juveniles and adults, unspecified
  isMaturityDefined <- all(isJuvenile | isJuvenilesAndAdults | isAdult | isUnspecifiedLifeStage)
  isMaturityConsistent <- all(range(isJuvenile + isAdult + isJuvenilesAndAdults + isUnspecifiedLifeStage) == 1)
  # isMaturityDefined
  # isMaturityConsistent
  
  dat$maturity[isUnspecifiedLifeStage] <- 'unspecified'
  dat$maturity[isJuvenile] <- 'juvenile'
  dat$maturity[isJuvenilesAndAdults] <- 'juveniles and adults'
  dat$maturity[isAdult] <- 'adult'
  #' Check updated names
  # d <- unique(data.frame(old = lifeStage, new = dat$maturity))
  # print(d[order(d$new),], row.names = FALSE)
  
  
  #' LifeStage: nauplius, copepodite, adult, unspecified
  isLifeStageDefined <- all(isEggs | isNauplius | isCopepodite | 
                              isCopepoditeAndAdult | isAdult | 
                              isUnspecifiedLifeStage)
  # isLifeStageDefined
  # unique(lifeStage[!{isEggs | isNauplius | isCopepodite | isCopepoditeAndAdult | isAdult | isUnspecifiedLifeStage}])
  isUnspecifiedLifeStage <- isUnspecifiedLifeStage | lifeStage == 'juvenile' #' update the unspecified category to account for crude values
  isLifeStageDefined <- all(isEggs | isNauplius | isCopepodite | 
                              isCopepoditeAndAdult | isAdult | 
                              isUnspecifiedLifeStage)
  # isLifeStageDefined
  isLifeStageConsistent <- all(range(isEggs + isNauplius + isCopepodite + 
                                       isCopepoditeAndAdult + isAdult + 
                                       isUnspecifiedLifeStage) == 1)
  # isLifeStageConsistent
  
  dat$lifeStage[isUnspecifiedLifeStage] <- 'unspecified'
  dat$lifeStage[isEggs] <- 'egg'
  dat$lifeStage[isNauplius] <- 'nauplius'
  dat$lifeStage[isCopepodite] <- 'copepodite'
  dat$lifeStage[isCopepoditeAndAdult] <- 'copepodite and adult'
  dat$lifeStage[isAdult] <- 'adult'
  #' Check updated names
  # d <- unique(data.frame(old = lifeStage, new = dat$lifeStage))
  # print(d[order(d$new),], row.names = FALSE)
  
  # CopepoditeStage: C1, C2, C3', C4, C5, C6, ...
  isDetailedCopepoditeStage <- isC1 | isC2 | isC3 | isC4 | isC5 | isC1toC3 | isC3toC5 | isC4toC5 | isCopepoditeAndAdult | isAdult
  isUnspecifiedCopepodite <- isCopepodite & !isDetailedCopepoditeStage
  isUnspecifiedLifeStage <- isUnspecifiedLifeStage | isUnspecifiedCopepodite #' update the unspecified category to account for crude values
  isCopepoditeStageDefined <- all(isDetailedCopepoditeStage | isEggs | isNauplius | 
                                    isUnspecifiedLifeStage)
  isCopepoditeStageConsistent <- all(range(isDetailedCopepoditeStage + isEggs + 
                                             isNauplius + isUnspecifiedLifeStage) == 1)
  # isCopepoditeStageDefined
  # isCopepoditeStageConsistent
  
  dat$copepoditeStage[isUnspecifiedLifeStage] <- 'unspecified'
  dat$copepoditeStage[isEggs] <- 'not copepodite'
  dat$copepoditeStage[isNauplius] <- 'not copepodite'
  dat$copepoditeStage[isC1] <- 'C1'
  dat$copepoditeStage[isC2] <- 'C2'
  dat$copepoditeStage[isC3] <- 'C3'
  dat$copepoditeStage[isC4] <- 'C4'
  dat$copepoditeStage[isC5] <- 'C5'
  dat$copepoditeStage[isC1toC3] <- 'C1-C3'
  dat$copepoditeStage[isC3toC5] <- 'C3-C5'
  dat$copepoditeStage[isC4toC5] <- 'C4-C5'
  dat$copepoditeStage[isC4toAdult] <- 'C4-C6'
  dat$copepoditeStage[isC5toAdult] <- 'C5-C6'
  dat$copepoditeStage[isAdult] <- 'C6'
  #' Check updated names
  # d <- unique(data.frame(old = lifeStage, new = dat$copepoditeStage))
  # print(d[order(d$new),], row.names = FALSE)
  
  rm('lifeStage') # remove large unwanted variables
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise and extract date/time data
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' The date_start/mid/end columns contain dates as UNIX time stamps in
  #' milliseconds since 1/1/1970. Use these columns to extract dates.
  #' The eventDate and eventTime columns contain dates/times, but in inconsistent
  #' formats. Still potentially useful for filling in NA values in the other date
  #' columns.
  
  message('\n', "Standardise 'date/time'")
  
  #' These fields give date but not time, and some entries are missing
  or <- '1970-01-01'
  fr <- '%Y-%m-%d'
  dat$date_start <- format(as.POSIXlt(dat$date_start/1000, origin = or), fr)
  dat$date_mid <- format(as.POSIXlt(dat$date_mid/1000, origin = or), fr)
  dat$date_end <- format(as.POSIXlt(dat$date_end/1000, origin = or), fr)
  
  #' Index rows missing dates specified as UNIX time stamps
  is.na.date <- apply(as.data.frame(dat)[,c('date_start','date_mid','date_end')],
                      1, function(z) all(is.na(z)))
  
  #' Use the eventDate/Time columns to infill these dates if possible.
  eventDate <- dat$eventDate
  eventTime <- dat$eventTime
  eventStart <- character(length(eventDate))
  eventEnd <- character(length(eventDate))
  
  #' Regularise format of times to H:M:S
  iTimes <- !{grepl(':', eventTime) | is.na(eventTime)}
  reformatTimes <- eventTime[iTimes]
  reformatMins <- substr(reformatTimes, nchar(reformatTimes)-1, nchar(reformatTimes))
  reformatHrs <- substr(reformatTimes, 1, 2)
  reformatHrs[nchar(reformatTimes) == 3] <- paste0('0',substr(reformatHrs[nchar(reformatTimes) == 3], 1, 1))
  reformatHrs[nchar(reformatTimes) == 2] <- '00'
  reformatTimes <- paste(reformatHrs, reformatMins, '00', sep = ':')
  eventTime[iTimes] <- reformatTimes
  reformatTimes <- strsplit(eventTime, '\\+')
  eventTime <- sapply(reformatTimes, function(z) z[1])
  
  #' Regularise format of dates to Y-m-d, or Y-m-d H:M:S for those that have times
  eventDate <- gsub('T', ' ', eventDate) #' swap placeholder 'T' for space
  eventDate <- gsub('Z', '', eventDate) #' remove  trailing 'Z', which stands for zero added time (UTC)
  
  #' Numerous formats -- work through them from least to most complicated
  fr2 <- '%Y-%m-%d %H:%M:%S'
  # sort(unique(nchar(eventDate)))
  eventDateIncomplete <- rep(FALSE, length(eventDate))
  i <- nchar(eventDate) == 4 # Y
  eventDateIncomplete[i] <- TRUE
  i <- nchar(eventDate) == 7 # Y-m (not a complete date)
  eventDateIncomplete[i] <- TRUE
  i <- nchar(eventDate) == 8 # Y-m-d
  if(any(i)){
    eventDate[i] <- format(strptime(eventDate[i], format = fr), fr) 
  }
  i <- nchar(eventDate) == 9 # Y-m-d or d/m/y
  if(any(i)){
    j <- grepl('-', eventDate)
    eventDate[i&j] <- format(strptime(eventDate[i&j], format = fr), fr)
    j <- grepl('/', eventDate)
    eventDate[i&j] <- format(strptime(eventDate[i&j], format = '%d/%m/%Y'), fr)
  }
  i <- nchar(eventDate) == 10 # Y-m-d or d/m/y
  if(any(i)){
    j <- grepl('-', eventDate)
    eventDate[i&j] <- format(strptime(eventDate[i&j], format = fr), fr)
    j <- grepl('/', eventDate)
    eventDate[i&j] <- format(strptime(eventDate[i&j], format = '%d/%m/%Y'), fr)
  }
  i <- nchar(eventDate) == 13 # Y-m-d+H (time adjustment but no time!)
  if(any(i)){
    x <- strsplit(eventDate[i], '\\+')
    eventDate[i] <- sapply(x, function(z) z[1])
  }
  i <- nchar(eventDate) == 15 # Y-m-d HM or Y-M/Y-M(start/end)
  if(any(i)){
    eventStart <- character(length(eventDate))
    eventEnd <- character(length(eventDate))
    j <- grepl('/', eventDate)
    x <- strsplit(eventDate[i&j], '/')
    eventStart[i&j] <- sapply(x, function(z) z[1])
    eventEnd[i&j] <- sapply(x, function(z) z[2])
    eventDateIncomplete[i&j] <- TRUE
    eventDate[i&j] <- ''
    j <- grepl(' ', eventDate)
    x <- strsplit(eventDate[i&j], ' ')
    x <- sapply(x, function(z) paste(z[1], paste(substr(z[2], 1, 2), substr(z[2], 3, 4), sep = ':')))
    eventDate[i&j] <- format(strptime(x, format = '%Y-%m-%d %H:%M'), fr2)
  }
  i <- nchar(eventDate) == 16 # Y-m-d H:M
  if(any(i)){
    j <- !grepl(' ', eventDate)
    x <- regexpr(':', eventDate[i&j])
    eventDate[i&j] <- format(strptime(paste(substr(eventDate[i&j], 1, x-1), 
                                            substr(eventDate[i&j], x+1, nchar(eventDate[i&j]))),
                                      format = '%Y-%m-%d %H:%M'), fr2)
    eventDate[i] <- format(strptime(eventDate[i], format = '%Y-%m-%d %H:%M'), fr2)
  }
  i <- nchar(eventDate) == 19 # Y-m-d H:M or Y-m-d H:M+H
  if(any(i)){
    j <- grepl('\\+', eventDate)
    x <- strsplit(eventDate[i&j], '\\+') #' adjust for time where explicitly given in the data
    adj <- as.numeric(sapply(x, function(z) z[2]))
    x <- sapply(x, function(z) z[1])
    x <- strptime(x, format = '%Y-%m-%d %H:%M')
    x <- x + adj*60^2
    eventDate[i&j] <- format(x, fr2)
    eventDate[i] <- format(strptime(eventDate[i], format = fr2), fr2)
  }
  i <- nchar(eventDate) == 21 # Y-m-d/Y-m-d(start/end)
  if(any(i)){
    x <- strsplit(eventDate[i], '/')
    eventStart[i] <- format(strptime(sapply(x, function(z) z[1]), format = fr), fr)
    eventEnd[i] <- format(strptime(sapply(x, function(z) z[2]), format = fr), fr)
    eventDate[i] <- ''
  }
  i <- nchar(eventDate) == 22 # Y-m-d H:M:S+H
  if(any(i)){
    j <- grepl('\\+', eventDate)
    x <- strsplit(eventDate[i&j], '\\+')
    adj <- as.numeric(sapply(x, function(z) z[2]))
    x <- sapply(x, function(z) z[1])
    x <- strptime(x, format = fr2)
    eventDate[i&j] <- format(x + adj*60^2, fr2)
    eventDate[i] <- format(strptime(eventDate[i], format = fr2), fr2)
  }
  i <- nchar(eventDate) == 23 # Y-m-d / Y-m-d (start / end)
  if(any(i)){
    x <- strsplit(eventDate[i], ' / ')
    eventStart[i] <- format(strptime(sapply(x, function(z) z[1]), format = fr), fr)
    eventEnd[i] <- format(strptime(sapply(x, function(z) z[2]), format = fr), fr)
    eventDate[i] <- ''
  }
  i <- nchar(eventDate) == 24 # Y-m-d/Y-m-d+H (start/end) (adjustment but no time!)
  if(any(i)){
    x <- sapply(strsplit(eventDate[i], '\\+'), function(z) z[1])
    x <- strsplit(x, '/')
    eventStart[i] <- format(strptime(sapply(x, function(z) z[1]), format = fr), fr)
    eventEnd[i] <- format(strptime(sapply(x, function(z) z[2]), format = fr), fr)
    eventDate[i] <- ''
  }
  i <- nchar(eventDate) == 25 # Y-m-d H:M:S+H:M
  if(any(i)){
    j <- grepl('\\+', eventDate)
    x <- strsplit(eventDate[i&j], '\\+')
    adj <- sapply(x, function(z) z[2])
    x <- sapply(x, function(z) z[1])
    x <- strptime(x, format = fr2)
    adj <- strsplit(adj, ':')
    adjh <- as.numeric(sapply(adj, function(z) z[1]))
    adjm <- as.numeric(sapply(adj, function(z) z[2]))
    adjs <- adjh*60^2 + adjm*60
    x <- x + adjs
    eventDate[i&j] <- format(x, fr2)
  }
  i <- nchar(eventDate) == 33 # Y-m-d H:M/Y-m-d H:M (start/end)
  if(any(i)){
    x <- strsplit(eventDate[i], '/')
    eventStart[i] <- format(strptime(sapply(x, function(z) z[1]), format = '%Y-%m-%d %H:%M'), fr2)
    eventEnd[i] <- format(strptime(sapply(x, function(z) z[2]), format = '%Y-%m-%d %H:%M'), fr2)
    eventDate[i] <- ''
  }
  i <- nchar(eventDate) == 36 # Y-m-d H:M/Y-m-d H:M+H (start/end)
  if(any(i)){
    j <- grepl('\\+', eventDate)
    x <- strsplit(eventDate[i&j], '\\+')
    adj <- as.numeric(sapply(x, function(z) z[2]))
    x <- sapply(x, function(z) z[1])
    x <- strsplit(x, '/')
    eventStart[i&j] <- format(strptime(sapply(x, function(z) z[1]), format = '%Y-%m-%d %H:%M') + adj*60^2, fr2)
    eventEnd[i&j] <- format(strptime(sapply(x, function(z) z[2]), format = '%Y-%m-%d %H:%M') + adj*60^2, fr2)
    eventDate[i&j] <- ''
  }
  i <- nchar(eventDate) == 39 # Y-m-d H:M+H/Y-m-d H:M+H (start/end)
  if(any(i)){
    x <- strsplit(eventDate[i], '/')
    xs <- sapply(x, function(z) z[1])
    xe <- sapply(x, function(z) z[2])
    xs <- strsplit(xs, '\\+')
    xe <- strsplit(xe, '\\+')
    adjs <- as.numeric(sapply(xs, function(z) z[2]))
    adje <- as.numeric(sapply(xe, function(z) z[2]))
    xs <- sapply(xs, function(z) z[1])
    xe <- sapply(xe, function(z) z[1])
    eventStart[i] <- format(strptime(xs, format = '%Y-%m-%d %H:%M') + adjs*60^2, fr2)
    eventEnd[i] <- format(strptime(xe, format = '%Y-%m-%d %H:%M') + adje*60^2, fr2)
    eventDate[i] <- ''
  }
  
  #' Infill dates & times missing from some fields
  i <- is.na(eventDate) | eventDate == ''
  j <- !is.na(dat$date_mid)
  k <- i&j
  eventDate[k] <- dat$date_mid[k]
  
  i <- is.na(eventStart) | eventStart == ''
  j <- !is.na(dat$date_start)
  k <- i&j
  eventStart[k] <- dat$date_start[k]
  
  i <- is.na(eventEnd) | eventEnd == ''
  j <- !is.na(dat$date_end)
  k <- i&j
  eventEnd[k] <- dat$date_end[k]
  
  i <- is.na(dat$date_mid) | dat$date_mid == ''
  j <- !is.na(eventDate) & eventDate != '' & !eventDateIncomplete
  k <- i&j
  dat$date_mid[k] <- format(strptime(eventDate[k], format = fr), fr)
  
  i <- is.na(dat$date_start) | dat$date_start == ''
  j <- !is.na(eventStart) & eventStart != '' & !eventDateIncomplete
  k <- i&j
  dat$date_start[k] <- format(strptime(eventStart[k], format = fr), fr)
  
  i <- is.na(dat$date_end) | dat$date_end == ''
  j <- !is.na(eventEnd) & eventEnd != '' & !eventDateIncomplete
  k <- i&j
  dat$date_end[k] <- format(strptime(eventEnd[k], format = fr), fr)
  
  i <- is.na(eventTime) | eventTime == ''
  j <- !is.na(eventDate) & eventDate != '' & !eventDateIncomplete
  k <- i&j
  eventTime[k] <- format(strptime(eventDate[k], format = fr2), '%H:%M:%S')
  
  dat$time_mid <- eventTime
  dat$time_start <- format(strptime(eventStart, format = fr2), '%H:%M:%S')
  dat$time_end <- format(strptime(eventEnd, format = fr2), '%H:%M:%S')
  
  #' Times recorded as 00:00:00 or 12:00:00 are suspicious, and it's tempting to
  #' remove them, but lots of times were reported to the hour, so even though
  #' there's an overabundance of 00:00:00 and 12:00:00, they should not be deemed
  #' as unrecorded times unless all times reported to the hour are also removed.
  # x1 <- substr(dat$time_mid, 1, 2)
  # x2 <- substr(dat$time_mid, 4, 8)
  # x3 <- x1[x2 == '00:00']
  # table(x3)
  
  dat$date_mid[dat$date_mid == ''] <- NA
  dat$date_start[dat$date_start == ''] <- NA
  dat$date_end[dat$date_end == ''] <- NA
  dat$time_mid[dat$time_mid == ''] <- NA
  dat$time_start[dat$time_start == ''] <- NA
  dat$time_end[dat$time_end == ''] <- NA
  
  dat$eventTime <- NULL
  dat$eventDate <- NULL
  
  #' Remove rows lacking date
  dat <- dat[!is.na(dat$date_mid),]
  
  #' Include some extra columns
  dat$year <- as.numeric(strftime(dat$date_mid, '%Y'))
  dat$month <- strftime(dat$date_mid, '%b')
  dat$dayOfYear <- as.numeric(strftime(dat$date_mid, '%j'))
  
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[is.na(dat$time_mid) | dat$time_mid == ''] <- ''
  
  #' ~~~~~~~~~~~~~~~~~~~~~~
  #' Sampling gear/protocol
  #' ~~~~~~~~~~~~~~~~~~~~~~
  
  #' The 'samplingProtocol' field contains info on gear used to collect data, and
  #' this is supplemented by the 'collectionCode' field -- there may also be some
  #' useful info in the fieldNotes field.
  
  message('\n', "Standardise 'sample gear'")
  
  gear <- dat$samplingProtocol
  code <- dat$collectionCode
  note <- dat$fieldNotes
  # sort(unique(gear))
  # sort(unique(code))
  # sort(unique(note))
  
  unknownGear <- is.na(gear)
  # sort(unique(code[unknownGear])) #' collection codes for records lacking info on sampling protocol
  
  #' Collection codes can be used to inform missing info CPR and WP-2 gears.
  CPR_codes <- unique(code[grepl('cpr', code, ignore.case = TRUE)]) #' unique 'CPR' collection codes
  gear[unknownGear & code %in% CPR_codes] <- 'CPR'
  
  WP_codes <- unique(code[grepl('wp', code, ignore.case = TRUE)]) #' unique 'WP-2' collection codes
  gear[unknownGear & code %in% WP_codes] <- 'WPII'
  
  unknownGear <- is.na(gear)
  # sort(unique(note[unknownGear])) #' field notes for records lacking info on sampling protocol
  
  net_notes <- unique(note[grepl('net', note, ignore.case = TRUE)])
  gear[unknownGear & note %in% net_notes] <- 'net'
  
  diving_notes <- unique(note[grepl('diving', note, ignore.case = TRUE)])
  diving_notes <- diving_notes[!diving_notes %in% net_notes]
  gear[unknownGear & note %in% diving_notes] <- 'diving'
  
  unknownGear <- is.na(gear)
  
  # sort(unique(gear))
  
  #' Some records provide a cruise report or reference link for sampling protocol.
  #' Store this info in a seperate column.
  isInCruiseReport <- grepl('report', gear, ignore.case = TRUE) | 
    grepl('http', gear, ignore.case = TRUE) | grepl('doi', gear, ignore.case = TRUE)
  dat$cruiseReport <- gear
  dat$cruiseReport[!isInCruiseReport] <- NA
  gear[isInCruiseReport] <- 'see cruise report'
  
  isCPR <- grepl('cpr', gear, ignore.case = TRUE)
  isBongo <- grepl('bongo', gear, ignore.case = TRUE)
  isNansen <- grepl('nansen', gear, ignore.case = TRUE)
  isMulti <- grepl('multi', gear, ignore.case = TRUE)
  isPump <- grepl('pump', gear, ignore.case = TRUE)
  isJuday <- grepl('juday', gear, ignore.case = TRUE)
  isWP2 <- grepl('wp', gear, ignore.case = TRUE)
  isMOCNESS <- grepl('moc', gear, ignore.case = TRUE)
  isRing <- grepl('ring', gear, ignore.case = TRUE) &
    !{grepl('measuring', gear, ignore.case = TRUE) | 
        grepl('syringe', gear, ignore.case = TRUE)}
  isDredge <- grepl('dredge', gear, ignore.case = TRUE)
  isHandCollected <- grepl('hand', gear, ignore.case = TRUE) | 
    grepl('diving', gear, ignore.case = TRUE)
  isShore <- grepl('shore', gear, ignore.case = TRUE) | 
    grepl('tide line', gear, ignore.case = TRUE) |
    grepl('mid-littoral', gear, ignore.case = TRUE)
  isBogorov <- grepl('bogorov', gear, ignore.case = TRUE)
  isClarke <- grepl('clarke', gear, ignore.case = TRUE)
  isStramin <- grepl('stramin', gear, ignore.case = TRUE)
  isApstein <- grepl('apstein', gear, ignore.case = TRUE)
  isDiscoveryN70 <- grepl('discovery', gear, ignore.case = TRUE)
  isLachlan50 <- grepl('lachlan', gear, ignore.case = TRUE)
  isAgassiz <- grepl('agassiz', gear, ignore.case = TRUE)
  isIsaaksKidd <- grepl('isaaks', gear, ignore.case = TRUE)
  isSligsbyGorbunov <- grepl('sligsby', gear, ignore.case = TRUE)
  isMenzies <- grepl('menzies', gear, ignore.case = TRUE)
  isOther <- grepl('longline', gear, ignore.case = TRUE) | 
    grepl('bottle', gear, ignore.case = TRUE) | 
    grepl('traps', gear, ignore.case = TRUE)
  
  x <- unknownGear | isInCruiseReport | isCPR | isBongo | isNansen | isMulti | isPump |
    isJuday | isWP2 | isMOCNESS | isRing | isDredge | isHandCollected | isShore | isBogorov |
    isClarke | isStramin | isApstein | isDiscoveryN70 | isLachlan50 | isAgassiz | isIsaaksKidd |
    isSligsbyGorbunov | isMenzies | isOther # | isTrawl
  
  # sort(unique(gear[!x]))
  
  isUnspecifiedNet <- gear %in% unique(gear[!x])
  
  x <- unknownGear + isInCruiseReport + isCPR + isBongo + isNansen + isMulti + isPump +
    isJuday + isWP2 + isMOCNESS + isRing + isDredge + isHandCollected + isShore + isBogorov +
    isClarke + isStramin + isApstein + isDiscoveryN70 + isLachlan50 + isAgassiz + isIsaaksKidd +
    isSligsbyGorbunov + isMenzies + isOther + isUnspecifiedNet
  
  gearDefined4AllRecords <- all(x > 0)
  # cat(paste('\ngear defined for all records:', gearDefined4AllRecords,'\n\n'))
  gearUnique4AllRecords <- all(x < 2)
  # cat(paste('\ngear uniquely defined for all records:', gearUnique4AllRecords,'\n\n'))
  
  gear[unknownGear] <- 'unknown gear'
  gear[isCPR] <- 'CPR'
  gear[isBongo] <- 'BONGO'
  gear[isNansen] <- 'Nansen'
  gear[isMulti] <- 'multinet'
  gear[isPump] <- 'water pump'
  gear[isJuday] <- 'Juday'
  gear[isWP2] <- 'WPII'
  gear[isMOCNESS] <- 'MOCNESS'
  gear[isRing] <- 'ring net'
  gear[isDredge] <- 'dredge'
  gear[isHandCollected] <- 'hand collected'
  gear[isShore] <- 'on shore'
  gear[isBogorov] <- 'Bogorov-Rass'
  gear[isClarke] <- 'Clarke-Bumpus'
  gear[isStramin] <- 'Stramin'
  gear[isApstein] <- 'Apstein'
  gear[isDiscoveryN70] <- 'Discovery N70'
  gear[isLachlan50] <- 'Lachlan N50'
  gear[isAgassiz] <- 'Agassiz'
  gear[isIsaaksKidd] <- 'Isaac-Kidd'
  gear[isSligsbyGorbunov] <- 'Sligsby-Gorbunov'
  gear[isMenzies] <- 'Menzies'
  gear[isOther] <- 'Other'
  gear[isUnspecifiedNet] <- 'Unspecified net'
  
  #' Compare standardised gear names to originals
  d <- unique(data.frame(original_name = dat$samplingProtocol, standardised_name = gear,
                         collectionCode = code, fieldNotes = note))
  d <- do.call('rbind', lapply(1:nrow(d), function(z, n)
    matrix(d[z,], nrow = 1, dimnames = list(NULL, names(d))), n = names(d)))
  # print(d[order(unlist(d[,'standardised_name'])),])
  
  dat$samplingProtocol <- gear
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise units of organism quantity
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'abundance'")
  
  organismQuantity <- dat$organismQuantity
  individualCount <- dat$individualCount
  quantType <- dat$organismQuantityType
  # unique(quantType) # there are volumetric concentrations, area densities, total numbers, presence/absence, and others...
  quantType[is.na(quantType)] <- ''
  
  concentrationLabels <- c('Abundance per cubic metre', 'number of individuals per 100 cubic meter',
                           'Quantity per cubic metre', 'number of individuals per cubic meter',
                           'Individuals per litre', 'abundance per cubic metre',
                           'taxon per cubic metre', 'Number per cubic metre')
  #' convert concentrations to units of individuals per cubic metre
  i <- quantType == 'number of individuals per 100 cubic meter'
  organismQuantity[i] <- organismQuantity[i] * 1e-2
  i <- quantType == 'Individuals per litre'
  organismQuantity[i] <- organismQuantity[i] * 1e3
  quantType[quantType %in% concentrationLabels] <- 'individuals / m3' #' regularise the concentration labels
  
  densityLabels <- c('abundance per m2')
  quantType[quantType %in% densityLabels] <- 'individuals / m2'
  
  totalLabels <- c('individuals', 'number of individuals', 'abundance?')
  quantType[quantType %in% totalLabels] <- 'individuals'
  missingQuant <- is.na(organismQuantity) #' substitute individual counts from another column
  subQuant <- missingQuant & !is.na(individualCount)
  organismQuantity[subQuant] <- individualCount[subQuant]
  quantType[subQuant] <- 'individuals'
  
  dat$organismQuantity <- organismQuantity
  dat$organismQuantityType <- quantType
  dat <- dat[,names(dat) != 'individualCount']
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  i <- lat_lim[1] <= dat$decimalLatitude & dat$decimalLatitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' ~~~~~~~~~~~~~
  #' Order by time
  #' ~~~~~~~~~~~~~
  dat <- as.data.frame(dat)
  dat$geometry <- NULL
  dat <- dat[order(dat$date_mid, dat$time_mid, dat$time_start),]
  
  #' ~~~~~~~~~~~~~~~~~~~
  #' Assign sample event
  #' ~~~~~~~~~~~~~~~~~~~
  #' Use info on gear, coordinates, date, and time. Use a time increment of 30
  #' minutes to separate events, and round  coordinates to 2 decimal places.
  dat$no.time <- is.na(dat$time_mid) | dat$time_mid == ''
  dat$time_mid[dat$no.time] <- '12:00:00'
  dat$time.inc <- dat$time_mid
  dat$time_mid[dat$no.time] <- NA
  i <- is.na(dat$time.inc)
  dat$time.inc[i] <- dat$time_start[i]
  inc <- 30
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(floor(mins / inc))
  }, inc = inc)
  
  dat$lon.r <- round(dat$decimalLongitude, digits = 2)
  dat$lat.r <- round(dat$decimalLatitude, digits = 2)
  
  x <- dat %>%
    select(samplingProtocol, samplingEffort, basisOfRecord, eventID, date_mid,
           time.inc, lon.r, lat.r) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    select(-time.inc, -lon.r, -lat.r) %>%
    relocate(Sample.event, .before = species)
  
  dat$no.time <- NULL
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n------------------\n',
          'Finished OBIS data', 
          '\n------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  
  # GBIF --------------------------------------------------------------------
  
  Source <- 'GBIF'
  
  message('\n------------------\n',
          'Cleaning GBIF data',
          '\n------------------')
  
  
  message('\n', "Omit empty rows/columns")
  
  dat <- DATA[[Source]]
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  noName <- is.na(dat$species) #' missing species name
  noPosition <- is.na(dat$decimalLongitude) | is.na(dat$decimalLatitude) #' missing location info
  noDepth <- is.na(dat$depth)
  noYrMonDay <- is.na(dat$year) & is.na(dat$month) & is.na(dat$day)
  noDate <- is.na(dat$eventDate) | dat$eventDate == ''
  noDate <- noDate & noYrMonDay  #' missing dates
  dat$verbatimEventDate <- gsub('- ', '', dat$verbatimEventDate) #' verbatim dates are cumbersome but usable
  dat$verbatimEventDate <- gsub('-', '', dat$verbatimEventDate)
  dat$verbatimEventDate <- gsub(' 1', '_1', dat$verbatimEventDate)
  dat$verbatimEventDate <- gsub(' 2', '_2', dat$verbatimEventDate)
  dat$verbatimEventDate <- gsub(' ', '', dat$verbatimEventDate)
  dat$verbatimEventDate <- gsub('_', ' ', dat$verbatimEventDate)
  dat$verbatimEventDate <- gsub('unknown', '', dat$verbatimEventDate)
  infillVerbatimDate <- noDate & !dat$verbatimEventDate == ''
  y <- as.numeric(substr(dat$verbatimEventDate, nchar(dat$verbatimEventDate)-3, nchar(dat$verbatimEventDate)))
  m <- tolower(substr(dat$verbatimEventDate, 1, 3))
  m <- outer(m, tolower(month.abb), '==')
  m <- apply(m, 1, which.max)
  dat$year[infillVerbatimDate] <- y[infillVerbatimDate]
  dat$month[infillVerbatimDate] <- m[infillVerbatimDate]
  noDate <- noDate & !infillVerbatimDate
  omitRecords <- noName | noPosition | noDepth | noDate
  dat <- dat[!omitRecords,]
  
  #' Filter data by species to omit those with only a few records
  
  message('\n', "Omit poorly sampled species")
  
  recordsPerSpecies <- countRecordsPerSpecies(dat, 'species')
  
  #' Create some visual display of which species are most recorded...
  propRecords <- cumulativeProportion(recordsPerSpecies)
  
  #' Data may be reduced by retaining only the species that have the most records.
  #' This is controlled by `propRecords2Keep`, which retains the most sampled
  #' species accounting for `propRecords2Keep` * 100% of the records.
  
  # propRecords2Keep <- 0.8
  
  speciesSelected <- chooseSpeciesBySampleSize(
    recordsPerSpecies, propRecords, propRecords2Keep = propRecords2Keep)
  
  #' Update data to retain only the most sampled species
  dat <- filterDataBySpecies(dat, speciesSelected, 'species')
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Omit records lacking information on depth
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  recordDepth <- !is.na(dat$depth)
  noDepthInfo <- !recordDepth
  dat <- dat[!noDepthInfo,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise the sex field
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'sex'")
  
  sex <- dat$sex
  # unique(sex)
  isUnspecifiedSex <- is.na(sex) | sex ==''
  sex[isUnspecifiedSex] <- 'unknown'
  knownSex <- dat$sex != 'unknown'
  sex <- tolower(sex)
  isMale <- sex == 'male'
  isFemale <- sex == 'female'
  dat$sex <- sex #' assign cleaned variable to data frame
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise the life stage field
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' Compared to OBIS, the GBIF data appear to have less detail on life stage,
  #' particularly copepodite stage.
  
  message('\n', "Standardise 'life stage'")
  
  #' Search for data fields that may contain info on copepodite stage
  search4fields <- function(term, data, ignore.case = TRUE){
    x <- sapply(data, function(z) any(grepl(term, z, ignore.case = ignore.case)))
    names(data)[x]}
  x <- search4fields('copepodite', dat)
  # print(x)
  # print(unique(dat[,x][grepl('copepodite', dat[,x], ignore.case = TRUE)]))
  
  d <- unique(as.data.frame(dat)[c('lifeStage','identificationQualifier','sex')])
  # print(d)
  # The useful fields are 'lifeStage', 'sex', and perhaps 'identificationQualifier'
  dat$identificationQualifier[dat$identificationQualifier %in% c('uncertain', 'Species', '?')] <- ''
  
  lifeStage <- dat$lifeStage
  # unique(lifeStage)
  
  #' Though there's less detail than OBIS, following the same procedure for
  #' categorising life stage, capturing the detail within three fields:
  #' (1) maturity = c('juvenile', 'adult')
  #' (2) lifeStage = c('nauplius', 'copepodite', 'adult')
  #' (3) copepoditeStage = c('C1','C2','C3','C4','C5', 'C6', ...)
  n <- names(dat)
  ni <- which(n == 'lifeStage')
  n <- c(n[1:{ni-1}], 'maturity', 'lifeStage', 'copepoditeStage', n[-{1:ni}])
  dat$maturity <- NA
  dat$lifeStage <- NA
  dat$copepoditeStage <- NA
  dat <- dat[,n]
  
  #' Collect records with unknown life stage under 'unspecified'
  lifeStage[is.na(lifeStage) | lifeStage == ''] <- 'unspecified' #' replace blanks
  lifeStage <- tolower(lifeStage) #' all lower case
  
  #' Group unspecified life stages
  unspecifiedLifeStages <- c('unspecified', 'medusa')
  lifeStage[lifeStage %in% unspecifiedLifeStages] <- 'unspecified'; rm('unspecifiedLifeStages')
  isUnspecifiedLifeStage <- lifeStage == 'unspecified' #' records lacking info on development stage
  
  #' Use the identificationQualifier field to fill in some missing values.
  stageInfo <- dat$identificationQualifier != ''
  # unique(lifeStage[stageInfo])
  lifeStage[isUnspecifiedLifeStage & stageInfo] <- dat$identificationQualifier[isUnspecifiedLifeStage & stageInfo]
  isUnspecifiedLifeStage <- lifeStage == 'unspecified'
  
  #' Use the sex field to fill in missing values where possible. Records classed 
  #' as male/female/male and female are life stage C6 unless recorded otherwise.
  dat$sex[!knownSex & lifeStage %in% c('cf','cf.')]  <- 'female'
  lifeStage[lifeStage %in% c('cf','cf.')] <- 'adult'
  knownSex <- dat$sex != 'unknown'
  unique(lifeStage[knownSex])
  lifeStage[isUnspecifiedLifeStage & knownSex] <- 'adult'
  isUnspecifiedLifeStage <- lifeStage == 'unspecified'
  
  isEggs <- grepl('egg', lifeStage)
  isNauplius <- grepl('naupli', lifeStage) | grepl('larva', lifeStage)
  
  isC1 <- lifeStage %in% c('copepodite I')
  isC2 <- lifeStage %in% c('copepodite II')
  isC3 <- lifeStage %in% c('copepodite III')
  isC4 <- lifeStage %in% c('copepodite IV')
  isC5 <- lifeStage %in% c('copepodite V')
  isC6 <- lifeStage == 'adult'
  
  isCopepodite <- isC1 | isC2 | isC3 | isC4 | isC5
  
  isAdult <- grepl('adult', lifeStage)
  isJuvenile <- isEggs | isNauplius | isCopepodite |
    grepl('juvenile', lifeStage) | grepl('immature', lifeStage)
  
  #' Maturity: juvenile, adult, juveniles and adults, unspecified
  isMaturityDefined <- all(isJuvenile | isAdult | isUnspecifiedLifeStage)
  # cat(paste('\nall records have maturity specified:', isMaturityDefined, '\n\n'))
  isMaturityConsistent <- all({isJuvenile + isAdult + isUnspecifiedLifeStage} < 2)
  # cat(paste('\nall records have unique maturity status:', isMaturityConsistent, '\n\n'))
  
  dat$maturity[isUnspecifiedLifeStage] <- 'unspecified'
  dat$maturity[isJuvenile] <- 'juvenile'
  dat$maturity[isAdult] <- 'adult'
  #' Check updated names
  # d <- unique(data.frame(old = lifeStage, new = dat$maturity))
  # print(d[order(d$new),], row.names = FALSE)
  
  
  #' LifeStage: nauplius, copepodite, adult, unspecified
  isLifeStageDefined <- all(isEggs | isNauplius | isCopepodite | isAdult |
                              isUnspecifiedLifeStage)
  # isLifeStageDefined
  # unique(lifeStage[!{isEggs | isNauplius | isCopepodite | isAdult | isUnspecifiedLifeStage}])
  isUnspecifiedLifeStage <- isUnspecifiedLifeStage |
    {lifeStage %in% c('juvenile', 'immature')} #' update the unspecified category to account for crude values
  isLifeStageDefined <- all(isEggs | isNauplius | isCopepodite | isAdult | isUnspecifiedLifeStage)
  # isLifeStageDefined
  isLifeStageConsistent <- all(range(isEggs + isNauplius + isCopepodite + isAdult + isUnspecifiedLifeStage) == 1)
  # isLifeStageConsistent
  
  dat$lifeStage[isUnspecifiedLifeStage] <- 'unspecified'
  dat$lifeStage[isEggs] <- 'egg'
  dat$lifeStage[isNauplius] <- 'nauplius'
  dat$lifeStage[isCopepodite] <- 'copepodite'
  dat$lifeStage[isAdult] <- 'adult'
  #' Check updated names
  # d <- unique(data.frame(old = lifeStage, new = dat$lifeStage))
  # print(d[order(d$new),], row.names = FALSE)
  
  
  #' CopepoditeStage: C1, C2, C3', C4, C5, C6, ...
  isDetailedCopepoditeStage <- isC1 | isC2 | isC3 | isC4 | isC5 | isAdult
  isUnspecifiedCopepodite <- isCopepodite & !isDetailedCopepoditeStage
  isUnspecifiedLifeStage <- isUnspecifiedLifeStage | isUnspecifiedCopepodite #' update the unspecified category to account for crude values
  isCopepoditeStageDefined <- all(isDetailedCopepoditeStage | isEggs | isNauplius | isUnspecifiedLifeStage)
  isCopepoditeStageConsistent <- all(range(isDetailedCopepoditeStage + isEggs + isNauplius + isUnspecifiedLifeStage) == 1)
  # isCopepoditeStageDefined
  # isCopepoditeStageConsistent
  
  dat$copepoditeStage[isUnspecifiedLifeStage] <- 'unspecified'
  dat$copepoditeStage[isEggs] <- 'not copepodite'
  dat$copepoditeStage[isNauplius] <- 'not copepodite'
  dat$copepoditeStage[isC1] <- 'C1'
  dat$copepoditeStage[isC2] <- 'C2'
  dat$copepoditeStage[isC3] <- 'C3'
  dat$copepoditeStage[isC4] <- 'C4'
  dat$copepoditeStage[isC5] <- 'C5'
  dat$copepoditeStage[isAdult] <- 'C6'
  #' Check updated names
  # d <- unique(data.frame(old = lifeStage, new = dat$copepoditeStage))
  # print(d[order(d$new),], row.names = FALSE)
  
  rm('lifeStage') # remove large unwanted variables
  
  
  #' ~~~~~
  #' Dates
  #' ~~~~~
  
  #' Some filtering has been done already -- all useful info contained in fields
  #' 'eventDate', 'year', 'month', 'day'.
  
  message('\n', "Standardise 'date/time'")
  
  names(dat)[grepl('date', names(dat), ignore.case = TRUE)]
  names(dat)[grepl('time', names(dat), ignore.case = TRUE)]
  
  eventDate <- dat$eventDate
  eventTime <- dat$eventTime
  
  eventDate <- gsub('T',' ', eventDate)
  #' Separate time from dates
  dateSplit <- strsplit(eventDate, ' ')
  x <- sapply(dateSplit, function(z) z[2])
  eventDate <- sapply(dateSplit, function(z) z[1])
  #' There are times stored in eventTime that do not appear in the eventDate field
  i0 <- x == '00:00:00' #' index suspicious times that could perhaps be infilled from eventTime
  i12 <- x == '12:00:00'
  i <- eventTime != ''
  # sort(unique(nchar(eventTime[i])))
  i <- 3 #' HM
  j <- nchar(eventTime) == i
  k <- {i0 | i12} & j
  if(any(k)){
    eventTime[k] <- paste(paste0(0, substr(eventTime[k], 1, 1)), substr(eventTime[k], 2, 3), '00', sep = ':')
    x[k] <- eventTime[k]
  }
  i <- 4 #' HM
  j <- nchar(eventTime) == i
  k <- {i0 | i12} & j
  if(any(k)){
    eventTime[k] <- paste(substr(eventTime[k], 1, 2), substr(eventTime[k], 3, 4), '00', sep = ':')
    x[k] <- eventTime[k]
  }
  i <- 8 #' H:M:S
  j <- nchar(eventTime) == i
  k <- {i0 | i12} & j
  if(any(k)){
    x[k] <- eventTime[k]
  }
  i <- 14 #' H:M:S+H:M
  j <- nchar(eventTime) == i
  k <- {i0 | i12} & j # no values
  i <- 17 #' H:M:S \\ H:M:S
  j <- nchar(eventTime) == i
  k <- {i0 | i12} & j
  if(any(k)){
    x[k] <- substr(eventTime[k], 1, 8)  
  }
  i <- 22 #' Y-m-d H:M:S+H
  j <- nchar(eventTime) == i
  k <- {i0 | i12} & j
  if(any(k)){
    y <- sapply(strsplit(eventTime[k], ' '), function(z) z[2])
    y <- strsplit(y, '\\+')
    adj <- as.numeric(sapply(y, function(z) z[2]))
    y <- sapply(y, function(z) z[1])
    eventTime[k] <- format(strptime(y, format = '%H:%M:%S') + adj*60^2, '%H:%M:%S')
    x[k] <- eventTime[k]
  }
  
  eventTime <- x
  
  rm(x,y,i,j,i0,i12,k)
  
  dat$eventDate <- eventDate
  dat$eventTime <- eventTime
  
  dat$day <- as.numeric(strftime(as.Date(eventDate), '%d'))
  dat$dayOfYear <- as.numeric(strftime(as.Date(eventDate), '%j'))
  dat$month <- month.abb[dat$month] #' convert month to character abbreviation to match OBIS data set
  
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[is.na(dat$eventTime) | dat$eventTime == ''] <- ''
  
  #' ~~~~~~~~~~~~~~~~~~~~~~
  #' Sampling gear/protocol
  #' ~~~~~~~~~~~~~~~~~~~~~~
  
  #' The 'samplingProtocol' field contains info on gear used to collect data, and
  #' this is supplemented by the 'collectionCode' and datasetName fields -- there
  #' may also be some useful info in the fieldNotes field.
  
  message('\n', "Standardise 'sample gear'")
  
  gear <- dat$samplingProtocol
  code <- dat$collectionCode
  name <- dat$datasetName
  note <- dat$fieldNotes
  # sort(unique(gear))
  # sort(unique(code))
  # sort(unique(name))
  # sort(unique(note))
  
  gear[gear == ''] <- NA
  unknownGear <- is.na(gear)
  # sort(unique(code[unknownGear])) #' collection codes for records lacking info on sampling protocol
  
  #' Collection codes can be used to inform missing info CPR and WP-2 gears.
  CPR_codes <- unique(code[grepl('cpr', code, ignore.case = TRUE)]) #' unique 'CPR' collection codes
  CPR_names <- unique(name[grepl('cpr', name, ignore.case = TRUE)]) #' unique 'CPR' data set names
  gear[unknownGear & {code %in% CPR_codes | name %in% CPR_names}] <- 'CPR'
  unknownGear <- is.na(gear)
  # sort(unique(note[unknownGear])) #' field notes for records lacking info on sampling protocol
  
  net_notes <- unique(note[grepl('net', note, ignore.case = TRUE)])
  gear[unknownGear & note %in% net_notes] <- 'net'
  unknownGear <- is.na(gear)
  
  #' Standardise the samplingProtocol column
  # sort(unique(gear))
  
  #' Some records provide a cruise report or reference link for sampling protocol. Store this info in a seperate column.
  isInCruiseReport <- grepl('report', gear, ignore.case = TRUE) |
    grepl('http', gear, ignore.case = TRUE) | grepl('doi', gear, ignore.case = TRUE) |
    grepl('publication', gear, ignore.case = TRUE)
  dat$cruiseReport <- gear
  dat$cruiseReport[!isInCruiseReport] <- NA
  gear[isInCruiseReport] <- 'see cruise report'
  
  isCPR <- grepl('cpr', gear, ignore.case = TRUE)
  isBongo <- grepl('bongo', gear, ignore.case = TRUE)
  isNansen <- grepl('nansen', gear, ignore.case = TRUE)
  isMulti <- grepl('multi', gear, ignore.case = TRUE)
  isPump <- grepl('pump', gear, ignore.case = TRUE)
  isJuday <- grepl('juday', gear, ignore.case = TRUE)
  isWP2 <- grepl('wp', gear, ignore.case = TRUE)
  isMOCNESS <- grepl('moc', gear, ignore.case = TRUE)
  isRing <- grepl('ring', gear, ignore.case = TRUE) &
    !{grepl('measuring', gear, ignore.case = TRUE) |
        grepl('syringe', gear, ignore.case = TRUE)}
  isDredge <- grepl('dredge', gear, ignore.case = TRUE)
  isHandCollected <- grepl('hand', gear, ignore.case = TRUE) |
    grepl('div', gear, ignore.case = TRUE) |
    grepl('scuba', gear, ignore.case = TRUE)
  isShore <- !isHandCollected & {grepl('shore', gear, ignore.case = TRUE) |
      grepl('tide line', gear, ignore.case = TRUE) |
      grepl('littoral', gear, ignore.case = TRUE)}
  isBogorov <- grepl('bogorov', gear, ignore.case = TRUE)
  isClarke <- grepl('clarke', gear, ignore.case = TRUE)
  isStramin <- grepl('stramin', gear, ignore.case = TRUE)
  isApstein <- grepl('apstein', gear, ignore.case = TRUE)
  isDiscoveryN70 <- grepl('discovery', gear, ignore.case = TRUE)
  isLachlan50 <- grepl('lachlan', gear, ignore.case = TRUE)
  isAgassiz <- grepl('agassiz', gear, ignore.case = TRUE)
  isIsaaksKidd <- grepl('isaa', gear, ignore.case = TRUE)
  isSligsbyGorbunov <- grepl('sligsby', gear, ignore.case = TRUE)
  isMenzies <- grepl('menzies', gear, ignore.case = TRUE)
  isOther <- grepl('trap', gear, ignore.case = TRUE) | 
    grepl('corer', gear, ignore.case = TRUE) |
    grepl('ice core', gear, ignore.case = TRUE) |
    grepl('bottle', gear, ignore.case = TRUE) |
    grepl('grab', gear, ignore.case = TRUE) |
    grepl('longline', gear, ignore.case = TRUE) |
    grepl('sled', gear, ignore.case = TRUE) |
    grepl('water sampler', gear, ignore.case = TRUE)
  
  x <- unknownGear | isInCruiseReport | isCPR | isBongo | isNansen | isMulti | 
    isPump | isJuday | isWP2 | isMOCNESS | isRing | isDredge | isHandCollected |
    isShore | isBogorov | isClarke | isStramin | isApstein | isDiscoveryN70 |
    isLachlan50 | isAgassiz | isIsaaksKidd | isSligsbyGorbunov | isMenzies |
    isOther
  
  y <- sort(unique(gear[!x]))
  # print(y)
  
  isUnspecifiedNet <- gear %in% unique(gear[!x])
  
  x <- unknownGear + isInCruiseReport + isCPR + isBongo + isNansen + isMulti + isPump +
    isJuday + isWP2 + isMOCNESS + isRing + isDredge + isHandCollected + isShore + isBogorov +
    isClarke + isStramin + isApstein + isDiscoveryN70 + isLachlan50 + isAgassiz + isIsaaksKidd +
    isSligsbyGorbunov + isMenzies + isOther + isUnspecifiedNet
  
  gearDefined4AllRecords <- all(x > 0)
  # cat(paste('\ngear defined for all records:', gearDefined4AllRecords,'\n\n'))
  gearUnique4AllRecords <- all(x < 2)
  # cat(paste('\ngear uniquely defined for all records:', gearUnique4AllRecords,'\n\n'))
  
  gear[unknownGear] <- 'unknown gear'
  gear[isCPR] <- 'CPR'
  gear[isBongo] <- 'BONGO'
  gear[isNansen] <- 'Nansen'
  gear[isMulti] <- 'multinet'
  gear[isPump] <- 'water pump'
  gear[isJuday] <- 'Juday'
  gear[isWP2] <- 'WPII'
  gear[isMOCNESS] <- 'MOCNESS'
  gear[isRing] <- 'ring net'
  gear[isDredge] <- 'dredge'
  gear[isHandCollected] <- 'hand collected'
  gear[isShore] <- 'on shore'
  gear[isBogorov] <- 'Bogorov-Rass'
  gear[isClarke] <- 'Clarke-Bumpus'
  gear[isStramin] <- 'Stramin'
  gear[isApstein] <- 'Apstein'
  gear[isDiscoveryN70] <- 'Discovery N70'
  gear[isLachlan50] <- 'Lachlan N50'
  gear[isAgassiz] <- 'Agassiz'
  gear[isIsaaksKidd] <- 'Isaac-Kidd'
  gear[isSligsbyGorbunov] <- 'Sligsby-Gorbunov'
  gear[isMenzies] <- 'Menzies'
  gear[isOther] <- 'Other'
  gear[isUnspecifiedNet] <- 'Unspecified net'
  
  #' Compare standardised gear names to originals
  d <- unique(data.frame(original_name = dat$samplingProtocol, standardised_name = gear,
                         collectionCode = code, fieldNotes = note))
  d <- do.call('rbind', lapply(1:nrow(d), function(z, n)
    matrix(d[z,], nrow = 1, dimnames = list(NULL, names(d))), n = names(d)))
  # print(d[order(unlist(d[,'standardised_name'])),])
  
  dat$samplingProtocol <- gear
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise units of organism quantity
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'abundance'")
  
  organismQuantity <- dat$organismQuantity
  individualCount <- dat$individualCount
  quantType <- dat$organismQuantityType
  
  # unique(quantType) #' there are volumetric concentrations, area densities, total numbers, presence/absence, and others...
  quantType[is.na(quantType)] <- ''
  
  concentrationLabels <- c('Quantity per cubic metre', 'ind m3',
                           'Number per cubic metre', 'taxon per cubic metre',
                           'Abundances in individuals per m3',
                           'number of individuals per 100 cubic meter',
                           'number of individuals per cubic meter',
                           'Abundance per cubic metre', 'abundance per cubic metre', 
                           'Individuals per litre')  # "individuals per volume" is probably useless!
  #' convert concentrations to units of individuals per cubic metre
  i <- quantType == 'number of individuals per 100 cubic meter'
  organismQuantity[i] <- organismQuantity[i] * 1e-2
  i <- quantType == 'Individuals per litre'
  organismQuantity[i] <- organismQuantity[i] * 1e3
  quantType[quantType %in% concentrationLabels] <- 'individuals / m3' #' regularise the concentration labels
  
  densityLabels <- c('abundance per m2')
  quantType[quantType %in% densityLabels] <- 'individuals / m2'
  
  totalLabels <- c('individuals', 'number of individuals')
  quantType[quantType %in% totalLabels] <- 'individuals'
  missingQuant <- is.na(organismQuantity) #' substitute individual counts from another column
  subQuant <- missingQuant & !is.na(individualCount)
  organismQuantity[subQuant] <- individualCount[subQuant]
  quantType[subQuant] <- 'individuals'
  
  #' Omit the few measures where the unit is not properly specified or uncertain
  omitUnit <- quantType %in% c('abundance?', 'individuals per volume')
  quantType[omitUnit] <- ''
  organismQuantity[omitUnit] <- NA
  
  dat$organismQuantity <- organismQuantity #' update data frame
  dat$organismQuantityType <- quantType
  dat <- dat[,names(dat) != 'individualCount']
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$decimalLatitude & dat$decimalLatitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' ~~~~~~~~~~~~~
  #' Order by time
  #' ~~~~~~~~~~~~~
  dat <- dat[order(dat$eventDate, dat$eventTime),]
  
  #' ~~~~~~~~~~~~~~~~~~~
  #' Assign sample event 
  #' ~~~~~~~~~~~~~~~~~~~
  #' Use info on gear, coordinates, date, and time. Use a time increment of 30
  #' minutes to separate events, and round coordinates to 2 decimal places.
  inc <- 30
  dat$time.inc <- dat$eventTime
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(floor(mins / inc))
  }, inc = inc)
  
  dat$lon.r <- round(dat$decimalLongitude, digits = 2)
  dat$lat.r <- round(dat$decimalLatitude, digits = 2)
  
  x <- dat %>%
    select(basisOfRecord, eventID, parentEventID, eventDate, samplingProtocol,
           samplingEffort, time.inc, lon.r, lat.r) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    select(-time.inc, -lon.r, -lat.r)
  
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n------------------\n',
          'Finished GBIF data',
          '\n------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  # BAS bongo nets ----------------------------------------------------------
  
  # The BAS bongo net and rmt net samples are already similarly formatted, so only
  # minor adjustments are needed to make them compatible with each other.
  
  Source <- 'BAS_bongo'
  
  message('\n---------------------------\n',
          'Cleaning BAS bongo net data',
          '\n---------------------------')
  
  dat <- DATA[[Source]]
  
  #' ~~~~~~~~~~~~~~~~~~~
  #' Adjust column names
  #' ~~~~~~~~~~~~~~~~~~~
  dat$Net.type <- 'Bongo'
  dat$Net.mesh.size <- gsub('Bongo ', '', dat$Net.mesh.size)
  dat$Net.mesh.size <- paste(dat$Net.mesh.size, paste0('\U03bc', 'm'))
  dat$Max.depth <- dat$Maxdepth; dat$Maxdepth <- NULL
  dat$Cruise.name <- dat$Cruise.name..click.for.details.; dat$Cruise.name..click.for.details. <- NULL
  dat$Sample.event <- dat$Eventid; dat$Eventid <- NULL
  dat$Abundance.m2 <- dat$Abundance..m.2.; dat$Abundance..m.2. <- NULL
  dat$Occurence.id <- dat$Occurenceid; dat$Occurenceid <- NULL
  
  #' ~~~~~~~~~~~~~~~~~~~~~
  #' Set date/time columns
  #' ~~~~~~~~~~~~~~~~~~~~~
  message('\n', "Standardise 'date/time'")
  
  Date <- sapply(strsplit(dat$Start.of.event, ' '), function(z) z[1])
  Time <- sapply(strsplit(dat$Start.of.event, ' '), function(z) z[2])
  noDate <- is.na(Date)
  noTime <- is.na(Time)
  Date[noDate] <- ''
  Time[noTime] <- ''
  Date <- tolower(Date)
  for(i in 1:12) Date <- gsub(tolower(month.abb[i]), i, Date)
  Date[!noDate & noTime] <- as.character(strptime(Date[!noDate & noTime], format = '%d-%m-%Y'))
  Date[!{noDate | noTime}] <- as.character(strptime(paste(Date, Time)[!{noDate | noTime}], format = '%d-%m-%Y %H:%M:%OS'))
  dat$Start.of.event <- Date
  
  Date <- sapply(strsplit(dat$End.of.event, ' '), function(z) z[1])
  Time <- sapply(strsplit(dat$End.of.event, ' '), function(z) z[2])
  noDate <- is.na(Date)
  noTime <- is.na(Time)
  Date[noDate] <- ''
  Time[noTime] <- ''
  Date <- tolower(Date)
  for(i in 1:12) Date <- gsub(tolower(month.abb[i]), i, Date)
  Date[!noDate & noTime] <- as.character(strptime(Date[!noDate & noTime], format = '%d-%m-%Y'))
  Date[!{noDate | noTime}] <- as.character(strptime(paste(Date, Time)[!{noDate | noTime}], format = '%d-%m-%Y %H:%M:%OS'))
  dat$End.of.event <- Date
  
  dat$Year <- as.numeric(strftime(dat$Start.of.event, '%Y'))
  dat$Month <- as.numeric(strftime(dat$Start.of.event, '%m'))
  dat$Day <- as.numeric(strftime(dat$Start.of.event, '%j'))
  
  dat$Season <- dat$Year - {dat$Month <= 6}
  dat$Season <- paste(dat$Season, dat$Season + 1, sep = '-')
  
  noTime <- nchar(dat$Start.of.event) != 19
  
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[noTime] <- ''
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise life stage
  #' ~~~~~~~~~~~~~~~~~~~~~~
  message('\n', "Standardise 'life stage'")
  
  dat$Sex <- NA
  dat$Copepodite.stage <- NA
  dat$Maturity <- NA
  #' Sex
  j <- dat$Taxon.class
  i <- grepl(' female', dat$Taxon.class, ignore.case = TRUE)
  dat$Sex[i] <- 'female'
  j[i] <- ''
  i <- grepl(' male', dat$Taxon.class, ignore.case = TRUE)
  dat$Sex[i] <- 'male'
  j[i] <- ''
  #' Copepodite stage
  i <- j %in% paste0('C', 1:5)
  dat$Copepodite.stage[i] <- j[i]
  j[i] <- ''
  i <- rowSums(vgrepl(paste0('C', 1:5, '-'), j)) > 0
  dat$Copepodite.stage[i] <- j[i]
  dat$Copepodite.stage <- gsub('-', '-C', dat$Copepodite.stage)
  dat$Copepodite.stage <- gsub('-Cadult', '-C6', dat$Copepodite.stage)
  j[i] <- ''
  #' Maturity
  dat$Maturity[!is.na(dat$Sex)] <- 'adult'
  i <- j == 'adult'
  dat$Maturity[i] <- 'adult'
  j[i] <- ''
  i <- j == 'juvenile' | j == 'juvenille'
  dat$Maturity[i] <- 'juvenile'
  j[i] <- ''
  i <- grepl('furcilia', j, ignore.case = TRUE) | 
    grepl('nauplii', j, ignore.case = TRUE) | 
    grepl('calyptopis', j, ignore.case = TRUE) |
    grepl('larvae', j, ignore.case = TRUE) |
    grepl('zoeae', j, ignore.case = TRUE)
  dat$Maturity[i] <- 'juvenile'
  j[i] <- ''
  i <- grepl('medusae', j, ignore.case = TRUE) | 
    grepl('oozoid', j, ignore.case = TRUE)
  dat$Maturity[i] <- 'adult'
  j[i] <- ''
  
  
  #' Regularise format of abundance values
  message('\n', "Standardise 'abundance'")
  
  dat$Abundance.m2 <- gsub(',', '', dat$Abundance.m2)
  dat$Abundance.m2 <- as.numeric(dat$Abundance.m2)
  #' NA abundance values are where no animals were found because nobody counted.
  #' These may or may not be zeros.
  dat$Abundance.m2[!is.na(dat$Abundance.m2) & dat$Abundance.m2 < 0] <- NA
  
  dat$Unit <- 'number/m2'
  names(dat)[grepl('abundance', names(dat), ignore.case = TRUE)] <- 'Abundance'
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Lat & dat$Lat <= lat_lim[2]
  dat <- dat[i,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~
  #' Reformat sample event
  #' ~~~~~~~~~~~~~~~~~~~~~
  dat <- dat[order(dat$Start.of.event, dat$Event.name, dat$Taxon.name),]
  x <- dat %>% select(Sample.event) %>% distinct()
  x$Sample.event.num <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x))
  dat$Sample.event <- dat$Sample.event.num  
  dat$Sample.event.num <- NULL
  
  #' Reorder columns
  colOrder <- c('Taxon.name', 'Taxon.class', 'Sex', 'Maturity', 'Copepodite.stage', 'Net.type', 'Net.mesh.size', 'Cruise.name', 'Event.name', 'Sample.event', 'Season', 'Year', 'Month', 'Day', 'Time.Flag', 'Start.of.event', 'Lat', 'Lon', 'End.of.event', 'End.lat', 'End.lon', 'Max.depth', 'Scientist', 'Occurence.id', 'Abundance', 'Unit')
  dat <- dat[,colOrder]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n---------------------------\n',
          'Finished BAS bongo net data',
          '\n---------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  # BAS rmt nets ------------------------------------------------------------
  
  Source <- 'BAS_rmt'
  
  message('\n-------------------------\n',
          'Cleaning BAS rmt net data', 
          '\n-------------------------')
  
  
  dat <- DATA[[Source]]
  
  #' ~~~~~~~~~~~~~~~~~~~
  #' Adjust column names
  #' ~~~~~~~~~~~~~~~~~~~
  dat$Start.of.event <- dat$Start.of.Event; dat$Start.of.Event <- NULL
  dat$Abundance.m2 <- dat$Abundm2; dat$Abundm2 <- NULL
  dat$Abundance.m3 <- dat$Abundm3; dat$Abundm3 <- NULL
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~
  #' Set date/time columns
  #' ~~~~~~~~~~~~~~~~~~~~~
  message('\n', "Standardise 'date/time'")
  
  Date <- sapply(strsplit(dat$Start.of.event, ' '), function(z) z[1])
  Time <- sapply(strsplit(dat$Start.of.event, ' '), function(z) z[2])
  noDate <- is.na(Date)
  noTime <- is.na(Time)
  Date[noDate] <- ''
  Time[noTime] <- ''
  Date <- tolower(Date)
  for(i in 1:12) Date <- gsub(tolower(month.abb[i]), i, Date)
  Date[!noDate & noTime] <- as.character(strptime(Date[!noDate & noTime], format = '%d-%m-%Y'))
  Date[!{noDate | noTime}] <- as.character(strptime(paste(Date, Time)[!{noDate | noTime}], format = '%d-%m-%Y %H:%M:%OS'))
  dat$Start.of.event <- Date
  
  Date <- sapply(strsplit(dat$End.of.event, ' '), function(z) z[1])
  Time <- sapply(strsplit(dat$End.of.event, ' '), function(z) z[2])
  noDate <- is.na(Date)
  noTime <- is.na(Time)
  Date[noDate] <- ''
  Time[noTime] <- ''
  Date <- tolower(Date)
  for(i in 1:12) Date <- gsub(tolower(month.abb[i]), i, Date)
  Date[!noDate & noTime] <- as.character(strptime(Date[!noDate & noTime], format = '%d-%m-%Y'))
  Date[!{noDate | noTime}] <- as.character(strptime(paste(Date, Time)[!{noDate | noTime}], format = '%d-%m-%Y %H:%M:%OS'))
  dat$End.of.event <- Date
  
  dat$Year <- as.numeric(strftime(dat$Start.of.event, '%Y'))
  dat$Month <- as.numeric(strftime(dat$Start.of.event, '%m'))
  dat$Day <- as.numeric(strftime(dat$Start.of.event, '%j'))
  
  dat$Season <- dat$Year - {dat$Month <= 6}
  dat$Season <- paste(dat$Season, dat$Season + 1, sep = '-')
  
  noTime <- nchar(dat$Start.of.event) != 19
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[noTime] <- ''
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise life stage
  #' ~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'life stage'")
  
  dat$Sex <- NA
  dat$Copepodite.stage <- NA
  dat$Maturity <- NA
  #' Sex
  j <- dat$Taxon.class
  i <- grepl(' female', dat$Taxon.class, ignore.case = TRUE)
  dat$Sex[i] <- 'female'
  j[i] <- ''
  i <- grepl(' male', dat$Taxon.class, ignore.case = TRUE)
  dat$Sex[i] <- 'male'
  j[i] <- ''
  #' Copepodite stage
  i <- j %in% paste0('C', 1:5)
  dat$Copepodite.stage[i] <- j[i]
  j[i] <- ''
  i <- rowSums(vgrepl(paste0('C', 1:5, '-'), j)) > 0
  dat$Copepodite.stage[i] <- j[i]
  j[i] <- ''
  i <- rowSums(vgrepl(paste0('copepodites C', 1:5), j)) > 0
  dat$Copepodite.stage[i] <- gsub('copepodites ', '', j[i])
  dat$Copepodite.stage <- gsub('-', '-C', dat$Copepodite.stage)
  dat$Copepodite.stage <- gsub('-Cadult', '-C6', dat$Copepodite.stage)
  j[i] <- ''
  #' Maturity
  dat$Maturity[!is.na(dat$Sex)] <- 'adult'
  i <- j == 'adult'
  dat$Maturity[i] <- 'adult'
  j[i] <- ''
  i <- j == 'juvenile' | j == 'juvenille'
  dat$Maturity[i] <- 'juvenile'
  j[i] <- ''
  i <- grepl('furcilia', j, ignore.case = TRUE) | 
    grepl('nauplii', j, ignore.case = TRUE) | 
    grepl('calyptopis', j, ignore.case = TRUE) |
    grepl('larvae', j, ignore.case = TRUE)
  dat$Maturity[i] <- 'juvenile'
  j[i] <- ''
  i <- grepl('medusae', j, ignore.case = TRUE) |
    grepl('gonads', j, ignore.case = TRUE)
  dat$Maturity[i] <- 'adult'
  j[i] <- ''
  # dat$Copepodite.stage[dat$Maturity == 'adult'] <- 'C6'
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Reformat some abundance values
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'abundance'")
  
  dat$Abundance.m2 <- gsub(',', '', dat$Abundance.m2)
  dat$Abundance.m2 <- as.numeric(dat$Abundance.m2)
  
  #' Omit missing measurement values
  i <- is.na(dat$Abundance.m2) | dat$Abundance.m2 < 0
  dat$Abundance.m2[i] <- NA
  i <- is.na(dat$Abundance.m3) | dat$Abundance.m3 < 0
  dat$Abundance.m3[i] <- NA
  dat <- melt(dat, measure.vars = c('Abundance.m2','Abundance.m3'), variable.name = 'Unit', value.name = 'Abundance')
  i <- is.na(dat$Abundance)
  dat <- dat[!i,]
  
  dat$Unit <- gsub('Abundance.', 'number/', dat$Unit)
  
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Omit records missing depth information
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- is.na(dat$Max.depth)
  dat <- dat[!i,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Lat & dat$Lat <= lat_lim[2]
  dat <- dat[i,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~
  #' Reformat sample event
  #' ~~~~~~~~~~~~~~~~~~~~~
  dat <- dat[order(dat$Start.of.event, dat$Taxon.class),]
  x <- dat %>% select(Event.name) %>% distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x))
  dat$Event.name <- NULL
  
  
  #' ~~~~~~~~~~~~~~~
  #' Reorder columns
  #' ~~~~~~~~~~~~~~~
  colOrder <- c('Taxon.name', 'Taxon.class', 'Sex', 'Maturity', 'Copepodite.stage', 'Net.type', 'Cruise.name', 'Sample.event', 'Season', 'Year', 'Month', 'Day', 'Time.Flag', 'Start.of.event', 'Lat', 'Lon', 'End.of.event', 'End.lat', 'End.lon', 'Max.depth', 'Abundance', 'Unit')
  dat <- dat[,colOrder]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n-------------------------\n',
          'Finished BAS rmt net data',
          '\n-------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  
  # BAS MOCNESS nets --------------------------------------------------------
  
  Source <- 'BAS_mocness'
  
  message('\n-------------------------\n',
          'Cleaning BAS MOCNESS data',
          '\n-------------------------')
  
  dat <- DATA[[Source]]
  
  station.dat <- dat[[2]]
  dat <- dat[[1]]
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Arrange the numerical data
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  abn.unit <- names(dat)[1]
  dat <- setNames(unname(dat[-1,]), dat[1,])
  i <- 1:{grep('Species', names(dat))-1}
  j <- {max(i)+1}:ncol(dat)
  dat1 <- dat[,i]
  dat2 <- dat[,j]
  
  dat1.2 <- dat1[,-{1:3}]
  station <- sapply(strsplit(names(dat1.2), '\\.'), function(z) z[1])
  net <- as.numeric(unname(dat1.2[1,]))
  dat1.2 <- unname(dat1.2[-1,])
  station <- rep(station, each = nrow(dat1.2))
  net <- as.integer(rep(net, each = nrow(dat1.2)))
  species <- rep(dat1[-1,1], ncol(dat1.2))
  dev.stage <- rep(dat1[-1,2], ncol(dat1.2))
  dat1 <- data.frame(Species = species, Development.Stage = dev.stage,
                     Station = station, Net = net,
                     Abundance = as.numeric(unlist(dat1.2)))
  dat <- dat1
  dat2 <- station.dat
  rm(dat1, dat1.2, station.dat)
  
  #' ~~~~~~~~~~~~~~~~~~~~~~
  #' Merge the station data
  #' ~~~~~~~~~~~~~~~~~~~~~~
  dat$Station <- as.integer(gsub('E', '', dat$Station))
  dat2 <- dat2[!apply(is.na(dat2) | dat2 == '', 1, all),]
  names(dat2)[c(2:5,9)] <- c('Station', 'Net', 'Latitude', 'Longitude', 'Volume.Filtered')
  dat <- dat %>%
    left_join(dat2, by = c('Station','Net'))
  rm(dat2)
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Reformat column categories
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- is.na(dat$Development.Stage) |
    dat$Development.Stage %in% c('(blank)', 'Stage', 'Species ', 'Cnidaria')
  dat$Development.Stage[i] <- NA
  dat$Development.Stage <- gsub('\\.', '', dat$Development.Stage)
  i <- !is.na(dat$Development.Stage) &
    dat$Development.Stage %in% c('A/JV', 'AD/J')
  dat$Development.Stage[i] <- 'AD/JV'
  
  i <- !is.na(dat$Development.Stage) & dat$Development.Stage %in% c('UN')
  dat$Development.Stage[i] <- 'Unspecified'
  
  dat$Sex <- NA
  dat$Sex[dat$Development.Stage == 'AD F'] <- 'female'
  dat$Sex[dat$Development.Stage == 'AD M'] <- 'male'
  
  dat$Development.Stage <- gsub('AD F', 'AD', dat$Development.Stage)
  dat$Development.Stage <- gsub('AD M', 'AD', dat$Development.Stage)
  
  dat$Maturity <- NA
  i <- dat$Development.Stage == 'AD'
  dat$Maturity[i] <- 'adult'
  i <- dat$Development.Stage %in% c(paste0('C',1:5), 'cypris', 'NP', 'EG',
                                    'calyptopis', 'furcilia', 'LV', 'JV')
  dat$Maturity[i] <- 'juvenile'
  i <- dat$Development.Stage == 'AD/JV'
  dat$Maturity[i] <- 'juvenile and adult'
  
  dat$Copepodite.stage <- NA
  i <- dat$Development.Stage %in% paste0('C', 1:5)
  dat$Copepodite.stage[i] <- dat$Development.Stage[i]
  i <- dat$Development.Stage == 'AD'
  dat$Copepodite.stage[i] <- 'C6'
  
  dat$Life.stage <- dat$Development.Stage
  i <- dat$Life.stage %in% c('AD', paste0('C',1:5))
  dat$Life.stage[i] <- 'copepodite'
  i <- dat$Life.stage == 'NP'
  dat$Life.stage[i] <- 'nauplius'
  i <- dat$Life.stage == 'EG'
  dat$Life.stage[i] <- 'egg'
  i <- dat$Life.stage == 'LV'
  dat$Life.stage[i] <- 'larva'
  i <- dat$Life.stage %in% c('JV', 'AD/JV')
  dat$Life.stage[i] <- 'Unspecified'
  
  dat$Development.Stage <- NULL
  
  names(dat)[3] <- 'Net.Number'
  
  x <- strsplit(dat$Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Date <- as.character(as.Date(dat$Date, format = '%d/%m/%Y'))
  dat$Time <- sapply(x, function(z) z[2])
  dat$Time <- paste0(dat$Time, ':00')
  
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  dat$Unit <- 'individuals/m3'
  dat$Mesh.Size <- '300 µm'
  
  #' Omit single bongo net sample
  i <- dat$Net.type == 'MiniBongo'
  dat <- dat[!i,]
  
  #' Set column classes
  dat$Longitude <- as.numeric(dat$Longitude)
  dat$Latitude <- as.numeric(dat$Latitude)
  
  #' Omit empty columns
  dat <- dat[,!sapply(1:ncol(dat), function(z) all(is.na(dat[,z]) | dat[,z] == ''))]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~
  #' Reformat station number
  #' ~~~~~~~~~~~~~~~~~~~~~~~
  dat <- dat[order(dat$Date, dat$Time),]
  x <- dat %>% select(Station) %>% distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .before = Station) %>%
    select(-Station)
  
  dat <- dat[,c('Species', 'Maturity', 'Life.stage', 'Copepodite.stage', 'Sex',
                'Sample.event', 'Net.Number', 'Longitude', 'Latitude', 'Date', 'Time',
                'Time.Flag', 'Net.type', 'Mesh.Size', 'Open.depth', 'Closed.depth',
                'Volume.Filtered', 'Abundance', 'Unit')]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n-------------------------\n',
          'Finished BAS MOCNESS data',
          '\n-------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # Schnack-Schiel ----------------------------------------------------------
  
  Source <- 'Schnack.Schiel'
  
  message('\n----------------------------\n',
          'Cleaning Schnack-Schiel data',
          '\n----------------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Regularise the data column names
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  regColNames <- function(dat){
    n <- names(dat)
    # Species names -- single dot between family & species
    n <- sapply(seq_along(n), function(z){
      o <- n[z]
      r <- gregexec('\\.\\.', o)[[1]]
      if(all(class(r) == 'integer') && r == -1) return(o) else{
        p <- r[,1]
        a <- attr(r, 'match.length')[,1]
        if(p == 2 && a == 2) o <- paste0(substr(o, 1, 2), substr(o, 4, nchar(o)))
        return(o)}})
    # Replace quadruple dots with underscore
    n <- gsub('\\.\\.\\.\\.', '_', n)
    # Remove any underscores appearing as last character
    r <- sapply(gregexec('_', n), function(z) tail(as.vector(z), 1))
    nc <- nchar(n)
    m <- r == nc
    n[m] <- substr(n[m], 1, nc[m] - 1)
    # Remove any dots appearing as last character
    r <- sapply(gregexec('\\.', n), function(z) tail(as.vector(z), 1))
    nc <- nchar(n)
    m <- r == nc
    n[m] <- substr(n[m], 1, nc[m] - 1)
    # Replace triple dots with underscore
    n <- gsub('\\.\\.\\.', '_', n)
    # Replace spp.. with spp.
    n <- gsub('spp\\.\\.', 'spp\\.', n)
    # Replace m.. with m
    n <- gsub('m\\.\\.', 'm', n)
    # Replace ..m with _m
    n <- gsub('\\.\\.m', '_m', n)
    # Get rid of remaining .. by omitting anything appearing afterwards
    n <- sapply(strsplit(n, '\\.\\.'), function(z) z[1])
    names(dat) <- n
    return(dat)}
  
  dat <- lapply(dat, regColNames)
  
  #' Select columns to retain
  metaDataCols <- c('event', 'date', 'time', 'latitude', 'longitude', 'elevation',
                    'depth', 'vol', 'comment')
  #' And columns to omit
  omitCols <- c('event.2')
  #' Abundances of all species are recorded as volumetric concentration and the
  #' column names include '_m3', so use this to identify species columns
  dat <- lapply(1:length(dat), function(z){
    zdat <- dat[[z]]
    ndat <- names(zdat)
    d <- data.frame(Data.Table = rep(sub(
      '_crop.tab', '', data.file.names[[Source]][z]), nrow(zdat)))
    keep <- vgrepl(c(metaDataCols, '_m3'), ndat, ignore.case = TRUE)
    omit <- vgrepl(omitCols, ndat, ignore.case = TRUE)
    keep <- {rowSums(keep) > 0} & !{rowSums(omit) > 0}
    cbind(d, zdat[,keep])})
  metaDataCols <- c('Data.Table', metaDataCols)
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  #' Melt data into long form
  id_vars <- names(dat)[rowSums(vgrepl(metaDataCols, names(dat),
                                       ignore.case = TRUE)) > 0]
  dat <- melt(data = dat, id.vars = id_vars, variable.name = 'Measurement',
              value.name = 'Value', na.rm = TRUE)
  
  #' Some top/bottom depths were inputted back-to-front -- correct this
  message('\n', "Standardise 'depth'")
  
  i <- {dat$Depth.bot_m - dat$Depth.top_m} < 0
  dt <- dat$Depth.bot_m[i]
  db <- dat$Depth.top_m[i]
  dat$Depth.bot_m[i] <- db
  dat$Depth.top_m[i] <- dt
  rm(list = c('i', 'db', 'dt'))
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise the Comment column
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message('\n', "Standardise the 'Comments' column")
  
  i <- grepl('St.', dat$Comment)
  dat$Comment[i] <- sapply(strsplit(dat$Comment[i], '\\. '),
                           function(z) paste(z[-c(1,2)], collapse = '. '))
  #' Replace periods with commas
  dat$Comment <- gsub('\\.', ',', dat$Comment)
  
  #' ~~~~~~~~~
  #' Net types
  #' ~~~~~~~~~
  message('\n', "Standardise 'net types'")
  
  #' The data are from 3 net types: bongo, multi-net, and Nansen.
  netTypes <- c('bongo','multinet','nansen')
  dat$Comment <- gsub('MN', 'Multinet', dat$Comment)
  i <- vgrepl(netTypes, dat$Comment, ignore.case = TRUE)
  dat$Net.Type <- sapply(1:nrow(dat), function(z) netTypes[i[z,]]); rm(i)
  dat$Net.Type[dat$Net.Type == 'bongo'] <- 'Bongo'
  dat$Net.Type[dat$Net.Type == 'multinet'] <- 'Multinet'
  dat$Net.Type[dat$Net.Type == 'nansen'] <- 'Nansen'
  netTypes <- unique(dat$Net.Type)
  dat$Net.Type <- factor(dat$Net.Type, levels = netTypes)
  netTypes <- levels(dat$Net.Type)
  
  
  #' ~~~~~~~~~~
  #' Mesh sizes
  #' ~~~~~~~~~~
  
  #' All mesh sizes are reported as µm
  message('\n', "Standardise 'mesh size'")
  
  i <- gsub(' µm', 'µm', dat$Comment)
  i <- strsplit(i, ' ')
  i <- sapply(i, function(z) z[grepl('µm', z)])
  i <- gsub('µm', ' µm', i)
  i <- as.numeric(do.call('rbind', strsplit(i, ' '))[,1])
  j <- paste(sort(unique(i)), paste0('\U03bc', 'm'))
  i <- paste(i, paste0('\U03bc', 'm'))
  dat$Mesh.Size <- factor(i, levels = j, ordered = TRUE)
  rm(i,j)
  
  
  #' ~~~~~~~~~~~~~~
  #' Ice conditions
  #' ~~~~~~~~~~~~~~
  message('\n', "Standardise 'ice conditions'")
  
  dat$Ice.Conditions <- 'no ice'
  dat$Ice.Conditions[grepl('open pack-ice', dat$Comment)] <- 'open pack-ice'
  dat$Ice.Conditions[grepl('pack-ice', dat$Comment)] <- 'pack-ice'
  
  
  #' ~~~~~~~~~~~~~~~~~~
  #' Format dates/times
  #' ~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'date/time'")
  
  nc <- nchar(dat$Date.Time)
  # unique(nc)
  dat$Date.Time[nc == 16] <- paste0(dat$Date.Time[nc == 16], ':00') #' include seconds where not indicated
  dat$Date.Time <- gsub('T', ' ', dat$Date.Time)
  dat$Date.Time <- strptime(dat$Date.Time, format = '%Y-%m-%d %H:%M:%S')
  dat <- dat %>% distinct()
  x <- strsplit(as.character(dat$Date.Time), ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  rm(nc, x)
  
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  #' Data from some cruises are split over multiple tables, and some have
  #' duplicate events. Merge these into single tables with single Data.Table name.
  
  message('\n', "Standardise 'data tables'")
  
  #' ANT-III_3
  i <- grepl('ANT-III_', dat$Data.Table)
  n <- unique(dat$Data.Table[i])
  dd <- setNames(lapply(n, function(z) dat[dat$Data.Table == z,]), n)
  # sapply(dd, nrow)
  
  #' ANT-III_3_calanoida_abund is the largest table containing most, but not all,
  #' measurements. 
  dd_ <- dd$`ANT-III_3_calanoida_abund`
  dd_$Data.Table <- 'ANT-III_3_calanoida_cyclopoida_abund'
  for(j in n[!n == 'ANT-III_3_calanoida_abund']){
    dn <- dd[[j]]
    dn$Data.Table <- 'ANT-III_3_calanoida_cyclopoida_abund'
    dd_ <- merge(dd_, dn, all = TRUE)
  }
  dat <- rbind(dat[!i,], dd_)
  
  #' ANT-V_3
  i <- grepl('ANT-V_3', dat$Data.Table)
  n <- unique(dat$Data.Table[i])
  dd <- setNames(lapply(n, function(z) dat[dat$Data.Table == z,]), n)
  # sapply(dd, nrow)
  
  #' ANT-V_3_calanoida_abund is the largest table containing most, but not all,
  #' measurements. 
  dd_ <- dd$`ANT-V_3_calanoida_abund`
  dd_$Data.Table <- 'ANT-V_3_calanoida_cyclopoida_abund'
  for(j in n[!n == 'ANT-V_3_calanoida_abund']){
    dn <- dd[[j]]
    dn$Data.Table <- 'ANT-V_3_calanoida_cyclopoida_abund'
    dd_ <- merge(dd_, dn, all = TRUE)
  }
  dat <- rbind(dat[!i,], dd_)
  
  #' ANT-XI_3
  i <- grepl('ANT-XI_3', dat$Data.Table)
  n <- unique(dat$Data.Table[i])
  dd <- setNames(lapply(n, function(z) dat[dat$Data.Table == z,]), n)
  # sapply(dd, nrow)
  
  #' ANT-XI_3_calanoida_abund is the largest table containing most, but not all,
  #' measurements. 
  dd_ <- dd$`ANT-XI_3_calanoida_abund`
  dd_$Data.Table <- 'ANT-XI_3_calanoida_cyclopoida_abund'
  for(j in n[!n == 'ANT-XI_3_calanoida_abund']){
    dn <- dd[[j]]
    dn$Data.Table <- 'ANT-XI_3_calanoida_cyclopoida_abund'
    dd_ <- merge(dd_, dn, all = TRUE)
  }
  dat <- rbind(dat[!i,], dd_)
  
  #' ANT-XII_4
  i <- grepl('ANT-XII_4', dat$Data.Table)
  n <- unique(dat$Data.Table[i])
  dd <- setNames(lapply(n, function(z) dat[dat$Data.Table == z,]), n)
  # sapply(dd, nrow)
  
  #' ANT-XII_4_calanoida_abund is the largest table containing most, but not all,
  #' measurements. 
  dd_ <- dd$`ANT-XII_4_calanoida_abund`
  dd_$Data.Table <- 'ANT-XII_4_calanoida_cyclopoida_abund'
  for(j in n[!n == 'ANT-XII_4_calanoida_abund']){
    dn <- dd[[j]]
    dn$Data.Table <- 'ANT-XII_4_calanoida_cyclopoida_abund'
    dd_ <- merge(dd_, dn, all = TRUE)
  }
  dat <- rbind(dat[!i,], dd_)
  
  #' M44_2
  i <- grepl('M44_2', dat$Data.Table)
  n <- unique(dat$Data.Table[i])
  dd <- setNames(lapply(n, function(z) dat[dat$Data.Table == z,]), n)
  # sapply(dd, nrow)
  
  #' M44_2_copepods_abund is the largest table containing most, but not all,
  #' measurements. 
  dd_ <- dd$`M44_2_copepods_abund`
  dd_$Data.Table <- 'M44_2_copepods_abund'
  for(j in n[!n == 'M44_2_copepods_abund']){
    dn <- dd[[j]]
    dn$Data.Table <- 'M44_2_copepods_abund'
    dd_ <- merge(dd_, dn, all = TRUE)
  }
  dat <- rbind(dat[!i,], dd_)
  
  #' Some sample events have multiple measurements per depth layer, and were
  #' recorded with distinct times -- two distinct cases:
  #' (1) These measurements, sharing identical times, are due to deploying several
  #'     of the multinets within the same depth layer, producing multiple measures
  #'     at certain depths within a single event -- these values can just be
  #'     averaged to produce a single measurement per depth per event.
  #' (2) Some other of these measures, however, appear to be completely separate
  #'     gear deployments that have been recorded with the same event number,
  #'     apparent as they have distinct times -- these should be reassigned a new
  #'     event number.
  
  message('\n', "Standardise 'event labels'")
  
  #' (2) Assign new event labels where required
  d <- dat %>% select(c(Data.Table, Event, Date.Time)) %>% distinct()
  renameEvents <- unlist(lapply(unique(d$Event), function(z){
    x <- d[d$Event == z,]
    n <- nrow(x)
    if(n == 1) return(NULL) else return(z)}))
  d$EventRename <- d$Event
  d <- d[order(d$Date.Time),]
  for(i in renameEvents){
    j <- d$Event == i
    d$EventRename[j] <- paste(i, 1:sum(j), sep = '.')}
  dat <- merge(dat, d)
  dat <- dat %>% mutate(Event = EventRename) %>% select(!EventRename)
  rm(list = c('d', 'renameEvents', 'i', 'j'))
  
  #' (1) Average over multiple samples at single depth per event
  dat <- dat %>%
    mutate(Date.Time = mean(Date.Time), .by = c(Data.Table, Event, Depth.water_m)) %>%
    distinct()
  dat <- aggregate(Value ~ ., dat, mean)
  
  
  #' Some distinct sample events (in different locations) were recorded with
  #' identical times.
  dd <- dat %>% select(Event, Date.Time) %>% distinct()
  dd <- dd[order(dd$Date.Time, dd$Event),]
  dup <- duplicated(dd$Date.Time)
  ev <- unique(dd$Event[dup])
  dupTimes <- lapply(ev, function(z) dd$Date.Time[dd$Event == z])
  eventsWithDupTimes <- lapply(dupTimes, function(z) dd$Event[dd$Date.Time == z])
  #' It's likely that these duplicate times are due to the time from the 1st event
  #' being copied into the 2nd. There's no way to properly correct this. I can
  #' 'correct' it only by fudging the time -- do this by adjusting the date and
  #' setting the time blank
  dd <- dd[order(dd$Date.Time),]
  adjustTimes <- lapply(dupTimes, function(z){
    i <- which(dd$Date.Time == z)
    j <- dd$Date.Time[tail(i, 1) + 1]
    mean(c(z,j))})
  for(i in 1:length(ev)){
    j <- dd$Event == ev[i]
    dd$Date.Time[j] <- format(adjustTimes[[i]], '%Y-%m-%d')}
  colOrder <- names(dat)
  dat <- dat %>% select(-Date.Time)
  dat <- merge(dat, dd)
  dat <- dat[,colOrder]
  dat$Date.Time <- as.character(dat$Date.Time)
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  dat$Time[is.na(dat$Time)] <- ''
  rm(i,x,j)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Standardise sex and development stage
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'sex' and 'development stage'")
  
  dat$Measurement.Unit <- ''
  dat$Measurement.Unit[grepl('_m3', dat$Measurement)] <- 'number/m3'
  dat$Measurement <- gsub('_m3', '', dat$Measurement)
  
  dat$Sex <- NA
  dat$Development.Stage <- NA
  
  dat$Measurement <- gsub('spp', 'sp', dat$Measurement)
  m <- dat$Measurement
  i <- substr(m, 2, 2) == '.'
  m[i] <- sub('\\.', '', m[i])
  i <- strsplit(m, '\\.')
  j <- sapply(i, function(z) z[2])
  j[is.na(j)] <- ''
  j <- gsub('sp_', '', j)
  j[nchar(j) == 2 & grepl('sp', j)] <- ''
  i <- nchar(j) == 1
  dat$Sex[i & j == 'f'] <- 'female'
  j[i & j == 'f'] <- ''
  dat$Sex[i & j == 'm'] <- 'male'
  j[i & j == 'm'] <- ''
  i <- substr(j, 2, 2) == '_'
  k <- substr(j, 1, 1) == 'f'
  dat$Sex[i & k] <- 'female'
  j[i & k] <- ''
  k <- substr(j, 1, 1) == 'm'
  dat$Sex[i & k] <- 'male'
  j[i & k] <- ''
  i <- j == 'c'
  dat$Development.Stage[i] <- 'copepodite'
  j[i] <- ''
  i <- nchar(j) == 2
  dat$Development.Stage[i] <- j[i]
  j[i] <- ''
  i <- substr(j, 1, 2) == 'c_'
  dat$Development.Stage[i] <- 'copepodite'
  j[i] <- ''
  i <- substr(j, 1, 1) == 'c' & substr(j, 3, 3) == '_'
  dat$Development.Stage[i] <- substr(j[i], 1, 2)
  j[i] <- ''
  i <- j == 'copepodite'
  dat$Development.Stage[i] <- 'copepodite'
  j[i] <- ''
  i <- j == 'naup'
  dat$Development.Stage[i] <- 'nauplius'
  j[i] <- ''
  
  i <- rowSums(!is.na(dat[,c('Sex','Development.Stage')])) == 0
  j <- dat$Measurement
  j[!i] <- ''
  unique(j)
  j <- gsub('.sp', 'sp', j)
  n <- nchar(j)
  k <- substr(j, n-1, n) == '.m'
  dat$Sex[k] <- 'male'
  j[k] <- ''
  n <- nchar(j)
  k <- substr(j, n-1, n) == '.f'
  dat$Sex[k] <- 'female'
  j[k] <- ''
  
  k <- sapply(strsplit(j, '_'), function(z) z[1])
  k[is.na(k)] <- ''
  n <- nchar(k)
  kf <- substr(k, n-1, n) == '.f'
  km <- substr(k, n-1, n) == '.m'
  dat$Sex[km] <- 'male'
  dat$Sex[kf] <- 'female'
  j[km] <- ''
  j[kf] <- ''
  n <- nchar(j)
  k <- substr(j, n-1, n) == '.c'
  dat$Development.Stage[k] <- 'copepodite'
  j[k] <- ''
  n <- nchar(j)
  k <- substr(j, n-2, n-2) == '.' & substr(j, n-5, n-5) != '.' & substr(j, n-1, n) != 'co'
  dat$Development.Stage[k] <- substr(j[k], n[k]-1, n[k])
  j[k] <- ''
  n <- nchar(j)
  k <- substr(j, n-2, n-2) == '.' & substr(j, n-5, n-5) == '.'
  dat$Development.Stage[k] <- gsub('\\.', '-', substr(j[k], n[k]-4, n[k]))
  j[k] <- ''
  i <- sapply(strsplit(j, '_'), function(z) z[1])
  i[is.na(i)] <- ''
  n <- nchar(i)
  k <- substr(i, n-1, n) == '.c'
  dat$Development.Stage[k] <- 'copepodite'
  j[k] <- ''
  i[k] <- ''
  n <- nchar(i)
  k <- substr(i, n-2, n-2) == '.' & substr(i, n-5, n-5) != '.'
  dat$Development.Stage[k] <- substr(i[k], n[k]-1, n[k])
  j[k] <- ''
  i[k] <- ''
  k <- substr(i, n-2, n-2) == '.' & substr(i, n-5, n-5) == '.'
  dat$Development.Stage[k] <- gsub('\\.', '-', substr(i[k], n[k]-4, n[k]))
  j[k] <- ''
  i[k] <- ''
  
  dat$Development.Stage[!is.na(dat$Sex)] <- 'adult'
  dat$Development.Stage[is.na(dat$Development.Stage)] <- 'unspecified'
  
  for(i in paste0('c', 1:5)) dat$Development.Stage <- gsub(i, toupper(i), dat$Development.Stage)
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Convert the Measurement column into Species
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  message('\n', "Standardise 'species' column")
  
  m <- dat$Measurement
  n <- nchar(m)
  i <- substr(m, n-1, n) %in% c('.f', '.m', '.c')
  m[i] <- substr(m[i], 1, n[i]-2)
  n <- nchar(m)
  i <- substr(m, n-1, n) %in% c('_f', '_m', '_c')
  m[i] <- substr(m[i], 1, n[i]-2)
  
  n <- nchar(m)
  i <- substr(m, n-2, n) %in% paste0('.c', 1:5)
  m[i] <- substr(m[i], 1, n[i]-3)
  n <- nchar(m)
  i <- substr(m, n-2, n) %in% paste0('.c', 1:5)
  m[i] <- substr(m[i], 1, n[i]-3)
  m <- gsub('.c_', '_', m)
  m <- gsub('.f_', '_', m)
  m <- gsub('.m_', '_', m)
  m <- gsub('.c1_', '_', m)
  m <- gsub('.c2_', '_', m)
  m <- gsub('.c3_', '_', m)
  m <- gsub('.c4_', '_', m)
  m <- gsub('.c5_', '_', m)
  m <- gsub('.c1_', '_', m)
  m <- gsub('.c2_', '_', m)
  m <- gsub('.c3_', '_', m)
  m <- gsub('.c4_', '_', m)
  m <- gsub('.c5_', '_', m)
  
  n <- nchar(m)
  i <- !is.na(as.numeric(substr(m, n, n)))
  m[i] <- substr(m[i], 1, n[i]-1)
  n <- nchar(m)
  i <- substr(m, n, n) == '.'
  m[i] <- substr(m[i], 1, n[i]-1)
  n <- nchar(m)
  i <- substr(m, n-1, n) %in% c('.f','.m','.c')
  m[i] <- substr(m[i], 1, n[i]-2)
  
  i <- gregexpr('\\.', m)
  i <- suppressWarnings(do.call('rbind', i))
  k <- rowSums(i == 2) == ncol(i)
  j <- m
  j[k] <- ''
  
  n <- nchar(m)
  i <- substr(m, n-2, n) == '.sp' & regexpr('\\.', m) == {n-2}
  j[i] <- ''
  i <- grepl('sp_Species', j)
  m[i] <- gsub('sp_Species', 'sp', m[i])
  j[i] <- ''
  i <- grepl('.indet', j)
  m[i] <- gsub('.indet', '', m[i])
  j[i] <- ''
  i <- grepl('naup', j)
  m[i] <- gsub('.naup', '', m[i])
  j[i] <- ''
  i <- regexpr('\\.', j) == -1 & j != ''
  j[i] <- ''
  i <- grepl('_not.identified', j)
  m[i] <- gsub('_not.identified', '', m[i])
  j[i] <- ''
  i <- grepl('_includes', j)
  m[i] <- gsub('_includes', '', m[i])
  j[i] <- ''
  i <- grepl('.species.co', j)
  m[i] <- gsub('.species.co', '', m[i])
  j[i] <- ''
  i <- regexpr('.and.', j)
  k <- i > 0
  m[k] <- substr(m[k], 1, i[k] - 1)
  j[k] <- ''
  i <- regexpr('.copepodite', j)
  k <- i > 0
  m[k] <- substr(m[k], 1, i[k] - 1)
  j[k] <- ''
  i <- grepl('formerly.', j)
  m[i] <- gsub('formerly.', '' , m[i])
  j[i] <- ''
  n <- nchar(m)
  k <- gregexpr('\\.', m)
  i <- sapply(k, function(z) length(z) == 3)
  j <- sapply(k, function(z) z[2])
  m[i] <- paste(substr(m[i], 1, j[i]-1), substr(m[i], j[i]+1, n[i]), sep = '_')
  
  dat$Measurement <- m
  names(dat)[names(dat) == 'Measurement'] <- 'Species'
  
  dat <- dat %>% distinct()
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Redefine the sample event field
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat <- dat[order(dat$Date.Time, dat$Species, dat$Depth.top_m),]
  x <- dat %>% select(Data.Table, Event) %>% distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .before = Event) %>%
    select(-Event)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n----------------------------\n',
          'Finished Schnack-Schiel data',
          '\n----------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  
  # CHINARE -----------------------------------------------------------------
  
  Source <- 'CHINARE'
  
  message('\n---------------------\n',
          'Cleaning CHINARE data',
          '\n---------------------')
  
  dat <- DATA[[Source]]
  
  #' Process each data set separately. The first is the multinet, the second is
  #' the norpac net.
  
  message('\nProcessing multi-net data')
  
  d <- dat[[1]]
  
  #' Create column names
  names(d) <- c('variable', paste('sample', 1:{ncol(d)-1}, sep = '.'))
  
  #' Spelling error
  d$variable <- sub('Longtitude', 'Longitude', d$variable)
  
  #' Dates
  i <- unname(unlist(d[d$variable == 'Date', -1]))
  d[d$variable == 'Date', -1] <- as.character(as.Date(i, format = '%d/%m/%Y'))
  
  #' Times
  x <- unname(unlist(d[d$variable == 'Sampling time',-1]))
  x <- gsub(' ','',x)
  x[nchar(x) == 5] <- paste(x[nchar(x) == 5], '00', sep =':')
  x[nchar(x) == 4] <- paste(paste0('0', x[nchar(x) == 4]), '00', sep = ':')
  d[d$variable == 'Sampling time',-1] <- x
  
  #' Put data in long format
  names(d)[-1] <- d[d$variable == 'Sample ID', -1]
  d <- d[d$variable != 'Sample ID',]
  d <- melt(d, id.vars = 'variable', variable.name = 'Sample.ID')
  d <- dcast(d, Sample.ID ~ ...)
  d <- melt(d, id.vars = c('Sample.ID', 'Depth(m)', 'Station', 'Longitude', 'Latitude', 'Sampling time', 'Date'))
  names(d)[c(2,6,8)] <- c('Depth', 'Time', 'Species')
  
  #' Date-time
  d$Date.Time <- as.POSIXlt(paste(d$Date, d$Time),
                            format = c("%Y-%m-%d %H:%M:%S"))
  
  #' Depth
  d$Depth <- gsub(' ', '', d$Depth)
  d$Depth <- gsub('m', ' m', d$Depth)
  
  #' Station
  d$Station <- gsub(' ', '', d$Station)
  
  #' Species/stage
  life.stages <- c('C1-3', 'C1-C3', 'Juvenile', 'Adult', 'C4-5', 'F1-F3', 'NM', 'F4-F6', 'F4-6', 'larvae')
  life.stages <- sort(life.stages)
  
  x <- Vectorize(grepl, 'pattern')(life.stages, d$Species)
  stage.known <- apply(x, 1, any)
  y <- matrix(life.stages, 1)[rep(1,nrow(x)),]
  z <- t(y)[t(x)]
  d$Life.Stage <- NA
  d$Life.Stage[stage.known] <- z
  
  d$Species <- as.character(d$Species)
  i <- nchar(d$Species)
  j <- nchar(d$Life.Stage)
  k <- substr(d$Species, i - j + 1, i) == d$Life.Stage
  k[is.na(k)] <- FALSE
  d$Species[k] <- substr(d$Species[k], 1, i[k] - j[k] - 1)
  
  d$Life.Stage <- gsub('C1-3', 'C1-C3', d$Life.Stage)
  d$Life.Stage <- gsub('C4-5', 'C4-C5', d$Life.Stage)
  d$Life.Stage <- gsub('F4-6', 'F4-F6', d$Life.Stage)
  d$Life.Stage <- gsub('larvae', 'Larvae', d$Life.Stage)
  
  d$Net <- 'Multinet'
  d$Mesh.Size <- '200 µm'
  d$Net.Area <- '0.25 m2'
  d$Measurement.Unit <- 'ind/m3'
  
  #' Sort the data and ascribe column classes
  d <- d[,c('Sample.ID', 'Station', 'Date', 'Time', 'Date.Time', 'Longitude', 'Latitude', 'Depth', 'Net', 'Net.Area', 'Mesh.Size', 'Species', 'Life.Stage', 'value', 'Measurement.Unit')]
  
  d$Sample.ID <- as.character(d$Sample.ID)
  d$Sample.ID <- factor(d$Sample.ID, levels = sort(unique(d$Sample.ID)))
  
  d$Species <- factor(d$Species, levels = sort(unique(d$Species)))
  
  life.stages <- c('C1-C3', 'C4-C5', 'F1-F3', 'F4-F6', 'Larvae', 'Juvenile', 'Adult', 'NM')
  d$Life.Stage <- factor(d$Life.Stage, levels = life.stages)
  
  o <- order(d$Date.Time, d$Sample.ID, d$Species, d$Life.Stage)
  d <- d[o,]
  
  d$Sample.ID <- as.character(d$Sample.ID)
  d$Date.Time <- as.character(d$Date.Time)
  d$Species <- as.character(d$Species)
  d$Life.Stage <- as.character(d$Life.Stage)
  d$value <- as.numeric(d$value)
  
  
  #' Now process the second data set
  message('\nProcessing norpac net data')
  d1 <- d
  d <- dat[[2]]
  
  #' Create column names
  names(d) <- c('variable', paste('sample', 1:{ncol(d)-1}, sep = '.'))
  
  #' Dates
  i <- d[d$variable == 'sampling time', -1]
  j <- grepl('\\.', i)
  i[j] <- gsub('\\.', '/', i[j])
  i[j] <- paste0('20', i[j])
  i[j] <- format(strptime(i[j], '%Y/%m/%d'), '%Y-%m-%d')
  i[!j] <- format(strptime(i[!j], '%d/%m/%Y'), '%Y-%m-%d')
  d[d$variable == 'sampling time', -1] <- i
  
  #' Life stage and species names
  d$life.stage <- d$variable
  d$life.stage <- gsub('VI', '6', d$life.stage)
  d$life.stage <- gsub('IV', '4', d$life.stage)
  d$life.stage <- gsub('V', '5', d$life.stage)
  d$life.stage <- gsub('III', '3', d$life.stage)
  d$life.stage <- gsub('II', '2', d$life.stage)
  d$life.stage <- gsub('I', '1', d$life.stage)
  
  life.stages <- c(paste0('C', 1:5), paste0('F', 1:6), 'Adult', 'Juvenile', 'M', 'N')
  
  d$life.stage[!d$life.stage %in% life.stages] <- NA
  d$variable[d$life.stage %in% life.stages] <- NA
  
  for(i in 1:nrow(d)){
    infill <- is.na(d$variable[i])
    if(!infill) next
    d$variable[i] <- d$variable[i-1]
  }
  
  d <- d[!apply(is.na(d[,-1]), 1, all),] #' remove empty rows
  
  i <- !is.na(d$life.stage)
  d$variable[i] <- paste(d$variable[i], d$life.stage[i], sep = '_')
  d <- d[,-ncol(d)]
  
  names(d)[-1] <- d[1,-1]
  d <- d[-1,]
  
  d <- melt(d, id.vars = 'variable', variable.name = 'station')
  d <- dcast(d, station ~ ...)
  d <- melt(d, id.vars = c('station', 'sampling time', 'longitude (E)', 'latitude (S)'))
  
  names(d)[c(2,5)] <- c('date', 'species')
  
  d$species <- as.character(d$species)
  x <- strsplit(d$species, '_')
  d$life.stage <- sapply(x, function(z) z[2])
  d$species <- sapply(x, function(z) z[1])
  
  d$net <- 'Norpac'
  d$mesh.size <- '300 µm'
  d$net.area <- '0.5 m2'
  d$measurement.unit <- 'ind/1000 m3'
  d$depth <- '0-200 m'
  
  #' Sort the data and ascribe column classes
  d <- d[,c('station', 'date', 'longitude (E)', 'latitude (S)', 'depth', 'net', 'net.area', 'mesh.size', 'species', 'life.stage', 'value', 'measurement.unit')]
  
  d$station <- as.character(d$station)
  d$station <- factor(d$station, levels = sort(unique(d$station)))
  
  d$date <- as.Date(d$date)
  d$species <- factor(d$species, levels = sort(unique(d$species)))
  
  life.stages <- c('N', paste0('C', 1:5), paste0('F', 1:6), 'Juvenile', 'Adult', 'M')
  d$life.stage <- factor(d$life.stage, levels = life.stages)
  
  o <- order(d$date, d$station, d$species, d$life.stage)
  d <- d[o,]
  
  d$station <- as.character(d$station)
  d$date <- as.character(d$date)
  d$species <- as.character(d$species)
  d$life.stage <- as.character(d$life.stage)
  d$value <- as.numeric(d$value)
  
  
  #' Combine the two data sets
  message('\n', "Combining data tables")
  
  dat <- list(d, d1)
  rm(d, d1)
  
  #' Regularise the data column names
  names(dat[[1]]) <- gsub('longitude \\(E\\)', 'longitude', names(dat[[1]]))
  names(dat[[1]]) <- gsub('latitude \\(S\\)', 'latitude', names(dat[[1]]))
  regColNames <- function(dat){
    n <- names(dat)
    #' Captitalise 1st letters of each word, separate words with periods
    x <- strsplit(n, '\\.')
    n <- lapply(x, function(z){
      y <- sapply(1:length(z),
                  function(w) paste0(toupper(substr(z[w],1,1)), substr(z[w],2,nchar(z[w]))))
      paste(y, collapse = '.')})
    names(dat) <- n
    return(dat)}
  
  dat <- lapply(dat, regColNames)
  
  #' Distinguish distinct data sets
  dat <- lapply(1:length(dat), function(z) cbind(Data.Set = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  #' Create sample ID for data subsets that lack this variable, specifying the ID
  #' in accordance with the method used for the data subsets that contain this
  #' variable.
  dat$row.index <- 1:nrow(dat)
  i <- is.na(dat$Sample.ID)
  d <- dat[i,]
  x <- do.call('rbind',
               lapply(unique(d$Data.Set), function(z){
                 x <- unique(d[d$Data.Set == z, c('Station', 'Date', 'Depth')])
                 cbind(Sample.ID = as.character(1:nrow(x)), x)}))
  d <- d[,names(d) != 'Sample.ID']
  d <- left_join(d, x)
  d <- d[,names(dat)]
  dat <- rbind(dat[!i,], d)
  dat <- dat[order(dat$row.index),]
  dat <- dat[,names(dat) != 'row.index']
  rm(d, i, x)
  
  #' Depth is given as a range. Split this into min/max/mid
  x <- gsub(' m', '', dat$Depth)
  x <- strsplit(x, '-')
  i <- as.numeric(sapply(x, function(z) z[1]))
  j <- as.numeric(sapply(x, function(z) z[2]))
  dat$Depth.Mid <- 0.5 * {i + j}
  dat$Depth.Min <- i
  dat$Depth.Max <- j
  dat <- dat[,names(dat) != 'Depth']
  i <- 1:which(names(dat) == 'Latitude')
  j <- grep('Depth', names(dat))
  k <- 1:ncol(dat); k <- k[!k %in% c(i,j)]
  dat <- dat[,c(i,j,k)]
  
  #' Times are reported only for one dataset
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  
  dat$Time.Flag <- 'Assumed local time - time zone not specified'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  #' Column classes
  dat$Longitude <- as.numeric(dat$Longitude)
  dat$Latitude <- as.numeric(dat$Latitude)
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  # unique(dat$Latitude.S[!i])
  dat <- dat[i,]
  
  #' Sort the data by date/time, species, and depth
  noTime <- dat$Time == ''
  dat$Date.Time[noTime] <- paste(dat$Date[noTime], '12:00:00')
  dat$Date.Time <- strptime(dat$Date.Time, format = '%Y-%m-%d %H:%M:%S')
  dat$Species <- factor(dat$Species, levels = sort(unique(dat$Species)))
  life.stages <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C1-C3', 'C4-C5',
                   'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F1-F3', 'F4-F6',
                   'Larvae', 'Juvenile', 'Adult', 'M', 'N', 'NM')
  dat$Life.Stage <- factor(dat$Life.Stage, levels = life.stages)
  o <- order(dat$Date.Time, dat$Species, dat$Life.Stage, dat$Depth.Mid)
  dat$Date.Time <- as.character(dat$Date.Time)
  dat$Date.Time[noTime] <- ''
  dat$Species <- as.character(dat$Species)
  dat$Life.Stage <- as.character(dat$Life.Stage)
  
  dat <- dat[o,]
  rownames(dat) <- as.character(1:nrow(dat))
  
  #' Reformat sample event from station
  x <- dat %>% select(Data.Set, Station) %>% distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .before = Station) %>%
    select(-Station)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n---------------------\n',
          'Finished CHINARE data',
          '\n---------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # Palmer LTER (2009-2017) - MOCNESS ---------------------------------------------------
  
  Source <- 'Palmer.LTER_MOCNESS'
  
  message('\n---------------------------------\n',
          'Cleaning Palmer LTER MOCNESS data',
          '\n---------------------------------')
  
  dat <- DATA[[Source]]
  
  #' Melt data into long form
  id_vars <- names(dat)[1:which(names(dat) == 'maxVolFiltM3')]
  dat <- melt(data = dat, id.vars = id_vars, variable.name = 'Taxa',
              value.name = 'Value', na.rm = FALSE)
  
  #' Infill some NA values in OpenTime
  i <- is.na(dat$OpenTime)
  x1 <- strptime(dat$minDHMSNet, format = '%Y-%m-%dT%H:%M:%S')
  x2 <- strptime(dat$maxDHMSNet, format = '%Y-%m-%dT%H:%M:%S')
  x <- x2-x1
  dat$OpenTime[i] <- round(as.numeric(x[i]), 1)
  
  #' Include a sampling equipment column
  dat$SamplingGear <- 'MOCNESS'
  
  #' Include a mesh size column
  dat$MeshSize <- paste(500, paste0('\U03bc', 'm'))
  
  #' Measurement and units
  dat$Measurement <- 'numerical density'
  dat$MeasurementUnit <- 'individuals / m3'
  
  #' Replace abbreviations in taxa column with full names
  species.list <- data.frame(
    abbrv = c('Lrangii', 'Gymnosomata', 'Tomopteris', 'Chaetognatha', 'Mgerlachei', 
              'Cacutus', 'Cpropinquus', 'Rgigas', 'Pantarctica', 'Ostracoda',
              'Amphipoda', 'Ecrystal', 'Tmacrura', 'Sthompsoni'),
    full = c('Limacina rangii', 'Gymnosome pteropod', 'Tomopteris spp. polychaete',
             'Chaetognath', 'Metridia gerlachei', 'Calanoides acutus',
             'Calanus propinquus', 'Rhincalanus gigas', 'Paraeuchaeta antarctica',
             'Ostracod', 'Amphipod', 'Euphausia crystallorophias', 
             'Thysanoessa macrura', 'Salpa thompsoni'))
  
  species.list_ <- setNames(species.list$full, species.list$abbrv)
  dat$Taxa <- species.list_[dat$Taxa]
  
  #' Include a copepodite stage column. John Conroy told me that these data (for
  #' the species I'm interested in) generally correspond to copepodite stages
  #' C4-C6, although the samples are rarely, if ever, resolved to copepodite stage.
  dat$CopepoditeStage <- 'C4-C6'
  
  dat <- dat %>% distinct()
  
  #' Include only local time start/end -- local time is behind GMT
  hs <- as.numeric(substr(dat$TimeStart, 1, 2))
  hl <- as.numeric(substr(dat$TimeLocal, 1, 2))
  date.shift <- hs < hl
  dts <- strptime(paste(dat$Date, dat$TimeStart), format = '%Y-%m-%d %H:%M:%S')
  dte <- strptime(paste(dat$Date, dat$TimeEnd), format = '%Y-%m-%d %H:%M:%S')
  dtd <- dte - dts #' event duration
  dtls <- strptime(paste(dat$Date, dat$TimeLocal), format = '%Y-%m-%d %H:%M:%S')
  dtls[date.shift] <- dtls[date.shift] - 60*60*24
  dtle <- dtls + dtd
  dat <- dat %>%
    mutate(Date = format(dtls, '%Y-%m-%d'),
           TimeStart = format(dtls, '%H:%M:%S'),
           TimeEnd = format(dtle, '%H:%M:%S')) %>%
    select(-TimeLocal)
  rm(hs, hl, date.shift, dts, dte, dtd, dtls, dtle)
  
  dat$Time.Flag <- 'Local time'
  dat$Time.Flag[is.na(dat$TimeStart) | dat$TimeStart == ''] <- ''
  
  
  #' Remove rows outside of selected latitudinal range
  i <- lat_lim[1] <= dat$LatitudeStart & dat$LatitudeStart <= lat_lim[2]
  dat <- dat[i,]
  
  #' Sort the data by date/time, measurement type, and depth
  dat$Date.Time <- strptime(paste(dat$Date, dat$TimeStart), format = '%Y-%m-%d %H:%M:%S')
  dat <- dat[order(dat$Date.Time, dat$Taxa, dat$NetDepthEnd),]
  dat <- dat %>% select(-Date.Time)
  
  #' Reformat sample event
  x <- dat %>% select(CruiseName, CruiseTow) %>% distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .before = CruiseTow) %>%
    select(-CruiseTow)
  
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n---------------------------------\n',
          'Finished Palmer LTER MOCNESS data',
          '\n---------------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # Palmer LTER (1993-2008) - non stratified depths  ---------------------------------------------------
  
  Source <- 'Palmer.LTER_non_stratified'
  
  message('\n----------------------------------------------\n',
          'Cleaning Palmer LTER non depth-stratified data',
          '\n----------------------------------------------')
  
  dat <- DATA[[Source]]
  
  #' Melt data into long form
  id_vars <- names(dat)[1:which(names(dat) == 'SiphonVol..ml.1000m3.')]
  dat <- melt(data = dat, id.vars = id_vars, variable.name = 'Measurement',
              value.name = 'Value', na.rm = FALSE)
  
  #' Sort out names of coords -- get rid of degree symbols
  names(dat)[names(dat) == 'LatitudeStart..º.'] <- 'LatitudeStart'
  names(dat)[names(dat) == 'LongitudeStart..º.'] <- 'LongitudeStart'
  names(dat)[names(dat) == 'LatitudeEnd..º.'] <- 'LatitudeEnd'
  names(dat)[names(dat) == 'LongitudeEnd..º.'] <- 'LongitudeEnd'
  #' Correct the sign of the coords (south and west are negative, and all Palmer
  #' stations are in southwest sector)
  dat$LatitudeStart <- -abs(dat$LatitudeStart)
  dat$LongitudeStart <- -abs(dat$LongitudeStart)
  dat$LatitudeEnd <- -abs(dat$LatitudeEnd)
  dat$LongitudeEnd <- -abs(dat$LongitudeEnd)
  #' Replace zeros in the coords with NAs because, presumably, these are missing
  #' measurements -- it's only an issue for the end coordinates
  dat$LatitudeStart[dat$LatitudeStart == 0] <- NA
  dat$LongitudeStart[dat$LongitudeStart == 0] <- NA
  dat$LatitudeEnd[dat$LatitudeEnd == 0] <- NA
  dat$LongitudeEnd[dat$LongitudeEnd == 0] <- NA
  
  #' Tow durations
  names(dat)[names(dat) == 'TowDuration..min.'] <- 'TowDuration'
  #' Duration is reported in minutes where the data provide reliable values for
  #' start/end times. Duration can be used to infill end times.
  noEndTime <- dat$TimeEnd.GMT == '00:00:00'
  dat$TimeEnd.GMT[noEndTime] <- ''
  knownDuration <- dat$TowDuration > 0
  dat$TowDuration[!knownDuration] <- NA
  
  #' Use local times, which are behind GMT
  hs <- as.numeric(substr(dat$TimeStart.GMT, 1, 2))
  hl <- as.numeric(substr(dat$TimeLocal.CLST, 1, 2))
  date.shift <- hl > hs
  dts <- strptime(paste(dat$Date.GMT, dat$TimeStart.GMT), format = '%Y-%m-%d %H:%M:%S') #' date-time start (GMT)
  dte <- strptime(paste(dat$Date.GMT, dat$TimeEnd.GMT), format = '%Y-%m-%d %H:%M:%S') #' date-time end (GMT)
  i <- dat$TimeEnd.GMT == '' & !is.na(dat$TowDuration)
  dte[i] <- dts[i] + dat$TowDuration[i]*60 #' infill end times using tow duration
  ds <- as.Date(dat$Date.GMT, format = '%Y-%m-%d') #' date start (GMT)
  dsl <- ds #' date start (local -- behind GMT)
  dsl[date.shift] <- dsl[date.shift] - 1
  dtsl <- strptime(paste(dsl, dat$TimeLocal.CLST), format = '%Y-%m-%d %H:%M:%S') #' date-time start (local)
  time.diff <- dts - dtsl
  dtel <- dte - time.diff #' date-time end (local)
  dat <- dat %>%
    rename(Date = Date.GMT) %>% mutate(Date = as.character(dsl)) %>%
    select(-TimeStart.GMT) %>% rename(TimeStart = TimeLocal.CLST) %>%
    rename(TimeEnd = TimeEnd.GMT) %>% mutate(TimeEnd = format(dtel, '%H:%M:%S'))
  
  dat$Time.Flag <- 'Local time'
  dat$Time.Flag[is.na(dat$TimeStart) | dat$TimeStart == ''] <- ''
  
  #' Adjust more column names
  names(dat)[names(dat) == 'Heading..º.'] <- 'Heading_degrees'
  names(dat)[names(dat) == 'WaterDepth..m.'] <- 'WaterDepth_m'
  names(dat)[names(dat) == 'WindDirection..º.'] <- 'WindDirection_degrees'
  names(dat)[names(dat) == 'DepthMaximum..m.'] <- 'DepthMaximum_m'
  names(dat)[names(dat) == 'VolumeFilteredM3..m..'] <- 'VolumeFiltered_m3'
  names(dat)[names(dat) == 'SiphonNum..num.1000m..'] <- 'SiphonNum_numPer1000m3'
  names(dat)[names(dat) == 'SiphonVol..ml.1000m3.'] <- 'SiphonVol_mlPer1000m3'
  
  #' Untangle the various measurements
  dat$Measurement <- as.character(dat$Measurement)
  
  den <- grepl('Num..', dat$Measurement)
  vol <- grepl('Vol..', dat$Measurement)
  dat_d <- dat[den,]
  dat_v <- dat[vol,]
  di <- strsplit(dat_d$Measurement, 'Num..', fixed = TRUE)
  vi <- strsplit(dat_v$Measurement, 'Vol..', fixed = TRUE)
  
  dat_d$Taxa <- sapply(1:length(di), function(z) di[[z]][1])
  dat_d$MeasurementUnit <- sapply(1:length(di), function(z) di[[z]][2])
  dat_v$Taxa <- sapply(1:length(vi), function(z) vi[[z]][1])
  dat_v$MeasurementUnit <- sapply(1:length(vi), function(z) vi[[z]][2])
  dat_d$Measurement <- 'numerical density'
  dat_v$Measurement <- 'volumetric density'
  dat_d$MeasurementUnit <- 'numPer1000m3'
  dat_v$MeasurementUnit <- 'mlPer1000m3'
  dat <- rbind(dat_d, dat_v)
  rm(dat_d, dat_v)
  
  #' Clarify the Taxa
  species_list <- data.frame(
    abbrv = c('Siphon', 'Hydrozoa_o', 'Ctenoph', 'Clio', 'Limacina', 'Clione',
              'Pteropod_o', 'Cephalop', 'Tomopter', 'Polychae_o', 'Pseudosa',
              'Chaetogn_o', 'Copepoda', 'Ostracod', 'Mysida', 'Themisto',
              'Amphipod_o', 'Esuperba', 'Ecrystal', 'Efrigida', 'Etriacan',
              'Thysanoe', 'Euphaus_o', 'SalpEmb', 'SalpSol', 'SalpAgg',
              'Pantarct', 'Teleoste', 'Cnidaria_o', 'Decapoda', 'Other'),
    full = c('Siphonophorae', 'Hydrozoa (other hydrozoa, excluding Siphonophorae)',
             'Ctenophora', 'Clio spp. pteropods', 'Limacina helicina',
             'Clione spp. pteropods', 'Pteropoda (other pteropods, excluding defined taxa)',
             'Cephalopoda', 'Tomopteris spp. polychaetes',
             'Polychaeta (other polychaetes, excluding Tomopteris)',
             'Pseudosagitta spp. chaetognaths',
             'Sagittoidea (other chaetognaths, excluding Pseudosagitta)',
             'Copepoda', 'Ostracoda', 'Mysida', 'Themisto gaudichaudii',
             'Amphipoda (other amphipods, excluding defined taxa)',
             'Euphausia superba', 'Euphausia crystallorophias', 'Euphausia frigida',
             'Euphausia triacantha', 'Thysanoessa macrura',
             'Euphausiacea (other euphausiids, excluding defined taxa) (mostly immature stages)',
             'Salpa thompsoni embryo stage', 'Salpa thompsoni solitary stage',
             'Salpa thompsoni aggregate stage', 'Pleuragramma antarcticum',
             'Teleostei', 'Cnidaria (other Cnidaria, excluding defined taxa)',
             'Decapoda', 'Other')
  )
  
  species_list_ <- setNames(species_list$full, species_list$abbrv)
  dat$Taxa <- unname(species_list_[dat$Taxa])
  
  dat$Net.type <- 'Metro net'
  dat$MeshSize <- paste(700, paste0('\U03bc', 'm'))
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$LatitudeStart & dat$LatitudeStart <= lat_lim[2]
  dat <- dat[i,]
  
  #' Sort the data
  dat$Date.Time <- strptime(paste(dat$Date, dat$TimeStart), format = '%Y-%m-%d %H:%M:%S')
  dat <- dat[order(dat$Date.Time, dat$Taxa, dat$Measurement),]
  dat$Date.Time <- NULL
  
  #' Reformat the sample event field
  x <- dat %>% select(CruiseName, Event) %>% distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .before = CruiseTow) %>%
    select(-CruiseTow)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n----------------------------------------------\n',
          'Finished Palmer LTER non depth-stratified data',
          '\n----------------------------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # AtlantNIRO --------------------------------------------------------------
  
  Source <- 'AtlantNIRO'
  
  message('\n------------------------\n',
          'Cleaning AtlantNIRO data',
          '\n------------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' Identify different data tables
  dat <- lapply(1:length(dat), function(z) cbind(Data.Table = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  dat <- dat[,!names(dat) %in% c('',' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN',
      'Taxonomic.Modifier', 'Life.Stage', 'Plankton.Staging.Code', 'Sex',
      'Measurement.Type', 'Water.Strained', 'Original.Value', 'Original.Units',
      'Value.Per.Volume', 'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  #' Omit empty rows and records missing basic information
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' ~~~~~~~~~~~~~
  #' Clean columns
  #' ~~~~~~~~~~~~~
  
  #' Date/time
  
  x <- dat$Time.GMT
  hr <- as.character(floor(x))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  times <- paste(hr, min, '00', sep = ':')
  dat$Time.GMT <- times
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Time.GMT, -Date.Time.GMT, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time - converted from GMT'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  #' Tow type
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  #' Sampling gear
  dat$Gear <- gsub(127, 'bottle', dat$Gear)
  dat$Gear <- gsub(235, 'Juday net', dat$Gear)
  
  #' Mesh Size
  dat$Mesh.Size[dat$Gear == 'bottle'] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  #' Species descriptors
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  #' Life stage/maturity
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  #' Sex
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  #' Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  #' Measurement type
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Value.Per.Volume <- removeWhiteSpacePadding(dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('n/a', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- as.numeric(dat$Value.Per.Volume)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Value.Per.Area <- removeWhiteSpacePadding(dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('null', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('n/a', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- as.numeric(dat$Value.Per.Area)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  dat$Scientific.Name <- scientific.name
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    x <- paste0('c', 1:5) %in% z
    if(any(x)) return(which(x)) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- paste0('c', i[!is.na(i)])
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('calyptopis','eggs','furcilia','larva','nauplii')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('6406', 'Gizhiga', dat$Ship)
  dat$Ship <- gsub('6727', 'Nekton', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- ''
  
  dat$Institute <- gsub('848', 'AtlantNIRO - Atlantic Research Inst of Fishing Economy and Oceanography', dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers #' this looks like a duplicate of Scientific.Name.Plus.Modifiers
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  dat$Time[dat$no.time] <- '12:00:00'
  
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  x <- dat %>%
    select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original, Date, time.inc) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .after = Ship.Cruise) %>%
    select(-Station.ID.Original, -time.inc, -no.time)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n------------------------\n',
          'Finished AtlantNIRO data',
          '\n------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # ELTANIN -----------------------------------------------------------------
  
  Source <- 'ELTANIN'
  
  message('\n---------------------\n',
          'Cleaning ELTANIN data',
          '\n---------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' Identify different data tables
  dat <- lapply(1:length(dat), function(z) cbind(Data.Table = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  dat <- dat[,!names(dat) %in% c('',' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN',
      'Taxonomic.Modifier', 'Life.Stage', 'Plankton.Staging.Code', 'Sex',
      'Measurement.Type', 'Water.Strained', 'Original.Value', 'Original.Units',
      'Value.Per.Volume', 'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.GMT
  x[x == 99.99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  
  dat$Time.GMT <- x
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.GMT[i]
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Time.GMT, -Date.Time.GMT, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time - converted from GMT'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(112, 'Plankton Net (type not specified)', dat$Gear)
  dat$Gear <- gsub(113, 'Multiple Plankton Sampler (MPS)', dat$Gear)
  dat$Gear <- gsub(114, 'Bathypelagic Plankton Sampler (BPS)', dat$Gear)
  dat$Gear <- gsub(115, 'Indian Ocean Standard Net (IOSN)', dat$Gear)
  dat$Gear <- gsub(116, 'Clarke-Bumpus Sampler', dat$Gear)
  dat$Gear <- gsub(117, 'Neuston Net', dat$Gear)
  dat$Gear <- gsub(118, 'Bongo Net', dat$Gear)
  dat$Gear <- gsub(132, 'WP-2 (UNESCO Working Party 2)', dat$Gear)
  dat$Gear <- gsub(147, 'Micro Net (not specified)', dat$Gear)
  dat$Gear <- gsub(148, 'Open Net (not specified)', dat$Gear)
  
  dat$Mesh.Size[dat$Gear == 'bottle'] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  #' Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Occurrence <- removeWhiteSpacePadding(dat$Original.Value)
  dat$Original.Value <- as.numeric(dat$Occurrence)
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value == 0] <- 'absent'
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value > 0] <- 'present'
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Value.Per.Volume <- removeWhiteSpacePadding(dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('n/a', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('null', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- as.numeric(dat$Value.Per.Volume)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('-----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Value.Per.Area <- removeWhiteSpacePadding(dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('null', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('n/a', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- as.numeric(dat$Value.Per.Area)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  j <- !is.na(scientific.name)
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  dat <- dat[j,]
  scientific.name <- scientific.name[j]
  scientific.name.modifier <- scientific.name.modifier[j]
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    x <- paste0('c', 1:5) %in% z
    if(any(x)) return(which(x)) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- paste0('c', i[!is.na(i)])
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('actinotrocha larva','cyphonautes larva','eggs','juvenile','larva', 'medusae', 'polyp', 'tornaria larva', 'trochophore larva')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('1514', 'Eltanin', dat$Ship)
  dat$Ship <- gsub('2063', 'Ice Island', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('240', 
                         'United States Antarctic Research Project - USAP/USARP',
                         dat$Project.ID)
  dat$Institute <- gsub('289',
                        'SOSC - Smithsonian Oceanographic Sorting Center (Washington - DC)',
                        dat$Institute)
  dat$Institute <- gsub('393',
                        'SIO - Scripps Institution of Oceanography (La Jolla - CA)',
                        dat$Institute)
  dat$Institute <- gsub('408',
                        'University of Maine - Walpole',
                        dat$Institute)
  dat$Institute <- gsub('438',
                        'Texas A&M University (College Station - TX)',
                        dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers #' there's no indication what these numbers represent
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  #' Values of -999.9 placeholders with NA
  dat$Depth.top[dat$Depth.top == -999.9] <- NA
  dat$Depth.bottom[dat$Depth.bottom == -999.9] <- NA
  
  #' Correct reversed top/bottom depths
  i <- dat$Depth.bottom - dat$Depth.top
  i <- !is.na(i) & i < 0
  x <- data.frame(Depth.bottom = dat$Depth.bottom[i], Depth.top = dat$Depth.top[i])
  dat[c('Depth.bottom','Depth.top')][i,] <- x[c('Depth.top','Depth.bottom')]
  
  #' Omit records lacking any depth value
  i <- is.na(dat$Depth.bottom) & is.na(dat$Depth.top)
  dat <- dat[!i,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  #' Not all records have times so include depth as a further deliminator for
  #' gears that do not sample multiple depths in single deployments.
  # unique(dat$Gear[dat$no.time])
  gear.depth <- data.frame(
    Gear = c('Multiple Plankton Sampler (MPS)', 'Bathypelagic Plankton Sampler (BPS)',
             'Plankton Net (type not specified)', 'Clarke-Bumpus Sampler',
             'Indian Ocean Standard Net (IOSN)', 'Micro Net (not specified)',
             'WP-2 (UNESCO Working Party 2)', 'Bongo Net'),
    Multiple.Depths = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)) # input TRUE when unknown
  dat <- suppressMessages(left_join(dat, gear.depth))
  dat$Multiple.Depths[is.na(dat$Multiple.Depths)] <- TRUE
  
  dat$Time[dat$no.time] <- '12:00:00'
  
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  if(any(dat$Multiple.Depths) & any(!dat$Multiple.Depths)){
    x1 <- dat %>%
      filter(!Multiple.Depths) %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original, Date, time.inc, Depth.top, Depth.bottom) %>%
      distinct()
    x2 <- dat %>%
      filter(Multiple.Depths) %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original, Date, time.inc) %>%
      distinct()
    
    x1$Sample.event <- 1:nrow(x1)
    x2$Sample.event <- {1:nrow(x2)} + nrow(x1)
    d1 <- dat %>%
      filter(!Multiple.Depths) %>%
      left_join(x1)
    d2 <- dat %>%
      filter(Multiple.Depths) %>%
      left_join(x2)
    
    dat <- bind_rows(d1, d2)
    dat <- dat[order(dat$Date, dat$time.inc),] %>%
      relocate(Sample.event, .after = Ship.Cruise) %>%
      select(-Station.ID.Original, -time.inc, -no.time, -Multiple.Depths)
    
    d <- dat %>% 
      select(Sample.event) %>%
      distinct() %>%
      rename(event = Sample.event)
    d$Sample.event <- 1:nrow(d)
    d <- setNames(d$Sample.event, d$event)
    dat$Sample.event <- d[as.character(dat$Sample.event)]
    
    rm(x1, x2, d1, d2, d)
  }else{
    x <- dat %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original, Date, time.inc) %>%
      distinct()
    x$Sample.event <- 1:nrow(x)
    dat <- suppressMessages(left_join(dat, x)) %>%
      relocate(Sample.event, .after = Ship.Cruise) %>%
      select(-Station.ID.Original, -time.inc, -no.time)
    rm(x)
  }
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n---------------------\n',
          'Finished ELTANIN data\n',
          '\n---------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  
  # Foxton_1956 -------------------------------------------------------------
  
  Source <- 'Foxton_1956'
  
  message('\n---------------------------\n',
          'Cleaning Foxton (1956) data',
          '\n---------------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' Identify different data tables
  dat <- lapply(1:length(dat), function(z) cbind(Data.Table = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  dat <- dat[,!names(dat) %in% c('',' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN',
      'Taxonomic.Modifier', 'Life.Stage', 'Plankton.Staging.Code', 'Sex',
      'Measurement.Type', 'Water.Strained', 'Original.Value', 'Original.Units',
      'Value.Per.Volume', 'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  #' Omit empty rows and records missing basic information
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.GMT
  x[x == 99.99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  dat$Time.GMT <- x
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.GMT[i]
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Time.GMT, -Date.Time.GMT, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time - converted from GMT'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(201, 'N70V Vertical Closing Net', dat$Gear)
  
  dat$Mesh.Size[dat$Gear == 'bottle'] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  #' Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    x <- paste0('c', 1:5) %in% z
    if(any(x)) return(which(x)) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- paste0('c', i[!is.na(i)])
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('actinotrocha larva','cyphonautes larva','eggs','juvenile','larva', 'medusae', 'polyp', 'tornaria larva', 'trochophore larva')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('5750', 'Discovery II', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('470', 'Discovery Investigations', dat$Project.ID)
  
  dat$Institute <- gsub('1331',
                        'National Institute of Oceanography (United Kingdom)',
                        dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers #' there's no indication what these numbers represent
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  #' No times recorded. Only one gear used -- a vertical net that samples multiple
  #' depths. Depth cannot be used to delineate events.
  
  dat$Time[dat$no.time] <- '12:00:00'
  
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  x <- dat %>%
    select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original, Date, time.inc) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .after = Ship.Cruise) %>%
    select(-Station.ID.Original, -time.inc, -no.time)
  
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  
  message('\n---------------------------\n',
          'Finished Foxton (1956) data',
          '\n---------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # JARE --------------------------------------------------------------------
  
  Source <- 'JARE'
  
  message('\n------------------\n',
          'Cleaning JARE data',
          '\n------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' Identify different data tables
  dat <- lapply(1:length(dat), function(z) cbind(Data.Table = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  dat <- dat[,!names(dat) %in% c('', ' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN', 'Taxonomic.Modifier',
      'Life.Stage', 'Plankton.Staging.Code', 'Sex', 'Measurement.Type',
      'Water.Strained', 'Original.Value', 'Original.Units', 'Value.Per.Volume',
      'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.LOC
  x[x == -99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  
  dat$Time.LOC <- x
  dat$Date.LOC <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.Time <- as.character(strptime(paste(dat$Date.LOC, dat$Time.LOC), format = '%Y-%m-%d %H:%M:%S'))
  
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.LOC[i]
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.LOC, -Time.GMT, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(101, 'NORPAC (North Pacific Standard Net)', dat$Gear)
  dat$Gear <- gsub(125, 'ORI-C Net', dat$Gear)
  dat$Gear <- gsub(143, 'Motoda Horizontal Net (MTD)', dat$Gear)
  dat$Gear <- gsub(204, 'Nakai Fish Larvae Net', dat$Gear)
  dat$Gear <- gsub(205, 'Modified NIPR-1', dat$Gear)
  dat$Gear <- gsub(207, 'NIPR parasol net', dat$Gear)
  dat$Gear <- gsub(251, 'Twin NORPAC Net', dat$Gear)
  
  dat$Mesh.Size[dat$Gear == 'bottle'] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  #' Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Occurrence <- removeWhiteSpacePadding(dat$Original.Value)
  dat$Original.Value <- as.numeric(dat$Occurrence)
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value == 0] <- 'absent'
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value > 0] <- 'present'
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Value.Per.Volume <- removeWhiteSpacePadding(dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('n/a', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('null', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- as.numeric(dat$Value.Per.Volume)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('-----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Value.Per.Area <- removeWhiteSpacePadding(dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('null', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('n/a', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- as.numeric(dat$Value.Per.Area)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('c3-6','c4-6')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- i[!is.na(i)]
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  dat$Copepodite.Stage <- gsub('-','-C',dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('eggs','larva','nauplii','post-calyptopis')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('4743', 'Ice Breaker Fuji', dat$Ship)
  dat$Ship <- gsub('5140', 'Shirase', dat$Ship)
  dat$Ship <- gsub('5445', 'Tangoroa', dat$Ship)
  dat$Ship <- gsub('6744', 'Ice Camp', dat$Ship)
  dat$Ship <- gsub('9001', 'Syowa Station', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('343', 'Japanese Antarctic Research Expedition', dat$Project.ID)
  
  dat$Institute <- gsub('1057', 'National Institute of Polar Research - NIPR (Tokyo)', dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers #' duplication of Scientific.Name.Plus.Modifiers
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  # any(dat$no.time)
  # unique(dat$Gear[dat$no.time])
  
  #' Not all records have times so include depth as a further deliminator for gears
  #' that do not sample multiple depths in single deployments.
  # unique(dat$Gear[dat$no.time])
  gear.depth <- data.frame(
    Gear = c('Motoda Horizontal Net (MTD)', 'ORI-C Net',
             'NORPAC (North Pacific Standard Net)', 'Modified NIPR-1',
             'NIPR parasol net'),
    Multiple.Depths = c(TRUE, TRUE, FALSE, TRUE, TRUE)) #' input TRUE when unknown
  dat <- suppressMessages(left_join(dat, gear.depth))
  dat$Multiple.Depths[is.na(dat$Multiple.Depths)] <- TRUE
  
  dat$Time[dat$no.time] <- '12:00:00'
  
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  
  if(any(dat$Multiple.Depths) & any(!dat$Multiple.Depths)){
    x1 <- dat %>%
      filter(!Multiple.Depths) %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
             Date, time.inc, Depth.top, Depth.bottom) %>%
      distinct()
    x2 <- dat %>%
      filter(Multiple.Depths) %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
             Date, time.inc) %>%
      distinct()
    
    x1$Sample.event <- 1:nrow(x1)
    x2$Sample.event <- {1:nrow(x2)} + nrow(x1)
    d1 <- dat %>%
      filter(!Multiple.Depths) %>%
      left_join(x1)
    d2 <- dat %>%
      filter(Multiple.Depths) %>%
      left_join(x2)
    
    dat <- bind_rows(d1, d2)
    dat <- dat[order(dat$Date, dat$time.inc),] %>%
      relocate(Sample.event, .after = Ship.Cruise) %>%
      select(-Station.ID.Original, -time.inc, -no.time, -Multiple.Depths)
    
    d <- dat %>% 
      select(Sample.event) %>%
      distinct() %>%
      rename(event = Sample.event)
    d$Sample.event <- 1:nrow(d)
    d <- setNames(d$Sample.event, d$event)
    dat$Sample.event <- d[as.character(dat$Sample.event)]
    
    rm(x1, x2, d1, d2, d)
  }else{
    x <- dat %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
             Date, time.inc) %>%
      distinct()
    x$Sample.event <- 1:nrow(x)
    dat <- suppressMessages(left_join(dat, x)) %>%
      relocate(Sample.event, .after = Ship.Cruise) %>%
      select(-Station.ID.Original, -time.inc, -no.time)
    rm(x)
  }
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n------------------\n',
          'Finished JARE data',
          '\n------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # OB_1955-1957 ------------------------------------------------------------
  
  Source <- 'OB_1955_1957'
  
  message('\n----------------------------\n',
          'Cleaning OB (1955-1957) data',
          '\n----------------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' Identify different data tables
  dat <- lapply(1:length(dat), function(z) cbind(Data.Table = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  dat <- dat[,!names(dat) %in% c('', ' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN',
      'Taxonomic.Modifier', 'Life.Stage', 'Plankton.Staging.Code', 'Sex',
      'Measurement.Type', 'Water.Strained', 'Original.Value', 'Original.Units',
      'Value.Per.Volume', 'Unit.Value.Per.Volume',
      'Flag.Global.Annual.Range.Per.Vol', 'Flag.Basin.Annual.Range.Per.Vol',
      'Flag.Basin.Seasoinal.Range.Per.Vol', 'Flag.Basin.Monthly.Range.Per.Vol',
      'Value.Per.Area', 'Units.Value.Per.Area', 'Flag.Global.Annual.Range.Per.Area',
      'Flag.Basin.Annual.Range.Per.Area', 'Flag.Basin.Seasoinal.Range.Per.Area',
      'Flag.Basin.Monthly.Range.Per.Area', 'Scientific.Name.Plus.Modifiers',
      'Record.ID', 'Dataset.ID', 'Ship', 'Project.ID', 'Institute',
      'Cruise.ID.Original', 'Station.ID.Original', 'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.GMT
  x[x == 99.99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  dat$Time.GMT <- x
  x <- dat$Time.LOC
  x[x == -99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  dat$Time.LOC <- x
  
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.LOC <- dat$Date.GMT
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  dat$Date.Time.LOC <- as.character(strptime(paste(dat$Date.LOC, dat$Time.LOC), format = '%Y-%m-%d %H:%M:%S'))
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  i <- !is.na(dat$Date.Time.LOC)
  dat$Date.Time[i] <- dat$Date.Time.LOC[i]
  
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.LOC[i]
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.GMT[i]
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Date.LOC, -Time.GMT, -Date.Time.GMT, -Date.Time.LOC, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(214, 'JUDAY38 NET', dat$Gear)
  dat$Gear <- gsub(215, 'NANSEN15 NET', dat$Gear)
  dat$Gear <- gsub(216, 'K100 Net', dat$Gear)
  dat$Gear <- gsub(217, 'Bogorov-Rass Net', dat$Gear)
  dat$Gear <- gsub(238, 'JUDAY Net 37/50 (mouth diam = 37 cm ; mouth area = 0.1 sq-meters)', dat$Gear)
  dat$Gear <- gsub(248, 'JUDAY 80/113 Oceanic Model', dat$Gear)
  
  dat$Mesh.Size[dat$Gear == 'bottle'] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  # Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Occurrence <- removeWhiteSpacePadding(dat$Original.Value)
  dat$Original.Value <- as.numeric(dat$Occurrence)
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value == 0] <- 'absent'
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value > 0] <- 'present'
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Value.Per.Volume <- removeWhiteSpacePadding(dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('n/a', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('null', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- as.numeric(dat$Value.Per.Volume)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('-----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Value.Per.Area <- removeWhiteSpacePadding(dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('null', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('n/a', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- as.numeric(dat$Value.Per.Area)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('c4','c5')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- i[!is.na(i)]
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('juvenile')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('6478', 'Ob', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('458',
                         'Soviet Antarctic Expedition - SAE',
                         dat$Project.ID)
  
  dat$Institute <- gsub('1037',
                        'Zoological Institute Russian Academy of Science (St. Petersburg - Russia)',
                        dat$Institute)
  dat$Institute <- gsub('847',
                        'Arctic and Antarctic Scientific Research Institute (AARI)',
                        dat$Institute)
  
  dat$Taxa <- gsub('Calanus acutus', 'Calanoides acutus', dat$Taxa)
  
  taxa.modifier <- dat$Taxa.Modifiers #' duplication of Scientific.Name.Plus.Modifiers
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  #' Not all records have times so include depth as a further deliminator for gears
  #' that do not sample multiple depths in single deployments.
  # unique(dat$Gear[dat$no.time])
  gear.depth <- data.frame(
    Gear = c('K100 Net', 'JUDAY Net 37/50 (mouth diam = 37 cm ; mouth area = 0.1 sq-meters)'),
    Multiple.Depths = c(TRUE, TRUE)) # input TRUE when unknown
  dat <- suppressMessages(left_join(dat, gear.depth))
  dat$Multiple.Depths[is.na(dat$Multiple.Depths)] <- TRUE
  
  dat$Time[dat$no.time] <- '12:00:00'
  
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  if(any(dat$Multiple.Depths) & any(!dat$Multiple.Depths)){
    x1 <- dat %>%
      filter(!Multiple.Depths) %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
             Date, time.inc, Depth.top, Depth.bottom) %>%
      distinct()
    x2 <- dat %>%
      filter(Multiple.Depths) %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
             Date, time.inc) %>%
      distinct()
    
    x1$Sample.event <- 1:nrow(x1)
    x2$Sample.event <- {1:nrow(x2)} + nrow(x1)
    d1 <- dat %>%
      filter(!Multiple.Depths) %>%
      left_join(x1)
    d2 <- dat %>%
      filter(Multiple.Depths) %>%
      left_join(x2)
    
    dat <- bind_rows(d1, d2)
    dat <- dat[order(dat$Date, dat$time.inc),] %>%
      relocate(Sample.event, .after = Ship.Cruise) %>%
      select(-Station.ID.Original, -time.inc, -no.time, -Multiple.Depths)
    
    d <- dat %>% 
      select(Sample.event) %>%
      distinct() %>%
      rename(event = Sample.event)
    d$Sample.event <- 1:nrow(d)
    d <- setNames(d$Sample.event, d$event)
    dat$Sample.event <- d[as.character(dat$Sample.event)]
    
    rm(x1, x2, d1, d2, d)
  }else{
    x <- dat %>%
      select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
             Date, time.inc) %>%
      distinct()
    x$Sample.event <- 1:nrow(x)
    dat <- suppressMessages(left_join(dat, x)) %>%
      relocate(Sample.event, .after = Ship.Cruise) %>%
      select(-Station.ID.Original, -time.inc, -no.time)
    rm(x)
  }
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n----------------------------\n',
          'Finished OB (1955-1957) data',
          '\n----------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # Operation HIGHJUMP ------------------------------------------------------
  
  Source <- 'Operation_HIGHJUMP'
  
  message('\n--------------------------------\n',
          'Cleaning Operation HIGHJUMP data',
          '\n--------------------------------')
  
  dat <- DATA[[Source]]
  
  #' Identify different data tables
  dat <- cbind(Data.Table = 1, dat)
  
  dat <- dat[,!names(dat) %in% c('', ' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN', 'Taxonomic.Modifier',
      'Life.Stage', 'Plankton.Staging.Code', 'Sex', 'Measurement.Type',
      'Water.Strained', 'Original.Value', 'Original.Units', 'Value.Per.Volume',
      'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.GMT
  x[x == 99.99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  
  dat$Time.GMT <- x
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.GMT[i]
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Time.GMT, -Date.Time.GMT, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time - converted from GMT'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(232, 'Nansen Bottle', dat$Gear)
  
  dat$Mesh.Size[grepl('bottle', dat$Gear, ignore.case = TRUE)] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  #' Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  dat$Water.Strained <- gsub('null', NA, dat$Water.Strained)
  
  dat$Occurrence <- removeWhiteSpacePadding(dat$Original.Value)
  dat$Original.Value <- as.numeric(dat$Occurrence)
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value == 0] <- 'absent'
  dat$Occurrence[!is.na(dat$Original.Value) & dat$Original.Value > 0] <- 'present'
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Value.Per.Volume <- removeWhiteSpacePadding(dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('n/a', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- as.numeric(dat$Value.Per.Volume)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Value.Per.Area <- removeWhiteSpacePadding(dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('n/a', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- as.numeric(dat$Value.Per.Area)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('c3-6','c4-6')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- i[!is.na(i)]
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('eggs','larva','nauplii','post-calyptopis')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('7252', 'USS Cacapon', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('468', 'NAVYLAB', dat$Project.ID)
  
  dat$Institute <- gsub('393',
                        'Scripps Institution of Oceanography (La Jolla - CA)',
                        dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers # empty
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  #' No times to delineate sample events. Only one gear used -- bottles, that may
  #' or may sample multiple depths. Depth cannot be used to assess event.
  
  dat$Time[dat$no.time] <- '12:00:00'
  
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  #' Reformat sample event
  x <- dat %>%
    select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
           Date, time.inc) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .after = Ship.Cruise) %>%
    select(-Station.ID.Original, -time.inc, -no.time)
  
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n--------------------------------\n',
          'Finished Operation HIGHJUMP data',
          '\n--------------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  
  # Professor Siedlecki -----------------------------------------------------
  
  Source <- 'Professor_Siedlecki'
  
  message('\n---------------------------------\n',
          'Cleaning Professor Siedlecki data',
          '\n---------------------------------')
  
  dat <- DATA[[Source]]
  
  #' Identify different data tables
  dat <- cbind(Data.Table = 1, dat)
  
  dat <- dat[,!names(dat) %in% c('',' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN', 'Taxonomic.Modifier',
      'Life.Stage', 'Plankton.Staging.Code', 'Sex', 'Measurement.Type',
      'Water.Strained', 'Original.Value', 'Original.Units', 'Value.Per.Volume',
      'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.GMT
  x[x == 99.99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  
  dat$Time.GMT <- x
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  x <- strsplit(dat$Date.Time, ' ')
  dat$Date <- sapply(x, function(z) z[1])
  dat$Time <- sapply(x, function(z) z[2])
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.GMT[i]
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Time.GMT, -Date.Time.GMT, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time - converted from GMT'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(118, 'Bongo Net', dat$Gear)
  
  dat$Mesh.Size[grepl('bottle', dat$Gear, ignore.case = TRUE)] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  # Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('-----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('c3-6','c4-6')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- i[!is.na(i)]
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('juvenile')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub('5605', 'Professor Siedlecki', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('328',
                         'Second International Biomass EXperiment',
                         dat$Project.ID)
  
  dat$Institute <- gsub('1196',
                        'Institute of Oceanography - Gdansk University',
                        dat$Institute)
  dat$Institute <- gsub('750',
                        'Sea Fisheries Institute (Gdynia - Poland)',
                        dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers # duplication of Scientific.Name.Plus.Modifiers
  
  dat <- dat %>% distinct()
  
  #' Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  #' Times not recorded. Only bongo nets used, so depth can be used to help
  #' delineate event.
  
  dat$Time[dat$no.time] <- '12:00:00'
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  x <- dat %>%
    select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original, Date,
           time.inc, Depth.bottom, Depth.top) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .after = Ship.Cruise) %>%
    select(-Station.ID.Original, -time.inc, -no.time)
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n---------------------------------\n',
          'Finished Professor Siedlecki data',
          '\n---------------------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  
  # YugNIRO -----------------------------------------------------------------
  
  Source <- 'YugNIRO'
  
  message('\n---------------------\n',
          'Cleaning YugNIRO data',
          '\n---------------------')
  
  dat <- DATA[[Source]]
  
  message('\n', "Combining data tables")
  
  #' Identify different data tables
  dat <- lapply(1:length(dat), function(z) cbind(Data.Table = z, dat[[z]]))
  
  #' Combine data tables
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  dat <- dat[,!names(dat) %in% c('', ' ')]
  
  #' Rename columns
  col.rename <- data.frame(
    original = names(dat),
    new = c(
      'Data.Table', 'Ship.Cruise', 'Year', 'Month', 'Day', 'Time.GMT', 'Time.LOC',
      'Latitude', 'Longitude', 'Depth.top', 'Depth.bottom', 'Tow.Orientation',
      'Gear', 'Mesh.Size', 'Plankton.Grouping.Code', 'ITIS.TSN', 'Taxonomic.Modifier',
      'Life.Stage', 'Plankton.Staging.Code', 'Sex', 'Measurement.Type',
      'Water.Strained', 'Original.Value', 'Original.Units', 'Value.Per.Volume',
      'Unit.Value.Per.Volume', 'Flag.Global.Annual.Range.Per.Vol',
      'Flag.Basin.Annual.Range.Per.Vol', 'Flag.Basin.Seasoinal.Range.Per.Vol',
      'Flag.Basin.Monthly.Range.Per.Vol', 'Value.Per.Area', 'Units.Value.Per.Area',
      'Flag.Global.Annual.Range.Per.Area', 'Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Area', 'Flag.Basin.Monthly.Range.Per.Area',
      'Scientific.Name.Plus.Modifiers', 'Record.ID', 'Dataset.ID', 'Ship',
      'Project.ID', 'Institute', 'Cruise.ID.Original', 'Station.ID.Original',
      'Taxa', 'Taxa.Modifiers')
  )
  
  names(dat) <- col.rename$new
  
  dat <- omitEmptyColumns(dat)
  dat <- omitEmptyRows(dat)
  
  #' Clean columns
  x <- dat$Time.GMT
  x[x == 99.99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  dat$Time.GMT <- x
  x <- dat$Time.LOC
  x[x == -99] <- NA
  j <- !is.na(x)
  hr <- as.character(floor(x[j]))
  i <- nchar(hr) == 1
  hr[i] <- paste0(0, hr[i])
  min <- floor(60 * {x[j] %% 1})
  i <- nchar(min) == 1
  min[i] <- paste0(0, min[i])
  x[j] <- paste(hr, min, '00', sep = ':')
  dat$Time.LOC <- x
  dat$Date.GMT <- as.character(as.Date(paste(dat$Day, dat$Month, dat$Year, sep = '-'), '%d-%m-%Y'))
  dat$Date.LOC <- dat$Date.GMT
  dat$Date.Time.GMT <- strptime(paste(dat$Date.GMT, dat$Time.GMT), format = '%Y-%m-%d %H:%M:%S')
  dat$Date.Time.LOC <- as.character(strptime(paste(dat$Date.LOC, dat$Time.LOC), format = '%Y-%m-%d %H:%M:%S'))
  #' Roughly convert from GMT to local time
  GMT2local <- function(date.time.GMT, lon){
    i <- lon > 180
    lon[i] <- -{360-lon[i]}
    i <- lon < 0
    lon[i] <- lon[i] %% -360
    i <- lon > 0
    lon[i] <- lon[i] %% 360
    sec.shift <- round(60^2*lon / 15)
    return(as.character(date.time.GMT + sec.shift))}
  dat$Date.Time <- GMT2local(dat$Date.Time.GMT, dat$Longitude)
  i <- !is.na(dat$Date.Time.LOC)
  dat$Date.Time[i] <- dat$Date.Time.LOC[i]
  dat$Date <- dat$Date.LOC
  i <- is.na(dat$Date)
  dat$Date[i] <- dat$Date.GMT[i]
  dat$Time <- dat$Time.LOC
  i <- is.na(dat$Time)
  dat$Time[i] <- ''
  dat$Date.Time[i] <- ''
  x <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(x, '%Y'))
  dat$Month <- as.numeric(format(x, '%m'))
  dat <- dat %>%
    relocate(Date, Time, Date.Time, .before = Year) %>%
    select(-Date.GMT, -Date.LOC, -Time.GMT, -Date.Time.GMT, -Date.Time.LOC, -Time.LOC, -Day)
  
  dat$Time.Flag <- 'Local time'
  dat$Time.Flag[is.na(dat$Time) | dat$Time == ''] <- ''
  
  dat$Tow.Orientation <- gsub('V', 'vertical', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('H', 'horizontal', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('O', 'oblique', dat$Tow.Orientation)
  dat$Tow.Orientation <- gsub('B', NA, dat$Tow.Orientation)
  
  dat$Gear <- gsub(217, 'Bogorov-Rass Net', dat$Gear)
  dat$Gear <- gsub(231, 'JUDAY PLANKTON NET SMALL MODEL(CODE 963)', dat$Gear)
  dat$Gear <- gsub(296, 'JUDAY SMALL', dat$Gear)
  
  dat$Mesh.Size[grepl('bottle', dat$Gear, ignore.case = TRUE)] <- NA
  i <- !is.na(dat$Mesh.Size)
  dat$Mesh.Size[i] <- paste(dat$Mesh.Size[i], paste0('\U03bc', 'm'))
  
  i <- dat$Taxonomic.Modifier != 0
  dat$Taxonomic.Modifier[i] <- COPEPOD.taxa.modifiers$Taxa.Modifier.Description[dat$Taxonomic.Modifier[i]]
  dat$Taxonomic.Modifier[!i] <- ''
  
  i <- dat$Life.Stage != 0
  dat$Life.Stage[i] <- COPEPOD.life.stage.codes$Life.Stage.Description[dat$Life.Stage[i]]
  dat$Life.Stage[!i] <- ''
  
  plankton.staging.code <- data.frame(
    code = 0:5,
    stage = c('unspecified',
              'adult or sub-adult',
              'juvenile or larvae',
              'nauplius-like',
              'eggs',
              'incomplete body fragments'))
  
  dat$Plankton.Stage <- plankton.staging.code$stage[dat$Plankton.Staging.Code+1]
  
  i <- dat$Sex != 0
  dat$Sex[i] <- COPEPOD.sex.codes$Taxa.Sex.Description[dat$Sex[i]]
  dat$Sex[!i] <- 'unspecified'
  dat$Sex <- tolower(dat$Sex)
  
  #' Use sex to specify copepodite stage 6 where possible
  dat$Copepodite.Stage <- NA
  i <- tolower(dat$Sex) %in% c('female','f','male','m')
  dat$Copepodite.Stage[i] <- 'c6'
  
  measurement.type.code <- data.frame(
    index = 1:4,
    code = c('c','r','b','t'),
    type = c('Number Count',
             'Relative Abundance code',
             'Total Net-haul Biomass Value',
             'Invidual Taxa biomass or biovolume')
  )
  
  for(i in 1:nrow(measurement.type.code)){
    dat$Measurement.Type <- gsub(measurement.type.code$code[i],
                                 measurement.type.code$index[i], dat$Measurement.Type)}
  dat$Measurement.Type <- measurement.type.code$type[as.integer(dat$Measurement.Type)]
  
  dat$Water.Strained <- removeWhiteSpacePadding(dat$Water.Strained)
  
  dat$Original.Units <- removeWhiteSpacePadding(dat$Original.Units)
  
  dat$Value.Per.Volume <- removeWhiteSpacePadding(dat$Value.Per.Volume)
  dat$Value.Per.Volume <- gsub('n/a', NA, dat$Value.Per.Volume)
  dat$Value.Per.Volume <- as.numeric(dat$Value.Per.Volume)
  
  dat$Unit.Value.Per.Volume <- removeWhiteSpacePadding(dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('----', NA, dat$Unit.Value.Per.Volume)
  dat$Unit.Value.Per.Volume <- gsub('-----', NA, dat$Unit.Value.Per.Volume)
  
  dat$Value.Per.Area <- removeWhiteSpacePadding(dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('null', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- gsub('n/a', NA, dat$Value.Per.Area)
  dat$Value.Per.Area <- as.numeric(dat$Value.Per.Area)
  
  dat$Units.Value.Per.Area <- removeWhiteSpacePadding(dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('----', NA, dat$Units.Value.Per.Area)
  dat$Units.Value.Per.Area <- gsub('-----', NA, dat$Units.Value.Per.Area)
  
  x <- strsplit(dat$Scientific.Name.Plus.Modifiers, ' -\\[ ')
  scientific.name <- sapply(x, function(z) z[1])
  scientific.name[scientific.name == '-[ ]-'] <- NA
  scientific.name.modifier <- sapply(x, function(z) z[2])
  scientific.name.modifier <- gsub('\\]-', '', scientific.name.modifier)
  scientific.name.modifier <- gsub('\\]', '', scientific.name.modifier)
  scientific.name.modifier <- removeWhiteSpacePadding(scientific.name.modifier)
  scientific.name.modifier[is.na(scientific.name.modifier)] <- ''
  dat$Scientific.Name <- scientific.name
  
  #' Use name modifiers to infill missing data if possible
  scientific.name.modifier <- strsplit(scientific.name.modifier, ';')
  scientific.name.modifier <- lapply(scientific.name.modifier, function(z) removeWhiteSpacePadding(z))
  
  #' Sex
  i <- sapply(scientific.name.modifier, function(z) any(grepl('female', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'female'
  i <- !i & sapply(scientific.name.modifier, function(z) any(grepl('male', z, ignore.case = TRUE)))
  dat$Sex[i & dat$Sex == 'unspecified'] <- 'male'
  
  #' Copepodite stage
  i <- sapply(scientific.name.modifier, function(z){
    j <- paste0('c', 1:6)
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Copepodite.Stage[!is.na(i)] <- i[!is.na(i)]
  dat$Copepodite.Stage <- toupper(dat$Copepodite.Stage)
  
  #' Other planktonic stages
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('adult', 'copepodite (unspecified)', 'eggs', 'nauplii')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Plankton.Stage[!is.na(i)] <- i[!is.na(i)]
  
  #' Morphology
  dat$Morphology <- NA
  i <- sapply(scientific.name.modifier, function(z){
    j <- c('bacillus/rod-shaped','double cone','spherical/coccoid','truncated-conical')
    x <- j %in% z
    if(any(x)) return(j[x]) else return(NA)})
  dat$Morphology[!is.na(i)] <- i[!is.na(i)]
  
  #' Dimensions
  dat$Length <- sapply(scientific.name.modifier, function(z){
    j <- grepl('length', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('length=','',gsub(' ','',z[j])))})
  dat$Width <- sapply(scientific.name.modifier, function(z){
    j <- grepl('width', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('width=','',gsub(' ','',z[j])))})
  dat$Radius <- sapply(scientific.name.modifier, function(z){
    j <- grepl('radius', z, ignore.case = TRUE)
    if(!any(j)) return(NA) else return(gsub('radius=','',gsub(' ','',z[j])))})
  
  dat$Ship <- gsub(6293, 'Ariel', dat$Ship)
  dat$Ship <- gsub(6344, 'Fiolent', dat$Ship)
  dat$Ship <- gsub(6351, 'Chernomor', dat$Ship)
  dat$Ship <- gsub(6372, 'Skif', dat$Ship)
  dat$Ship <- gsub(6379, 'Chatyr-dag', dat$Ship)
  dat$Ship <- gsub(6394, 'Zvezda Azova', dat$Ship)
  dat$Ship <- gsub(6440, 'Lesnoye', dat$Ship)
  dat$Ship <- gsub(6459, 'Marlin', dat$Ship)
  dat$Ship <- gsub(6472, 'Nauka', dat$Ship)
  dat$Ship <- gsub(6580, 'Akademik Vernadskiy', dat$Ship)
  dat$Ship <- gsub(6594, 'V. Vorobyev', dat$Ship)
  dat$Ship <- gsub(6605, 'Zvezda Sevastopolya', dat$Ship)
  dat$Ship <- gsub(6721, 'MYS Litel', dat$Ship)
  dat$Ship <- gsub(6731, 'Ignat Pavlyuchenkov', dat$Ship)
  dat$Ship <- gsub(6931, 'Sevastopolskiy Rybak', dat$Ship)
  dat$Ship <- gsub(6944, 'Zvezda Kryma', dat$Ship)
  dat$Ship <- gsub(6948, 'Nikolay Reshetnyak', dat$Ship)
  dat$Ship <- gsub(6951, 'Dmitriy Stefanov', dat$Ship)
  dat$Ship <- gsub(7191, 'Zvezda Chernomorya', dat$Ship)
  
  dat$Project.Code <- removeWhiteSpacePadding(dat$Project.ID)
  dat$Project.ID <- dat$Project.Code
  dat$Project.ID <- gsub('500',
                         'General Fisheries Researches of the USSR',
                         dat$Project.ID)
  
  dat$Institute <- gsub('896', 'YugNIRO', dat$Institute)
  
  taxa.modifier <- dat$Taxa.Modifiers # duplication of Scientific.Name.Plus.Modifiers
  
  dat <- dat %>% distinct()
  
  # Melt data into long form, accounting for multiple measurement types/units
  dat <- melt(dat,
              measure.vars = c('Original.Value','Value.Per.Volume','Value.Per.Area'),
              variable.name = 'Measurement', value.name = 'Value')
  dat$Units <- NA
  i <- list(
    dat$Measurement == 'Original.Value',
    dat$Measurement == 'Value.Per.Volume',
    dat$Measurement == 'Value.Per.Area')
  dat$Units[i[[1]]] <- dat$Original.Units[i[[1]]]
  dat$Units[i[[2]]] <- dat$Unit.Value.Per.Volume[i[[2]]]
  dat$Units[i[[3]]] <- dat$Units.Value.Per.Area[i[[3]]]
  dat <- dat %>% select(-c(Original.Units, Unit.Value.Per.Volume, Units.Value.Per.Area))
  i <- grep('measur', names(dat), ignore.case = TRUE)
  names(dat)[i] <- names(dat)[rev(i)]
  j <- is.na(dat$Value)
  if('Occurrence' %in% names(dat)) j <- j & is.na(dat$Occurrence)
  dat <- dat[!j,]
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove rows outside of selected latitudinal range
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- lat_lim[1] <= dat$Latitude & dat$Latitude <= lat_lim[2]
  dat <- dat[i,]
  
  #' Reformat sample event
  dat$no.time <- is.na(dat$Time) | dat$Time == ''
  
  dat$Time[dat$no.time] <- '12:00:00'
  dat <- dat[order(dat$Date, dat$Time, dat$Taxa, dat$Depth.bottom, dat$Depth.top),]
  
  dat$time.inc <- dat$Time
  dat$Time[dat$no.time] <- ''
  inc <- 30 #' time increment to discriminate events
  dat$time.inc <- sapply(strsplit(dat$time.inc, ':'), function(z, inc){
    x <- as.numeric(z)
    mins <- x[1]*60 + x[2] + x[3]/60
    return(round(mins / inc))
  }, inc = inc)
  
  x <- dat %>%
    select(Ship.Cruise, Gear, Ship, Cruise.ID.Original, Station.ID.Original,
           Date, time.inc) %>%
    distinct()
  x$Sample.event <- 1:nrow(x)
  dat <- suppressMessages(left_join(dat, x)) %>%
    relocate(Sample.event, .after = Ship.Cruise) %>%
    select(-Station.ID.Original, -time.inc, -no.time)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Update the main data frame
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~
  DATA[[Source]] <- dat
  
  message('\n---------------------\n',
          'Finished YugNIRO data',
          '\n---------------------')
  
  #' Clean up
  rm.vars <- ls()[!ls() %in% keep.vars]
  rm(list = rm.vars)
  gc()
  
  # Save cleaned data -------------------------------------------------------
  
  if(save.cleaned.data){
    for(Source in names(DATA)){
      m <- paste('Saving', Source, 'data')
      message('\n', m)
      d <- as.data.frame(DATA[[Source]]) #' extract data from main list
      f <- data.file.names[[Source]] # create file name for cleaned data
      if(length(f) == 1){
        f <- strsplit(f, '\\.')[[1]]
        f <- paste(paste(f[1], 'cleaned', sep = '_'), 'csv.gz', sep = '.')
      }else{
        f <- 'all_data_tables_cleaned.csv.gz'
      }
      p <- dir.data.all[[Source]] #' directory path
      p <- file.path(p, f) #' full path
      write.csv(d, gzfile(p), row.names = FALSE) #' save cleaned data
    }
  }else{
    message("Cleaned data were not saved to disk, so are retained in the workspace in the list 'DATA'.")
    clear.on.completeion <- FALSE
  }
  
  if(!clear.on.completeion) assign('DATA', DATA, envir = .GlobalEnv)
  
  rm(list = c('clean.data'), envir = .GlobalEnv)
  
  return(invisible(NULL))
}
