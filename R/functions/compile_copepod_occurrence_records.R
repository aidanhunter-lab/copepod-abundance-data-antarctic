#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Load the cleaned data, compile them into a single, consistently formatted
#' table, remove duplicate rows, then save the output.
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

compile.data <- function(species.selection = NULL,
                         round.lat.lon = TRUE, #' Should coordinates be rounded?
                         lat.lon.dp = 4, #' If rounded, choose number of decimal places
                         save.compiled.data = TRUE,
                         returnPlots = TRUE, #' output plots in list
                         savePlots = FALSE){
  
  # Load packages -----------------------------------------------------------
  library(data.table)
  library(dplyr)

  # Set directories ---------------------------------------------------------
  # setwd(this.dir())
  dir.root <- dirname(getwd())
  dir.data.base <- paste(dir.root, 'data', sep = '/')
  dir.data.zoo <- paste(dir.data.base, 'zooplankton', sep = '/')
  dir.functions <- paste(getwd(), 'functions', sep = '/')
  # dir.temp <- paste(getwd(), 'temp', sep = '/')
  # dir.map <- paste(dir.data.base, 'map files', 'Natural Earth', sep = '/')
  
  data.sources <- list.dirs(dir.data.zoo, recursive = FALSE, full.names = FALSE)
  data.sources <- data.sources[!grepl('compiled', data.sources)]
  
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
    if(!dir.data.all[[i]] %in% list.dirs(dir.root, recursive = TRUE)){
      stop(paste('The directory specified for', names(dir.data.all)[i], 
                 'data is not in project root directory! Check directory names.'))}}
  
  copepod.species.list <- file.path(dir.data.base, 'misc', 'copepod_species_list.txt')
  
  # Source functions --------------------------------------------------------
  # Load all .R files from 'functions' directory into global environment
  
  omit.funs <- c('clean_copepod_occurrence_records.R',
                 'compile_copepod_occurrence_records.R',
                 'plot_copepod_occurrence_records.R')
  R_functions <- list.files(dir.functions, pattern = "*.R$", ignore.case = TRUE)
  R_functions <- R_functions[!R_functions %in% omit.funs]
  get.functions <- function(dir, n, e){
    invisible(sapply(paste(dir, n, sep = '/'), source, local = e))}
  env <- environment(get.functions)
  get.functions(dir.functions, R_functions, env)

  # Load cleaned copepod occurrence records ---------------------------------
  
  message(
    '\n----------------------\n',
    'Load cleaned data sets',
    '\n----------------------'
  )
  
  all.data.file.names <- lapply(dir.data.all, function(z){
    f <- list.files(z)
    f[grepl('cleaned', f)]})
  
  data.file.names <- setNames(vector('list', length = n.data.sources),
                              names(dir.data.all))
  
  all.data.sources <- names(all.data.file.names)
  
  for(i in 1:n.data.sources){
    s <- all.data.sources[i]
    f <- all.data.file.names[[s]]
    if(length(f) == 1) data.file.names[[s]] <- f else{
      ff <- NA
      while(is.na(ff)){
        preprint <- data.frame(number = 1:length(f), data.file = f)
        Prompt <- 'Enter a number to choose a data file: '
        p <- prompt.user.input(Prompt, preprint)
        ff <- f[as.numeric(p)]
        data.file.names[[s]] <- ff
        if(is.na(ff)) cat('\n', 'Invalid number entered. Choose one of the listed options.', '\n')
      }}}
  
  #' Load cleaned data into a list
  for(i in 1:n.data.sources){
    message('\nLoading ', names(data.file.names)[i], ' data file: ', data.file.names[[i]])
    Dir <- dir.data.all[[i]]
    file.name <- data.file.names[[i]]
    file.path <- paste(Dir, file.name, sep = '/')
    assign(all.data.sources[i], read.csv(gzfile(file.path)))
    rm(Dir, file.name, file.path)
  }
  
  dat <- setNames(lapply(all.data.sources, function(z){
    get(z)
  }), all.data.sources)
  rm(list = all.data.sources)
  
  copepod.species.list <- scan(copepod.species.list, character(), sep = '\n',
                               quiet = TRUE)
  
  
  # Combine data sets -------------------------------------------------------
  
  message(
    '\n-------------------------\n',
    'Combine cleaned data sets',
    '\n-------------------------'
  )
  
  Sources <- names(dat)
  
  dat <- setNames(lapply(Sources, function(z)
    cbind(data.frame(Data.Source = rep(z, nrow(dat[[z]]))), dat[[z]])), Sources)
  
  #' Remove empty columns
  message('\n', 'Removing unwanted columns')
  
  empty.columns <- lapply(dat, function(z) sapply(1:ncol(z), function(w) all(is.na(z[,w]) | z[,w] == '')))
  for(i in names(dat)) dat[[i]] <- dat[[i]][,!empty.columns[[i]]]
  
  #' Remove unwanted columns
  all.fields <- sort(unique(unlist(lapply(dat, function(z) names(z)))))
  
  unwanted.fields <- 
    c('acceptedNameUsageID','acceptedTaxonKey','accessRights','aphiaID',
      'basisOfRecord','catalogNumber','class','classKey','collectionID',
      'Comments','continent','coordinatePrecision',
      'coordinateUncertaintyInMeters','countryCode','CruiseTow','CTNID',
      'datasetKey','disposition',
      'familyKey',
      'fieldNotes','Flag.Basin.Annual.Range.Per.Area',
      'Flag.Basin.Annual.Range.Per.Vol','Flag.Basin.Monthly.Range.Per.Area',
      'Flag.Basin.Monthly.Range.Per.Vol','Flag.Basin.Seasoinal.Range.Per.Area',
      'Flag.Basin.Seasoinal.Range.Per.Vol','Flag.Global.Annual.Range.Per.Area',
      'Flag.Global.Annual.Range.Per.Vol','footprintSRS','footprintWKT','gbifID',
      'genusKey','georeferenceProtocol','georeferenceRemarks',
      'georeferenceSources','georeferenceVerificationStatus','Grid.Station',
      'GridLine','GridStation','habitat','hasCoordinate','hasGeospatialIssues',
      'Heading_degrees','higherClassification','higherGeography',
      'identificationRemarks','identificationVerificationStatus','identifiedBy',
      'identifiedByID','institutionID','islandGroup','issue','ITIS.TSN',
      'iucnRedListCategory','kingdom','kingdomKey','language','lastCrawled',
      'lastInterpreted','lastParsed','Length','level0Gid','level0Name',
      'level1Gid','level1Name','level2Gid','level2Name','license','Location',
      'marine','materialSampleID','mediaType','modified','Morphology','NetID',
      'nomenclaturalCode','Occurence.id','occurrenceID','order','orderKey',
      'otherCatalogNumbers','ownerInstitutionCode','parentEventID','phylum',
      'phylumKey','Plankton.Grouping.Code','Plankton.Staging.Code','preparations',
      'Project.Code','projectId','protocol','publishingCountry','Radius',
      'recordedBy','recordedByID','recordNumber','repatriated','rightsHolder',
      'Sample',
      'Scientific.Name.Plus.Modifiers','scientificName',
      'scientificNameID','Scientist','Ship.Cruise','SOG','speciesKey',
      'stateProvince','Taxa.Modifiers','taxonConceptID','taxonID','taxonKey',
      'Taxonomic.Modifier','taxonomicStatus','taxonRank','taxonRemarks','Tow',
      'TowType','type','typeStatus','verbatimDepth','verbatimLocality',
      'waterBody','Width'
    )
  
  for(i in names(dat)) dat[[i]] <- dat[[i]][,!{names(dat[[i]]) %in% unwanted.fields}]
  
  all.fields <- sort(unique(unlist(lapply(dat, function(z) names(z)))))
  
  #' Remove any empty rows
  for(i in names(dat)) dat[[i]] <- dat[[i]][!apply(is.na(dat[[i]]), 1, 'all'),]
  
  message('\n', 'Merging all data')
  dat <- as.data.frame(rbindlist(dat, fill = TRUE))
  
  #' There's lots of redundancy in this data frame due to non-unique column names
  #' and excessive NA values, so the data requires lots of RAM. The data size
  # will reduce as it's regularised.
  data.size.orig <- object.size(dat)
  data.size.orig <- format(data.size.orig, 'Mb')
  
  message('\n', 'Combined data size = ', paste0(data.size.orig,'.'))
  
  data.size <- format(object.size(dat),'Mb')
  message('\n', 'Trimmed data size = ', paste0(data.size,'.'))
  
  dat <- as.list(dat) #' work with a list rather than data frame
  
  # Regularise fields -------------------------------------------------------
  
  #' Regularise column names between the various combined data sets.
  message('\n', 'Regularise column names and values...')
  
  # Species
  message('\n', '-- Species')
  
  i <- grep('species', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # any(!is.na(dat$species) & {dat$species == '' | dat$species == ' '})
  j <- !is.na(dat$Species) & is.na(dat$species)
  dat$species[j] <- dat$Species[j]
  # print(paste(sum(is.na(dat$species)), 'rows remaining'))
  
  i <- grep('tax', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  j <- !is.na(dat$Taxon.name) & is.na(dat$species)
  dat$species[j] <- dat$Taxon.name[j]
  # print(paste(sum(is.na(dat$species)), 'rows remaining'))
  
  j <- !is.na(dat$Taxa) & is.na(dat$species)
  dat$species[j] <- dat$Taxa[j]
  # print(paste(sum(is.na(dat$species)), 'rows remaining'))
  
  #' There are other columns containing species names, but these are redundant.
  i <- grep('name', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # unique(dat[!is.na(dat$Scientific.Name),c('species','Scientific.Name')])
  # j <- !is.na(dat$Scientific.Name) & is.na(dat$species)
  # j <- !is.na(dat$genericName) & is.na(dat$species)
  
  #' Remove the redundant species names columns
  omit <- c('Species', 'Taxa', 'Taxon.name', 'Scientific.Name', 'genericName', 
            'originalNameUsage', 'vernacularName', 'acceptedScientificName',
            'verbatimScientificName', 'specificEpithet')
  dat[omit] <- NULL
  
  names(dat)[names(dat) == 'species'] <- 'Species'
  
  #' Species name modifiers (genus or unwanted addemdums) -- some of these columns
  #' are redundant as the potentially useful information has been extracted already.
  omit <- c('genus', 'family')
  dat[omit] <- NULL
  
  #' Regularise the species names as far as possible
  
  # sort(unique(dat$Species)); print(paste(length(unique(dat$Species)), 'species names'))
  nc <- nchar(dat$Species)
  j <- substr(dat$Species, nc - 2, nc) == '.sp'
  dat$Species[j] <- paste(substr(dat$Species[j], 1, nc[j] - 3), 'sp')
  j <- substr(dat$Species, nc - 3, nc) %in% c('.spA', '.spB')
  dat$Species[j] <- paste(substr(dat$Species[j], 1, nc[j] - 4), 'sp')
  dat$Species <- gsub('\\.sp_', ' sp_', dat$Species)
  
  # j <- grepl('sp\\.', dat$Species)
  # sort(unique(dat$Species[j]))
  dat$Species <- gsub('sp\\. 1', 'sp', dat$Species)
  dat$Species <- gsub('sp\\. 2', 'sp', dat$Species)
  # x <- paste('sp.', c('A - Z (all)', LETTERS[1:13]))
  x <- paste('sp.', c('A - Z (all)', LETTERS))
  for(i in 1:length(x)){
    N <- nchar(dat$Species)
    n <- nchar(x[i])
    j <- substr(dat$Species, N - n + 1, N) == x[i]
    dat$Species[j] <- substr(dat$Species[j], 1, N[j] - n + 2)
  }; rm(x,N,n); invisible(gc())
  
  j <- grepl('sp\\.', dat$Species)
  # sort(unique(dat$Species[j]))
  dat$Species[j] <- gsub('sp\\.', 'sp', dat$Species[j])
  j <- grepl('spp\\.', dat$Species)
  # sort(unique(dat$Species[j]))
  dat$Species[j] <- gsub('spp\\.', 'spp', dat$Species[j])
  j <- grepl('\\.Sp', dat$Species)
  # sort(unique(dat$Species[j]))
  dat$Species[j] <- gsub('\\.Sp', '\\.sp', dat$Species[j])
  
  j <- grepl('\\.', dat$Species)
  # sort(unique(dat$Species[j]))
  dat$Species[j] <- gsub('\\.', ' ', dat$Species[j])
  j <- substr(dat$Species, 2, 2) == ' '
  n <- nchar(dat$Species)
  dat$Species[j] <- paste(substr(dat$Species[j], 1, 1), substr(dat$Species[j], 3, n[j]), sep = '. ')
  
  n <- nchar(dat$Species)
  j <- substr(dat$Species, n, n) == ' ' #' remove trailing spaces
  while(any(j)){
    dat$Species[j] <- substr(dat$Species[j], 1, n[j]-1)
    n <- nchar(dat$Species)
    j <- substr(dat$Species, n, n) == ' '
  }; rm(j,n); invisible(gc())
  # sort(unique(dat$Species)); print(paste(length(unique(dat$Species)), 'species names'))
  j <- grepl('  ', dat$Species) #' remove double spaces
  while(any(j)){
    dat$Species[j] <- gsub('  ', ' ', dat$Species[j])
    j <- grepl('  ', dat$Species)
  }
  
  i <- grepl(' and ', dat$Species)
  dat$Species[i] <- gsub(' and ', '_', dat$Species[i])
  i <- grepl('\\/', dat$Species)
  dat$Species[i] <- gsub(' \\/ ', '_', dat$Species[i])
  
  #' Include suffix 'spp' for Species entries listed as genus only.
  x <- strsplit(dat$Species, '_') # split entries with multiple names
  x1 <- sapply(x, function(z) z[1])
  x2 <- sapply(x, function(z) z[2])
  i <- !grepl(' ', x1)
  x1[i] <- paste(x1[i], 'spp')
  i <- !is.na(x2) & !grepl(' ', x2)
  x2[i] <- paste(x2[i], 'spp')
  i <- !is.na(x2)
  x <- x1
  x[i] <- paste(x[i], x2[i], sep = '_')
  dat$Species <- x
  rm(x,x1,x2)
  
  #' Select species
  if(is.null(species.selection)){
    print(sort(unique(dat$Species)))
    message('\n', 'Above are all species listed in data')
    Prompt <- 'Enter a single species name (as listed here) to select data for that species, or select multiple species by separating with underscores "_", or to select all species type "ALL" then enter: \n'
    p <- prompt.user.input(Prompt)
    multipleSpecies <- FALSE
    if(p == 'ALL' | p == 'all'){
      multipleSpecies <- TRUE
      species.selection <- 'all'
    }else{
      if(grepl('_', p)){
        multipleSpecies <- TRUE
        species.selection <- strsplit(p, '_')[[1]]
        if(!all(species.selection %in% dat$Species)) warning('One or more selected species does not appear in data exactly as typed -- perhaps a typo?')
      }else{
        if(!p %in% dat$Species){
          warning('Selected species does not appear in data exactly as typed -- perhaps a typo?')
        }else{
          species.selection <- p
        }
      }
    }
  }else{
    multipleSpecies <- FALSE
    is.all <- any(species.selection %in% c('all','All','ALL'))
    is.all.copepods <- any(species.selection %in% c('copepods','Copepods','COPEPODS'))
    if(is.all | is.all.copepods){
      multipleSpecies <- TRUE
      if(is.all) species.selection <- 'all'
      if(is.all.copepods) species.selection <- 'copepods'
    }else{
      if(grepl('_', species.selection)){
        multipleSpecies <- TRUE
        species.selection <- strsplit(species.selection, '_')[[1]]
        if(!all(species.selection %in% dat$Species)) warning('One or more selected species does not appear in data exactly as typed -- perhaps a typo?')
      }else{
        if(!species.selection %in% dat$Species) warning('Selected species does not appear in data exactly as typed -- perhaps a typo?')
      }
    }
  }
  
  #' If a single species has been selected then omit all other species.
  if(all(species.selection != 'all') & all(species.selection != 'copepods')){
    i <- rep(FALSE, length(dat$Species))
    nspecies <- length(species.selection)
    for(x in 1:nspecies){
      s <- species.selection[x]
      i <- i | grepl(s, dat$Species, ignore.case = TRUE)
      j <- strsplit(s, ' ')[[1]]
      nj <- length(j)
      if(nj > 1){
        t1 <- paste(substr(j[1], 1, 1), paste(j[2:nj]))
        t2 <- paste(paste0(substr(j[1], 1, 1), '.'), paste(j[2:nj]))
        i <- i | grepl(t1, dat$Species, ignore.case = TRUE) | 
          grepl(t2, dat$Species, ignore.case = TRUE)
      }
    }
    dat <- lapply(dat, function(z) z[i])
    rm(s,i,j,nj,t1,t2); invisible(gc())
  }
  
  if(all(species.selection == 'copepods')){
    #' Identify species (genus) names in data matching names in copepod species list
    i_ <- unique(dat$Species)
    ia <- i_[substr(i_, 2, 2) == '.'] #' all data names given with abbreviated genus are copepods
    i_ <- i_[!i_ %in% ia]
    i <- gsub(' \\& ', ' ', gsub(' and ', ' ', gsub(' or ', ' ', gsub(' sp', '', gsub(' spp', '', gsub('\\,', '', gsub('\\)', '', gsub('\\(', '', i_))))))))
    i <- lapply(i, function(z) strsplit(z, ' ')[[1]]) #' split data names into a space-separated list
    j <- unique(copepod.species.list) #' tidy the copepod species list
    j <- gsub(' \\.', '\\.', gsub('\" ', '', gsub('\\,', '', gsub('=', '', gsub('\\?', '', gsub('\\)', '', gsub('\\(', '', j)))))))
    k <- grepl('  ', j)
    while(any(k)){
      j <- gsub('  ', ' ', j)
      k <- grepl('  ', j)}
    j <- lapply(j, function(z) strsplit(z, ' ')[[1]]) #' split into space-separated list
    #' Compare names in data to entries copepod species list
    y <- setNames(lapply(i, function(z){
      y <- setNames(lapply(j, function(w){
        x <- vgrepl(z, w, ignore.case = TRUE)
        x <- matrix(x, length(w), length(z), dimnames = list(NULL, z))
        x <- apply(x, 2, any)
        if(all(!x)) return(NULL) else return(x)}),
        unlist(lapply(j, function(w) paste(w, collapse = ' '))))
      y <- y[sapply(y, function(z) !is.null(z))]
      return(y)}),
      unlist(lapply(i, function(z) paste(z, collapse = ' '))))
    #' Identify the copepods
    y <- sapply(y, function(z) length(z) > 0)
    #' Filter the data
    i <- c(ia, i_[y])
    j <- dat$Species %in% i
    dat <- lapply(dat, function(z) z[j])
    rm(i_,i,j,k,y); invisible(gc())
  }
  
  
  #' Regularise lower/upper case
  x <- strsplit(dat$Species, '_')
  x1 <- sapply(x, function(z) z[1])
  x2 <- sapply(x, function(z) z[2])
  x1 <- paste0(toupper(substr(x1, 1, 1)), tolower(substr(x1, 2, nchar(x1))))
  i <- !is.na(x2)
  x2[i] <- paste0(toupper(substr(x2[i], 1, 1)), tolower(substr(x2[i], 2, nchar(x2[i]))))
  x <- x1
  x[i] <- paste(x[i], x2[i], sep = '_')
  dat$Species <- x
  rm(x, x1, x2)
  
  dat$Species <- gsub('_', ' and ', dat$Species)
  
  
  #' Finally, manually examine Species and make any obvious corrections...
  #' Also check the listed names against the accepted taxonomic register to ensure
  #' the accepted names are used: @https://www.marinespecies.org/copepoda/aphia.php?p=search
  
  # print(sort(unique(dat$Species)))
  # length(unique(dat$Species))
  
  #' `A. australis` is ambiguous (Acartia (Odontacartia) australis, Aetideus australis)
  #' `C. bradyi` is ambiguous (Candacia bradyi, Centropages bradyi, Cervinia bradyi)
  #' `C. gracilis` is ambiguous (Centropages gracilis, Chiridius gracilis)
  #' `C. robustus` is ambiguous (Cornucalanus robustus, Corycaeus (Monocorycaeus) robustus)
  #' `H. major` is ambiguous (Haloptilus major, Heterostylites major)
  #' `M. princeps` is ambiguous (Metridia princeps, Megacalanus princeps)
  #' `P. antarctica` is ambiguous (Paraeuchaeta antarctica, Paralabidocera antarctica, Pleuromamma antarctica)
  #' `S. antarcticus` is ambiguous (Scaphocalanus antarcticus, Spinocalanus antarcticus, Stephos antarcticus)
  #' `S. magnus` is ambiguous (Scaphocalanus magnus, Scolecithrix magnus, Spinocalanus magnus)
  
  i <- dat$Species == 'A. antarctica'
  dat$Species[i] <- 'Aetideopsis antarctica'
  i <- dat$Species == 'A. arcuatus'
  dat$Species[i] <- 'Aetideus arcuatus'
  i <- dat$Species == 'A. dentipes'
  dat$Species[i] <- 'Amallothrix dentipes'
  i <- dat$Species == 'A. dentipes and Scolecithricella dentipes'
  dat$Species[i] <- 'Amallothrix dentipes and Scolecithricella dentipes'
  i <- dat$Species == 'A. glacialis'
  dat$Species[i] <- 'Augaptilus glacialis'
  i <- dat$Species == 'A. minor'
  dat$Species[i] <- 'Aetideopsis minor'
  i <- dat$Species == 'A. rostrata'
  dat$Species[i] <- 'Aetideopsis rostrata'
  i <- dat$Species == 'A. rostrata and Aetideopsis inflata'
  dat$Species[i] <- 'Aetideopsis rostrata and Aetideopsis inflata'
  i <- dat$Species == 'A. tonsa'
  dat$Species[i] <- 'Acartia (Acanthacartia) tonsa'
  i <- dat$Species == 'Acartia (acartia) negligens'
  dat$Species[i] <- 'Acartia (Acartia) negligens'
  i <- dat$Species == 'Acartia (odontacartia) australis'
  dat$Species[i] <- 'Acartia (Odontacartia) australis'
  i <- dat$Species == 'Acartia australis'
  dat$Species[i] <- 'Acartia (Odontacartia) australis'
  i <- substr(dat$Species, nchar(dat$Species) - 3, nchar(dat$Species)) == ' cop'
  dat$Species[i] <- paste(substr(dat$Species[i], 1, nchar(dat$Species[i]) - 4), 'sp')
  i <- dat$Species == 'Aetideopsis antarctia'
  dat$Species[i] <- 'Aetideopsis antarctica'
  i <- dat$Species == 'C. acutus'
  dat$Species[i] <- 'Calanoides acutus'
  i <- dat$Species == 'C. antarctica'
  dat$Species[i] <- 'Cenognatha antarctica'
  i <- dat$Species == 'C. australis'
  dat$Species[i] <- 'Calanus australis'
  i <- dat$Species == 'C. brachiatus'
  dat$Species[i] <- 'Centropages brachiatus'
  i <- dat$Species == 'C. brevipes'
  dat$Species[i] <- 'Clausocalanus brevipes'
  i <- dat$Species == 'C. cheirura'
  dat$Species[i] <- 'Candacia cheirura'
  i <- dat$Species == 'C. citer'
  dat$Species[i] <- 'Ctenocalanus citer'
  i <- dat$Species == 'C. falcifera'
  dat$Species[i] <- 'Candacia falcifera'
  i <- dat$Species == 'C. frigidus'
  dat$Species[i] <- 'Cephalophanes frigidus'
  i <- dat$Species == 'C. ingens'
  dat$Species[i] <- 'Clausocalanus ingens'
  i <- dat$Species == 'C. laticeps'
  dat$Species[i] <- 'Clausocalanus laticeps'
  i <- dat$Species == 'C. maxima'
  dat$Species[i] <- 'Candacia maxima'
  i <- dat$Species == 'C. patagoniensis'
  dat$Species[i] <- 'Calanoides patagoniensis'
  i <- dat$Species == 'C. pavoninus'
  dat$Species[i] <- 'Calocalanus pavoninus'
  i <- dat$Species == 'C. polaris'
  dat$Species[i] <- 'Chiridius polaris'
  i <- dat$Species == 'C. polaris and Chiridius subantarcticus'
  dat$Species[i] <- 'Chiridius polaris and Chiridius subantarcticus'
  i <- dat$Species == 'C. propinquus'
  dat$Species[i] <- 'Calanus propinquus'
  i <- dat$Species == 'C. simillimus'
  dat$Species[i] <- 'Calanus simillimus'
  i <- dat$Species == 'Cal sp'
  dat$Species[i] <- 'Calanidae sp'
  i <- dat$Species == 'Calanoid sp'
  dat$Species[i] <- 'Calanoida sp'
  i <- dat$Species == 'Centropages bradyii'
  dat$Species[i] <- 'Centropages bradyi'
  i <- dat$Species == 'Clauso sp'
  dat$Species[i] <- 'Clausocalanus sp'
  i <- dat$Species %in% c('Clausocalanus breuipes', 'Clausocalanus breviceps')
  dat$Species[i] <- 'Clausocalanus brevipes'
  i <- dat$Species == 'Clausocalanus ingen'
  dat$Species[i] <- 'Clausocalanus ingens'
  i <- dat$Species == 'Cop sp'
  dat$Species[i] <- 'Copepoda sp'
  i <- dat$Species == 'Cyclopoid sp'
  dat$Species[i] <- 'Cyclopoida sp'
  i <- dat$Species == 'Delibus nudus'
  dat$Species[i] <- 'Delius nudus'
  i <- dat$Species == 'D. forcipatus'
  dat$Species[i] <- 'Drepanopus forcipatus'
  i <- !{dat$Species %in% c('Drepanopus forcipatus total', 'Drepanopus total')}
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'E. antarcticum'
  dat$Species[i] <- 'Ectinosoma antarcticum'
  i <- dat$Species == 'E. antarcticus'
  dat$Species[i] <- 'Euaugaptilus antarcticus'
  i <- dat$Species == 'E. antarcticus and Euaugaptilus laticeps'
  dat$Species[i] <- 'Euaugaptilus antarcticus and Euaugaptilus laticeps'
  i <- dat$Species == 'E. bullifer'
  dat$Species[i] <- 'Euaugaptilus bullifer'
  i <- dat$Species != 'E. friacontha'
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'E. hyalinus and Eucalanus elongatus'
  dat$Species[i] <- 'Eucalanus hyalinus and Eucalanus elongatus'
  i <- dat$Species == 'E. nodifrons'
  dat$Species[i] <- 'Euaugaptilus nodifrons'
  i <- dat$Species == 'E. rostromagna'
  dat$Species[i] <- 'Euchirella rostromagna'
  i <- dat$Species == 'Euchirella rostrata rostramagna'
  dat$Species[i] <- 'Euchirella rostrata and Euchirella rostramagna'
  i <- dat$Species == 'F. barbatula'
  dat$Species[i] <- 'Foxtonia barbatula'
  i <- dat$Species == 'F. frigida'
  dat$Species[i] <- 'Farrania frigida'
  i <- dat$Species != 'Fragilariopsis f bouvet'
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'G. brevispinus and Gaidius intermedius'
  dat$Species[i] <- 'Gaetanus brevispinus and Gaidius intermedius'
  i <- dat$Species == 'G. kruppii and Gaetanus antarcticus'
  dat$Species[i] <- 'Gaetanus kruppii and Gaetanus antarcticus'
  i <- dat$Species == 'G. tenuispinus'
  dat$Species[i] <- 'Gaetanus tenuispinus'
  i <- dat$Species == 'G. tenuispinus and Gaidius tenuispinus'
  dat$Species[i] <- 'Gaetanus tenuispinus'
  i <- dat$Species == 'G. brevispinus and Gaidius intermedius'
  dat$Species[i] <- 'Gaetanus brevispinus'
  i <- dat$Species == 'Gaidius affinis'
  dat$Species[i] <- 'Gaetanus affinis'
  i <- dat$Species == 'Gaidius tenuispinus'
  dat$Species[i] <- 'Gaetanus tenuispinus'
  i <- dat$Species == 'Gaidius sp'
  dat$Species[i] <- 'Gaetanus sp'
  i <- dat$Species == 'H. acutifrons'
  dat$Species[i] <- 'Haloptilus acutifrons'
  i <- dat$Species == 'H. austrinus'
  dat$Species[i] <- 'Heterorhabdus austrinus'
  i <- dat$Species == 'H. fons'
  dat$Species[i] <- 'Haloptilus fons'
  i <- dat$Species == 'H. furcifer'
  dat$Species[i] <- 'Harpacticus furcifer'
  i <- dat$Species == 'H. longicirrus'
  dat$Species[i] <- 'Haloptilus longicirrus'
  i <- dat$Species == 'H. longicornis'
  dat$Species[i] <- 'Haloptilus longicornis'
  i <- dat$Species == 'H. ocellatus'
  dat$Species[i] <- 'Haloptilus ocellatus'
  i <- dat$Species == 'H. oxycephalus'
  dat$Species[i] <- 'Haloptilus oxycephalus'
  i <- dat$Species == 'H. spinifrons'
  dat$Species[i] <- 'Heterorhabdus spinifrons'
  i <- dat$Species %in% c('Haloptilus ocelatus', 'Haloptilus ocellatuss')
  dat$Species[i] <- 'Haloptilus ocellatus'
  i <- substr(dat$Species, nchar(dat$Species) - 8, nchar(dat$Species)) == ' copepods'
  dat$Species[i] <- paste(substr(dat$Species[1], 1, nchar(dat$Species[1]) - 9), 'sp')
  i <- dat$Species %in% c('Harpacticoid sp', 'Harpacticoida sp', 'Harpacticoidspp')
  dat$Species[i] <- 'Harpacticoida sp'
  i <- dat$Species == 'Heterorhabdus autrinus'
  dat$Species[i] <- 'Heterorhabdus austrinus'
  i <- dat$Species == 'I. antarctica and Idomene antarctica'
  dat$Species[i] <- 'Isidicola antarctica and Idomenella antarctica'
  i <- dat$Species == 'L. antarcticus'
  dat$Species[i] <- 'Landrumius antarcticus'
  i <- dat$Species == 'L. clausi'
  dat$Species[i] <- 'Lucicutia clausi'
  i <- dat$Species == 'L. curta'
  dat$Species[i] <- 'Lucicutia curta'
  i <- dat$Species == 'L. macrocera'
  dat$Species[i] <- 'Lucicutia macrocera'
  i <- dat$Species == 'L. ovalis'
  dat$Species[i] <- 'Lucicutia gaussae'
  i <- dat$Species == 'L. parva'
  dat$Species[i] <- 'Lucicutia parva'
  i <- dat$Species == 'L. polaris'
  dat$Species[i] <- 'Lucicutia polaris'
  i <- dat$Species == 'L. rara'
  dat$Species[i] <- 'Lucicutia bradyana'
  i <- dat$Species == 'L. wolfendeni'
  dat$Species[i] <- 'Lucicutia wolfendeni'
  i <- dat$Species == 'Lucicutiid sp'
  dat$Species[i] <- 'Lucicutiidae sp'
  i <- dat$Species == 'M. alter'
  dat$Species[i] <- 'Mappates alter'
  i <- dat$Species == 'M. alter and Scolecithricella altera'
  dat$Species[i] <- 'Mappates alter and Scolecithricella altera'
  i <- dat$Species == 'M. cultrifer'
  dat$Species[i] <- 'Mimocalanus cultrifer'
  i <- dat$Species == 'M. curticauda'
  dat$Species[i] <- 'Metridia curticauda'
  i <- dat$Species == 'M. gerlachei'
  dat$Species[i] <- 'Metridia gerlachei'
  i <- dat$Species == 'M. lucens'
  dat$Species[i] <- 'Metridia lucens lucens'
  i <- dat$Species == 'M. nudus'
  dat$Species[i] <- 'Mimocalanus nudus'
  i <- dat$Species == 'M. schielae'
  dat$Species[i] <- 'Mospicalanus schielae'
  i <- dat$Species == 'Metridia lucens'
  dat$Species[i] <- 'Metridia lucens lucens'
  i <- dat$Species == 'N. tonsus'
  dat$Species[i] <- 'Neocalanus tonsus'
  i <- dat$Species == 'O. atlantica'
  dat$Species[i] <- 'Oithona atlantica'
  i <- dat$Species == 'O. curvata'
  dat$Species[i] <- 'Oncaea curvata'
  i <- dat$Species == 'O. damkaeri'
  dat$Species[i] <- 'Oncaea damkaeri'
  i <- dat$Species == 'O. englishi'
  dat$Species[i] <- 'Oncaea englishi'
  i <- dat$Species == 'O. frigida'
  dat$Species[i] <- 'Oithona frigida'
  i <- dat$Species == 'O. magnus'
  dat$Species[i] <- 'Onchocalanus magnus'
  i <- dat$Species == 'O. parila'
  dat$Species[i] <- 'Oncaea parila'
  i <- dat$Species == 'Oncaea venusta'
  dat$Species[i] <- 'Oncaea venusta venusta'
  i <- dat$Species == 'O. similis'
  dat$Species[i] <- 'Oithona similis'
  i <- dat$Species == 'O. trigoniceps'
  dat$Species[i] <- 'Onchocalanus trigoniceps'
  i <- dat$Species == 'O. wolfendeni'
  dat$Species[i] <- 'Onchocalanus wolfendeni'
  i <- dat$Species == 'Oithon sp'
  dat$Species[i] <- 'Oithona sp'
  i <- dat$Species == 'P. barbata'
  dat$Species[i] <- 'Paraeuchaeta barbata barbata'
  i <- dat$Species == 'P. barbata and Paraeuchaeta farrani'
  dat$Species[i] <- 'Paraeuchaeta barbata barbata and Paraeuchaeta farrani'
  i <- dat$Species == 'P. belgicae'
  dat$Species[i] <- 'Pseudocyclopina belgicae'
  i <- dat$Species == 'P. biloba'
  dat$Species[i] <- 'Paraeuchaeta biloba'
  i <- dat$Species == 'P. cenotelis'
  dat$Species[i] <- 'Pseudoamallothrix cenotelis'
  i <- dat$Species == 'P. cenotelis and Scolecithricella cenotelis'
  dat$Species[i] <- 'Pseudoamallothrix cenotelis and Scolecithricella cenotelis'
  i <- dat$Species == 'P. compactus'
  dat$Species[i] <- 'Pseudodiaptomus compactus'
  i <- dat$Species == 'P. emarginata and Scolecithricella emarginata'
  dat$Species[i] <- 'Pseudoamallothrix emarginata and Scolecithricella emarginata'
  i <- dat$Species == 'P. farrani'
  dat$Species[i] <- 'Paraheterorhabdus farrani'
  i <- dat$Species == 'P. farrani and Heterorhabdus farrani'
  dat$Species[i] <- 'Paraheterorhabdus farrani and Heterorhabdus farrani'
  i <- dat$Species == 'P. longiremis'
  dat$Species[i] <- 'Pseudaugaptilus longiremis'
  i <- dat$Species == 'P. mawsoni'
  dat$Species[i] <- 'Pseudochirella mawsoni'
  i <- dat$Species == 'P. pacificus'
  dat$Species[i] <- 'Pseudhaloptilus pacificus'
  i <- dat$Species == 'P. rasa'
  dat$Species[i] <- 'Paraeuchaeta rasa'
  i <- dat$Species == 'P. similis'
  dat$Species[i] <- 'Paraeuchaeta similis'
  i <- dat$Species == 'P. spectabilis and Pseudochirella elongata'
  dat$Species[i] <- 'Pseudochirella spectabilis and Pseudochirella elongata'
  i <- dat$Species == 'Paracalanus parvus'
  dat$Species[i] <- 'Paracalanus parvus parvus'
  i <- dat$Species == 'Pleuromamma robusta'
  dat$Species[i] <- 'Pleuromamma robusta robusta'
  i <- dat$Species == 'Pleuromamma robusta f antarctica'
  dat$Species[i] <- 'Pleuromamma robusta antarctica'
  i <- dat$Species == 'R. antarcticus'
  dat$Species[i] <- 'Racovitzanus antarcticus'
  i <- dat$Species == 'R. atlantica'
  dat$Species[i] <- 'Ratania atlantica'
  i <- dat$Species == 'R. gigas'
  dat$Species[i] <- 'Rhincalanus gigas'
  i <- dat$Species == 'R. nasutus'
  dat$Species[i] <- 'Rhincalanus nasutus'
  i <- dat$Species != 'Reptantia (order of decapoda)'
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'S. abyssalis'
  dat$Species[i] <- 'Spinocalanus abyssalis abyssalis'
  i <- dat$Species == 'S. brevicaudatus'
  dat$Species[i] <- 'Spinocalanus brevicaudatus'
  i <- dat$Species == 'S. dentata'
  dat$Species[i] <- 'Scolecithricella dentata'
  i <- dat$Species == 'S. farrani'
  dat$Species[i] <- 'Scaphocalanus farrani'
  i <- dat$Species == 'S. globulosa'
  dat$Species[i] <- 'Scolecithricella globulosa'
  i <- dat$Species == 'S. globulosa and Scolecithricella schizosoma'
  dat$Species[i] <- 'Scolecithricella globulosa and Scolecithricella schizosoma'
  i <- dat$Species == 'S. horridus'
  dat$Species[i] <- 'Spinocalanus horridus'
  i <- dat$Species == 'S. longiceps'
  dat$Species[i] <- 'Subeucalanus longiceps'
  i <- dat$Species == 'S. longicornis'
  dat$Species[i] <- 'Spinocalanus longicornis'
  i <- dat$Species == 'S. longipes'
  dat$Species[i] <- 'Stephos longipes'
  i <- dat$Species == 'S. major'
  dat$Species[i] <- 'Scaphocalanus major'
  i <- dat$Species == 'S. minor'
  dat$Species[i] <- 'Scolecithricella minor minor'
  i <- dat$Species == 'S. subbrevicornis'
  dat$Species[i] <- 'Scaphocalanus subbrevicornis'
  i <- dat$Species == 'S. terranovae'
  dat$Species[i] <- 'Spinocalanus terranovae'
  i <- dat$Species == 'S. vervoorti'
  dat$Species[i] <- 'Scaphocalanus vervoorti'
  i <- dat$Species == 'Scaphocalanus farrni'
  dat$Species[i] <- 'Scaphocalanus farrani'
  i <- dat$Species == 'Scolecithricella gracialis'
  dat$Species[i] <- 'Scolecithricella glacialis'
  i <- dat$Species == 'Scolecithricella minor'
  dat$Species[i] <- 'Scolecithricella minor minor'
  i <- dat$Species != 'Sio nordenskjoldii'
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'T. antarctica'
  dat$Species[i] <- 'Triconia antarctica'
  i <- dat$Species == 'T. brevis'
  dat$Species[i] <- 'Temorites brevis'
  i <- dat$Species == 'T. magna'
  dat$Species[i] <- 'Tharybis magna'
  i <- dat$Species == 'T. meteorae'
  dat$Species[i] <- 'Teneriforma meteorae'
  
  # unique(dat$Species[i])
  # unique(dat$Data.Source[i])
  
  #' `Now check that the accepted names are used for all species...`
  
  i <- dat$Species == 'Euaetideus australis'
  dat$Species[i] <- 'Aetideus australis'
  i <- dat$Species == 'Acartia clausi'
  dat$Species[i] <- 'Acartia (Acartiura) clausii'
  i <- dat$Species == 'Acartia danae'
  dat$Species[i] <- 'Acartia (Acartia) danae'
  i <- dat$Species == 'Acartia negligens'
  dat$Species[i] <- 'Acartia (Acartia) negligens'
  i <- dat$Species == 'Aetideopsis rostrata and Aetideopsis inflata'
  dat$Species[i] <- 'Aetideopsis rostrata'
  i <- dat$Species == 'Amallothrix dentipes and Scolecithricella dentipes'
  dat$Species[i] <- 'Amallothrix dentipes'
  i <- dat$Species == 'C. antarctica and Neoscolecithrix antarctica'
  dat$Species[i] <- 'Cenognatha antarctica'
  i <- dat$Species == 'C. gracilis and Chiridius subgracilis'
  dat$Species[i] <- 'C. gracilis and Chiridius molestus'
  i <- dat$Species == 'Calanus tonsus'
  dat$Species[i] <- 'Neocalanus tonsus'
  i <- dat$Species == 'Candacia aethiopica'
  dat$Species[i] <- 'Candacia ethiopica'
  i <- dat$Species == 'Centropages orsini'
  dat$Species[i] <- 'Centropages orsinii'
  i <- dat$Species == 'Chiridius polaris and Chiridius subantarcticus'
  dat$Species[i] <- 'Chiridius polaris'
  i <- dat$Species == 'Chirundina streetsi'
  dat$Species[i] <- 'Chirundina streetsii'
  i <- dat$Species == 'Clausocalanus arcuicornis'
  dat$Species[i] <- 'Clausocalanus arcuicornis arcuicornis'
  i <- dat$Species == 'Cosmocalanus darwinii'
  dat$Species[i] <- 'Cosmocalanus darwinii darwinii'
  i <- dat$Species == 'Ditrichocorycaeus dahli'
  dat$Species[i] <- 'Corycaeus (Ditrichocorycaeus) dahli'
  i <- dat$Species == 'Eucalanus hyalinus and Eucalanus elongatus'
  dat$Species[i] <- 'Eucalanus hyalinus and Eucalanus elongatus elongatus'
  i <- dat$Species == 'Euaetideus australis'
  dat$Species[i] <- 'Aetideus australis'
  i <- dat$Species == 'Euaetideus bradyi'
  dat$Species[i] <- 'Aetideus bradyi'
  i <- dat$Species == 'Eucalanus crassus'
  dat$Species[i] <- 'Subeucalanus crassus'
  i <- dat$Species == 'Eucalanus elongatus'
  dat$Species[i] <- 'Eucalanus elongatus elongatus'
  i <- dat$Species == 'Eucalanus longiceps'
  dat$Species[i] <- 'Subeucalanus longiceps'
  i <- dat$Species == 'Eucalanus mucronatus'
  dat$Species[i] <- 'Subeucalanus mucronatus'
  i <- dat$Species == 'Eucalanus pseudoattenuatus'
  dat$Species[i] <- 'Pareucalanus attenuatus'
  i <- dat$Species == 'Eucalanus sewelli'
  dat$Species[i] <- 'Pareucalanus sewelli'
  i <- dat$Species == 'Eucalanus subcrassus'
  dat$Species[i] <- 'Subeucalanus subcrassus'
  i <- dat$Species == 'Eucalanus subtenuis'
  dat$Species[i] <- 'Subeucalanus subtenuis'
  i <- dat$Species == 'Euchaeta antarctica'
  dat$Species[i] <- 'Paraeuchaeta antarctica'
  i <- dat$Species == 'Euchaeta biloba'
  dat$Species[i] <- 'Paraeuchaeta biloba'
  i <- dat$Species == 'Euchaeta flava'
  dat$Species[i] <- 'Paraeuchaeta flava'
  i <- dat$Species == 'Euchirella mesinensis'
  dat$Species[i] <- 'Euchirella messinensis messinensis'
  i <- dat$Species == 'Euchirella rostrata and Euchirella rostramagna'
  dat$Species[i] <- 'Euchirella rostrata and Euchirella rostromagna'
  i <- dat$Species == 'Euchirella tunicata'
  dat$Species[i] <- 'Euchirella truncata'
  i <- dat$Species == 'Gaetanus affinis'
  dat$Species[i] <- 'Gaetanus brevispinus'
  i <- dat$Species == 'Gaetanus brevispinus and Gaidius intermedius'
  dat$Species[i] <- 'Gaetanus brevispinus'
  i <- dat$Species == 'Heterorhabdus clausii'
  dat$Species[i] <- 'Heterorhabdus clausis'
  i <- dat$Species == 'Heterorhabdus farrani'
  dat$Species[i] <- 'Paraheterorhabdus farrani'
  i <- dat$Species == 'Heterorhabdus robustus'
  dat$Species[i] <- 'Paraheterorhabdus robustus'
  i <- dat$Species != 'Hyperia crassa'
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'Lucicutia clausii'
  dat$Species[i] <- 'Lucicutia clausi'
  i <- dat$Species == 'Lucicutia ovalis'
  dat$Species[i] <- 'Lucicutia gaussae'
  i <- dat$Species == 'Mappates alter and Scolecithricella altera'
  dat$Species[i] <- 'Mappates alter and Mixtocalanus alterus'
  i <- dat$Species == 'Megacalanus princeps'
  dat$Species[i] <- 'Megacalanus princeps princeps'
  i <- dat$Species == 'Neocalanus tenuicornis'
  dat$Species[i] <- 'Mesocalanus tenuicornis'
  i <- dat$Species %in% c('Oncaea antarctia', 'Oncaea antarctica')
  dat$Species[i] <- 'Triconia antarctica'
  i <- dat$Species == 'Oncaea conifera'
  dat$Species[i] <- 'Triconia conifera'
  i <- dat$Species == 'Oithona frygida'
  dat$Species[i] <- 'Oithona frigida'
  i <- dat$Species == 'P. antarctica and Pleuromamma robusta'
  dat$Species[i] <- 'P. antarctica and Pleuromamma robusta robusta'
  i <- dat$Species == 'P. pacificus and Pachyptilus pacificus'
  dat$Species[i] <- 'P. pacificus and Pseudhaloptilus pacificus'
  i <- dat$Species == 'Paracalanus aculeatus'
  dat$Species[i] <- 'Paracalanus aculeatus aculeatus'
  i <- dat$Species == 'Paracalanus nudus'
  dat$Species[i] <- 'Delius nudus'
  i <- dat$Species == 'Paraeuchaeta barbata'
  dat$Species[i] <- 'Paraeuchaeta barbata barbata'
  i <- dat$Species == 'Paraeuchaeta barbata barbata and Paraeuchaeta farrani'
  dat$Species[i] <- 'Paraeuchaeta barbata barbata'
  i <- dat$Species == 'Paraheterorhabdus farrani and Heterorhabdus farrani'
  dat$Species[i] <- 'Paraheterorhabdus farrani'
  i <- dat$Species == 'Pareuchaeta antarctica'
  dat$Species[i] <- 'Paraeuchaeta antarctica'
  i <- dat$Species == 'Pareuchaeta biloba'
  dat$Species[i] <- 'Paraeuchaeta biloba'
  i <- dat$Species == 'Pareuchaeta erebi'
  dat$Species[i] <- 'Paraeuchaeta erebi'
  i <- dat$Species == 'Paraeuchaeta rosa'
  dat$Species[i] <- 'Paraeuchaeta rasa'
  i <- dat$Species == 'Pareuchaeta rasa'
  dat$Species[i] <- 'Paraeuchaeta rasa'
  i <- dat$Species == 'Pleuromamma abdominalis'
  dat$Species[i] <- 'Pleuromamma abdominalis abdominalis'
  i <- dat$Species != 'Pleuromamma biloba'
  dat <- sapply(dat, function(z) z[i], simplify = FALSE)
  i <- dat$Species == 'Pleuromamma gracilis'
  dat$Species[i] <- 'Pleuromamma gracilis gracilis'
  i <- dat$Species == 'Pseudoamallothrix cenotelis and Scolecithricella cenotelis'
  dat$Species[i] <- 'Pseudoamallothrix cenotelis'
  i <- dat$Species == 'Pseudoamallothrix emarginata and Scolecithricella emarginata'
  dat$Species[i] <- 'Pseudoamallothrix emarginata'
  i <- dat$Species == 'Pseudochirella spectabilis and Pseudochirella elongata'
  dat$Species[i] <- 'Pseudochirella spectabilis'
  i <- dat$Species == 'Rhincalanus cornutus'
  dat$Species[i] <- 'Rhincalanus cornutus cornutus'
  i <- dat$Species == 'S. longiceps and Eucalanus longiceps'
  dat$Species[i] <- 'S. longiceps and Subeucalanus longiceps'
  i <- dat$Species == 'Scolecithricella dentipes'
  dat$Species[i] <- 'Amallothrix dentipes'
  i <- dat$Species == 'Scolecithricella glacialis'
  dat$Species[i] <- 'Scolecithricella minor minor'
  i <- dat$Species == 'Scolecithricella globulosa and Scolecithricella schizosoma'
  dat$Species[i] <- 'Scolecithricella globulosa'
  i <- dat$Species == 'Scolecithricella ovata'
  dat$Species[i] <- 'Pseudoamallothrix ovata'
  i <- dat$Species == 'Scolecithrix polaris'
  dat$Species[i] <- 'Amallothrix polaris'
  i <- dat$Species == 'Spinocalanus abyssalis'
  dat$Species[i] <- 'Spinocalanus abyssalis abyssalis'
  
  # length(unique(dat$Species))
  
  i <- dat$Species %in% c('Cop spp', 'Copepoda sp')
  dat$Species[i] <- 'Copepoda spp'
  
  
  dat$Species <- factor(dat$Species, levels = sort(unique(dat$Species)))
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Maturity/lifeStage/copepoditeStage
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Maturity = adult/juvenile/unspecified
  message('\n', '-- Maturity/life stage/copepodite stage')
  
  dat$lifestage <- NA
  dat$copepoditestage <- NA
  
  i <- grep('stage', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  
  # sort(unique(dat$copepoditeStage))
  j <- !is.na(dat$copepoditeStage) & dat$copepoditeStage == 'C6'
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$copepoditeStage) & 
    dat$copepoditeStage %in% c(paste0('C', 1:5), 'C4-C5', 'C3-C5')
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$copepoditeStage) & dat$copepoditeStage %in% c('unspecified')
  dat$maturity[is.na(dat$maturity) & j] <- 'unspecified'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$copepoditeStage) & dat$copepoditeStage %in% c('not copepodite')
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'unspecified'
  j <- !is.na(dat$copepoditeStage) & 
    {!dat$copepoditeStage %in% c('not copepodite')}
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- dat$copepoditeStage[j]
  dat$copepoditeStage <- NULL
  
  # unique(dat$Copepodite.stage) # copepodite stage
  j <- !is.na(dat$Copepodite.stage) & dat$Copepodite.stage == 'C6'
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  j <- !is.na(dat$Copepodite.stage) & dat$Copepodite.stage != 'C6' & 
    grepl('C6', dat$Copepodite.stage)
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile and adult'
  j <- !is.na(dat$Copepodite.stage) & !grepl('C6', dat$Copepodite.stage)
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  j <- !is.na(dat$Copepodite.stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- dat$Copepodite.stage[is.na(dat$copepoditestage) & j]
  dat$Copepodite.stage <- NULL
  
  # unique(dat$CopepoditeStage) # copepodite stage
  j <- !is.na(dat$CopepoditeStage) & dat$CopepoditeStage == 'C4-C6'
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile and adult'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- dat$CopepoditeStage[is.na(dat$copepoditestage) & j]
  dat$CopepoditeStage <- NULL
  
  # unique(dat$Copepodite.Stage) # maturity/lifestage/copepodite stage
  j <- !is.na(dat$Copepodite.Stage) & dat$Copepodite.Stage == 'C6'
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  j <- !is.na(dat$Copepodite.Stage) & dat$Copepodite.Stage %in% paste0('C', 1:5)
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  j <- !is.na(dat$Copepodite.Stage) & grepl('-C6', dat$Copepodite.Stage)
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile and adult'
  j <- !is.na(dat$Copepodite.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  dat$copepoditestage[!is.na(dat$copepoditestage) & j] <- dat$Copepodite.Stage[!is.na(dat$copepoditestage) & j]
  dat$Copepodite.Stage <- NULL
  
  # sort(unique(dat$Development.Stage)) # maturity/lifestage/copepodite stage -- this is a mess due to the BAS_lower.trophic.database
  j <- !is.na(dat$Development.Stage) & dat$Development.Stage %in% c('adult', 'C6')
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- 'C6'
  j <- !is.na(dat$Development.Stage) & grepl('-C6', dat$Development.Stage)
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile and adult'
  j <- !is.na(dat$Development.Stage) & 
    {dat$Development.Stage %in% c(paste0('C', 1:5), 'nauplius', 'copepodite') |
        {grepl('-', dat$Development.Stage) & !grepl('6', dat$Development.Stage)}}
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  j <- !is.na(dat$Development.Stage) & 
    !dat$Development.Stage %in% c('nauplius', 'unspecified')
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$Development.Stage) & 
    dat$Development.Stage %in% c('nauplius', 'unspecified')
  dat$lifestage[is.na(dat$lifestage) & j] <- dat$Development.Stage[is.na(dat$lifestage) & j]
  j <- !is.na(dat$Development.Stage) & grepl('C', dat$Development.Stage)
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- dat$Development.Stage[is.na(dat$copepoditestage) & j]
  dat$Development.Stage <- NULL
  
  # unique(dat$Life.Stage) # maturity/lifestage/copepodite stage
  j <- !is.na(dat$Life.Stage) & 
    dat$Life.Stage %in% c('adults', 'adult', 'Adult', 'C6') | 
    grepl('medusa', dat$Life.Stage)
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  j <- !is.na(dat$Life.Stage) & dat$Life.Stage != 'C6' & 
    grepl('C6', dat$Life.Stage)
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile and adult'
  j <- !is.na(dat$Life.Stage) & !grepl('C6', dat$Life.Stage)  &
    {dat$Life.Stage %in% paste0('C', 1:5) | 
        apply(Vectorize(grepl, 'pattern')(paste0('C', 1:5, '-'), dat$Life.Stage), 1, any) |
        apply(Vectorize(grepl, 'pattern')(paste0('F', 1:6), dat$Life.Stage), 1, any) |
        dat$Life.Stage == 'N' |
        grepl('larva', dat$Life.Stage, ignore.case = TRUE) | 
        grepl('calyp', dat$Life.Stage, ignore.case = TRUE) |
        grepl('copepodite', dat$Life.Stage, ignore.case = TRUE) | 
        grepl('furci', dat$Life.Stage, ignore.case = TRUE) |
        grepl('juvenile', dat$Life.Stage, ignore.case = TRUE) | 
        grepl('nauplii', dat$Life.Stage, ignore.case = TRUE) |
        grepl('polyp', dat$Life.Stage, ignore.case = TRUE) | 
        grepl('sub ', dat$Life.Stage)}
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  j <- !is.na(dat$Life.Stage) & 
    {apply(Vectorize(grepl, 'pattern')(paste0('C', 1:6), dat$Life.Stage), 1, any) |
        grepl('copepodite', dat$Life.Stage)}
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$Life.Stage) & grepl('egg', dat$Life.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'egg'
  j <- !is.na(dat$Life.Stage) & {grepl('furcil', dat$Life.Stage) |
      apply(Vectorize(grepl, 'pattern')(paste0('F', 1:6), dat$Life.Stage), 1, any)}
  dat$lifestage[is.na(dat$lifestage) & j] <- 'furcilia'
  j <- !is.na(dat$Life.Stage) & grepl('larva', dat$Life.Stage, ignore.case = TRUE)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'larvae'
  j <- !is.na(dat$Life.Stage) & grepl('medus', dat$Life.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'medusae'
  j <- !is.na(dat$Life.Stage) & {grepl('naupli', dat$Life.Stage) | 
      dat$Life.Stage == 'N'}
  dat$lifestage[is.na(dat$lifestage) & j] <- 'nauplii'
  j <- !is.na(dat$Life.Stage) & grepl('polyp', dat$Life.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'polyp'
  j <- !is.na(dat$Life.Stage) & 
    dat$Life.Stage %in% c('adults', 'adult', 'Adult', 'C6')
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$Life.Stage) & substr(dat$Life.Stage, 1, 2) %in% paste0('C', 1:6)
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- dat$Life.Stage[is.na(dat$copepoditestage) & j]
  j <- !is.na(dat$Life.Stage) & 
    grepl('adult', dat$Life.Stage, ignore.case = TRUE) &
    !grepl('sub', dat$Life.Stage)
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- 'C6'
  dat$Life.Stage <- NULL
  # dat <- dat %>% select(-Life.Stage); invisible(gc())
  
  # unique(dat$Life.stage) # maturity/lifestage
  j <- !is.na(dat$Life.stage) & dat$Life.stage == 'copepodite'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$Life.stage) & dat$Life.stage == 'nauplius'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'nauplius'
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  j <- !is.na(dat$Life.stage) & dat$Life.stage == 'egg'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'egg'
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  dat$Life.stage <- NULL
  
  # unique(dat$lifeStage) # maturity/lifestage
  j <- !is.na(dat$lifeStage) & dat$lifeStage == 'adult'
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- 'C6'
  j <- !is.na(dat$lifeStage) & dat$lifeStage == 'copepodite'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$lifeStage) & dat$lifeStage == 'nauplius'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'nauplius'
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  dat$lifeStage <- NULL
  
  
  # unique(dat$Plankton.Stage) # maturity/lifestage
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'adult'
  dat$maturity[is.na(dat$maturity) & j] <- 'adult'
  j <- !is.na(dat$Plankton.Stage) &
    dat$Plankton.Stage %in% c(
      'actinotrocha larva', 'calyptopis', 'cyphonautes larva', 'eggs',
      'furcilia', 'juvenile', 'juvenile or larvae', 'larva', 'nauplii',
      'polyp', 'post-calyptopis', 'tornaria larva', 'trochophore larva')
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'adult or sub-adult'
  dat$maturity[is.na(dat$maturity) & j] <- 'juvenile and adult'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'adult'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'calyptopis'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'calyptopis'
  j <- !is.na(dat$Plankton.Stage) & grepl('copepodite', dat$Plankton.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'copepodite'
  j <- !is.na(dat$Plankton.Stage) & grepl('egg', dat$Plankton.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'egg'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'furcilia'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'furcilia'
  j <- !is.na(dat$Plankton.Stage) & grepl('larva', dat$Plankton.Stage)
  dat$lifestage[is.na(dat$lifestage) & j] <- 'larvae'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'medusae'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'medusae'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'nauplii'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'nauplii'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'polyp'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'polyp'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'post-calyptopis'
  dat$lifestage[is.na(dat$lifestage) & j] <- 'post-calyptopis'
  j <- !is.na(dat$Plankton.Stage) & dat$Plankton.Stage == 'adult'
  dat$copepoditestage[is.na(dat$copepoditestage) & j] <- 'C6'
  dat$Plankton.Stage <- NULL
  
  
  i <- grep('maturity', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # any(!is.na(dat$maturity) & {dat$maturity == '' | dat$maturity == ' '})
  j <- !is.na(dat$Maturity) & is.na(dat$maturity)
  dat$maturity[j] <- dat$Maturity[j]
  dat$Maturity <- NULL
  
  #' Infill/correct any values missed above
  # print(unique(data.frame(maturity = dat$maturity, lifestage = dat$lifestage, copepoditestage = dat$copepoditestage)))
  j <- !is.na(dat$maturity) & dat$maturity == 'adult' & is.na(dat$lifestage)
  dat$lifestage[j] <- 'copepodite'
  j <- !is.na(dat$maturity) & dat$maturity == 'adult' & is.na(dat$copepoditestage)
  dat$copepoditestage[j] <- 'C6'
  j <- !is.na(dat$lifestage) & dat$lifestage == 'egg' & is.na(dat$maturity)
  dat$maturity[j] <- 'juvenile'
  j <- !is.na(dat$copepoditestage) & grepl('-C6', dat$copepoditestage) &
    dat$maturity == 'juvenile'
  dat$maturity[j] <- 'juvenile and adult'
  j <- !is.na(dat$maturity) & dat$maturity == 'juvenile' & is.na(dat$lifestage)
  dat$lifestage[j] <- 'unspecified'
  j <- !is.na(dat$maturity) & dat$maturity == 'juvenile' & !is.na(dat$lifestage) & 
    dat$lifestage == 'copepodite' & is.na(dat$copepoditestage)
  dat$copepoditestage[j] <- 'unspecified'
  j <- is.na(dat$maturity) & !is.na(dat$lifestage) & dat$lifestage == 'unspecified'
  dat$maturity[j] <- 'unspecified'
  
  names(dat)[which(names(dat) == 'maturity')] <- 'Maturity'
  names(dat)[which(names(dat) == 'lifestage')] <- 'Life.Stage'
  names(dat)[which(names(dat) == 'copepoditestage')] <- 'Copepodite.Stage'
  
  
  dat$Life.Stage <- gsub('nauplii', 'nauplius', dat$Life.Stage)
  
  k <- c('juvenile','juvenile and adult','adult')
  k <- c(k, sort(unique(dat$Maturity[!dat$Maturity %in% k])))
  dat$Maturity <- factor(dat$Maturity, levels = k)
  
  k <- c('egg', 'nauplius', 'copepodite')
  k <- c(k, sort(unique(dat$Life.Stage[!dat$Life.Stage %in% k])))
  dat$Life.Stage <- factor(dat$Life.Stage, levels = k)
  
  k <- paste0('C', 1:6)
  k <- c(k, sort(unique(dat$Copepodite.Stage[!dat$Copepodite.Stage %in% k])))
  dat$Copepodite.Stage <- factor(dat$Copepodite.Stage, levels = k)
  
  rm(k)
  
  #' ~~~
  #' Sex
  #' ~~~
  message('\n', '-- Sex')
  
  i <- grep('sex', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  j <- !is.na(dat$Sex)
  dat$sex[is.na(dat$sex) & j] <- dat$Sex[is.na(dat$sex) & j]
  dat$Sex <- NULL
  names(dat)[grepl('sex', names(dat))] <- 'Sex'
  
  j <- !is.na(dat$Maturity) & dat$Maturity != 'adult'
  dat$Sex[j] <- NA
  j <- !is.na(dat$Maturity) & dat$Maturity == 'adult' & is.na(dat$Sex)
  dat$Sex[j] <- 'unspecified'
  dat$Sex <- gsub('unknown', 'unspecified', dat$Sex)
  j <- !is.na(dat$Sex) & dat$Sex != 'unspecified'
  dat$Maturity[j] <- 'adult'
  dat$Life.Stage[j] <- 'copepodite'
  dat$Copepodite.Stage[j] <- 'C6'
  j <- !is.na(dat$Sex) & dat$Sex == 'unspecified' & is.na(dat$Maturity) & 
    is.na(dat$Life.Stage) & is.na(dat$Copepodite.Stage)
  dat$Sex[j] <- NA
  
  dat$Sex <- factor(dat$Sex,
                    levels = sort(unique(dat$Sex)))
  
  # x <- unique(as.data.frame(dat[c('Maturity','Life.Stage','Copepodite.Stage','Sex')]))
  # x
  
  
  
  #' ~~~~~~~~~~~~~~~~~~
  #' Longitude/latitude
  #' ~~~~~~~~~~~~~~~~~~
  
  message('\n', '-- Longitude/latitude')
  
  i <- grep('lon', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # head(dat[,i, drop = FALSE])
  j <- is.na(dat$decimalLongitude) & !is.na(dat$Lon)
  dat$decimalLongitude[j] <- dat$Lon[j]
  j <- is.na(dat$decimalLongitude) & !is.na(dat$Longitude)
  dat$decimalLongitude[j] <- dat$Longitude[j]
  j <- is.na(dat$decimalLongitude) & !is.na(dat$avgLongNet)
  dat$decimalLongitude[j] <- dat$avgLongNet[j]
  j <- is.na(dat$decimalLongitude) & !is.na(dat$Longitude.E)
  dat$decimalLongitude[j] <- dat$Longitude.E[j]
  
  dat$Longitude.End <- dat$End.lon
  j <- is.na(dat$Longitude.End) & !is.na(dat$LongitudeEnd)
  dat$Longitude.End[j] <- dat$LongitudeEnd[j]
  dat$Longitude.Start <- dat$LongitudeStart
  
  omit <- c('Lon', 'End.lon', 'Longitude', 'Longitude.E', 'LongitudeStart',
            'LongitudeEnd', 'avgLongNet')
  dat[omit] <- NULL
  names(dat)[grepl('decimalLongitude', names(dat))] <- 'Longitude'
  
  i <- grep('lat', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # head(dat[,i, drop = FALSE])
  j <- is.na(dat$decimalLatitude) & !is.na(dat$Lat)
  dat$decimalLatitude[j] <- dat$Lat[j]
  j <- is.na(dat$decimalLatitude) & !is.na(dat$Latitude)
  dat$decimalLatitude[j] <- dat$Latitude[j]
  j <- is.na(dat$decimalLatitude) & !is.na(dat$avgLatNet)
  dat$decimalLatitude[j] <- dat$avgLatNet[j]
  j <- is.na(dat$decimalLatitude) & !is.na(dat$Latitude.S)
  dat$decimalLatitude[j] <- dat$Latitude.S[j]
  
  dat$Latitude.End <- dat$End.lat
  j <- is.na(dat$Latitude.End) & !is.na(dat$LatitudeEnd)
  dat$Latitude.End[j] <- dat$LatitudeEnd[j]
  dat$Latitude.Start <- dat$LatitudeStart
  
  omit <- c('Lat', 'End.lat', 'Latitude', 'Latitude.S', 'LatitudeStart',
            'LatitudeEnd', 'avgLatNet')
  dat[omit] <- NULL
  names(dat)[grepl('decimalLatitude', names(dat))] <- 'Latitude'
  
  #' Infill missing coordinates with start/end points if possible
  j <- is.na(dat$Longitude)
  dat$Longitude[j] <- 0.5 * {dat$Longitude.Start[j] + dat$Longitude.End[j]}
  j <- is.na(dat$Longitude)
  dat$Longitude[j] <- dat$Longitude.Start[j]
  j <- is.na(dat$Longitude)
  dat$Longitude[j] <- dat$Longitude.End[j]
  
  j <- is.na(dat$Latitude)
  dat$Latitude[j] <- 0.5 * {dat$Latitude.Start[j] + dat$Latitude.End[j]}
  j <- is.na(dat$Latitude)
  dat$Latitude[j] <- dat$Latitude.Start[j]
  j <- is.na(dat$Latitude)
  dat$Latitude[j] <- dat$Latitude.End[j]
  
  if(round.lat.lon){
    dat$Longitude <- round(dat$Longitude, lat.lon.dp)
    dat$Longitude.Start <- round(dat$Longitude.Start, lat.lon.dp)
    dat$Longitude.End <- round(dat$Longitude.End, lat.lon.dp)
    dat$Latitude <- round(dat$Latitude, lat.lon.dp)
    dat$Latitude.Start <- round(dat$Latitude.Start, lat.lon.dp)
    dat$Latitude.End <- round(dat$Latitude.End, lat.lon.dp)
  }
  
  
  #' ~~~~~
  #' Depth
  #' ~~~~~
  
  message('\n', '-- Depth')
  
  i <- grep('Depth', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # head(dat[,i, drop = FALSE])
  
  x <- data.frame(Data.Source = dat$Data.Source, as.data.frame(dat[i]))
  y <- setNames(lapply(unique(x$Data.Source), function(z){
    y <- x[x$Data.Source == z,-1]
    names(y)[apply(!is.na(y), 2, any)]}),
    unique(x$Data.Source))
  # print(y)
  
  j <- is.na(dat$depth) & !is.na(dat$Max.depth)
  dat$depth[j] <- dat$Max.depth[j]
  j <- is.na(dat$depth) & !is.na(dat$Depth.Sampled)
  dat$depth[j] <- dat$Depth.Sampled[j]
  j <- is.na(dat$depth) & !is.na(dat$Depth.water_m)
  dat$depth[j] <- dat$Depth.water_m[j]
  j <- is.na(dat$depth) & !is.na(dat$DepthMaximum_m)
  dat$depth[j] <- dat$DepthMaximum_m[j]
  j <- is.na(dat$depth) & !is.na(dat$Depth.Mid)
  dat$depth[j] <- dat$Depth.Mid[j]
  
  omit <- c('Max.depth', 'Depth.Sampled', 'Depth.water_m', 'DepthMaximum_m',
            'Depth.Mid')
  dat[omit] <- NULL
  
  i <- grep('Depth', names(dat), ignore.case = TRUE)
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  
  j <- is.na(dat$minimumDepthInMeters) & !is.na(dat$Depth.top_m)
  dat$minimumDepthInMeters[j] <- dat$Depth.top_m[j]
  j <- is.na(dat$minimumDepthInMeters) & !is.na(dat$minDepth)
  dat$minimumDepthInMeters[j] <- dat$minDepth[j]
  j <- is.na(dat$minimumDepthInMeters) & !is.na(dat$Depth.top)
  dat$minimumDepthInMeters[j] <- dat$Depth.top[j]
  j <- is.na(dat$minimumDepthInMeters) & !is.na(dat$NetDepthEnd)
  dat$minimumDepthInMeters[j] <- dat$NetDepthEnd[j]
  j <- is.na(dat$minimumDepthInMeters) & !is.na(dat$Depth.Min)
  dat$minimumDepthInMeters[j] <- dat$Depth.Min[j]
  j <- is.na(dat$minimumDepthInMeters) & !is.na(dat$Closed.depth)
  dat$minimumDepthInMeters[j] <- dat$Closed.depth[j]
  
  j <- is.na(dat$maximumDepthInMeters) & !is.na(dat$Depth.bot_m)
  dat$maximumDepthInMeters[j] <- dat$Depth.bot_m[j]
  j <- is.na(dat$maximumDepthInMeters) & !is.na(dat$maxDepth)
  dat$maximumDepthInMeters[j] <- dat$maxDepth[j]
  j <- is.na(dat$maximumDepthInMeters) & !is.na(dat$Depth.bottom)
  dat$maximumDepthInMeters[j] <- dat$Depth.bottom[j]
  j <- is.na(dat$maximumDepthInMeters) & !is.na(dat$NetDepthStart)
  dat$maximumDepthInMeters[j] <- dat$NetDepthStart[j]
  j <- is.na(dat$maximumDepthInMeters) & !is.na(dat$Depth.Max)
  dat$maximumDepthInMeters[j] <- dat$Depth.Max[j]
  j <- is.na(dat$maximumDepthInMeters) & !is.na(dat$Open.depth)
  dat$maximumDepthInMeters[j] <- dat$Open.depth[j]
  
  omit <- c('Depth.top_m', 'minDepth', 'Depth.top', 'Depth.Min', 'NetDepthEnd',
            'Closed.depth', 'Depth.bot_m', 'maxDepth', 'Depth.bottom',
            'Depth.Max', 'NetDepthStart', 'Open.depth')
  dat[omit] <- NULL
  
  dat$Tow.Depth.Target <- dat$TowDepthTarget
  j <- is.na(dat$Tow.Depth.Target) & !is.na(dat$DepthTarget)
  dat$Tow.Depth.Target[j] <- dat$DepthTarget[j]
  
  omit <- c('TowDepthTarget', 'DepthTarget')
  dat[omit] <- NULL
  
  names(dat)[names(dat) == 'depth'] <- 'Depth'
  names(dat)[names(dat) == 'minimumDepthInMeters'] <- 'Depth.Top'
  names(dat)[names(dat) == 'maximumDepthInMeters'] <- 'Depth.Bottom'
  
  #' Error-check
  j <- !{is.na(dat$Depth.Top) | is.na(dat$Depth.Bottom)} #' top & bottom depth recorded
  k <- j & dat$Depth.Top > dat$Depth.Bottom #' top recorded as deeper than bottom
  # j <- k & dat$Depth.Top >= dat$Depth & dat$Depth.Bottom <= dat$Depth # depth in between range
  xt <- dat$Depth.Top
  xb <- dat$Depth.Bottom
  dat$Depth.Top[k] <- xb[k] #' swap incorrectly input depth ranges
  dat$Depth.Bottom[k] <- xt[k]
  rm(xt, xb)
  
  dat$depthAccuracy <- NULL
  
  #' Infill missing depths from top/bottom depths if possible
  j <- is.na(dat$Depth)
  k <- !is.na(dat$Depth.Bottom) & is.na(dat$Depth.Top)
  jk <- j & k
  dat$Depth[jk] <- dat$Depth.Bottom[jk]
  
  j <- is.na(dat$Depth)
  k <- !is.na(dat$Depth.Bottom) & !is.na(dat$Depth.Top)
  jk <- j & k
  dat$Depth[jk] <- 0.5 * {dat$Depth.Top[jk] + dat$Depth.Bottom[jk]}
  # dat$Depth[jk] <- rowMeans(dat[jk,c('Depth.Bottom','Depth.Top')]) # for dat as data frame
  
  dat$Seafloor.Depth <- dat$WaterDepth #' Seafloor depth should be estimable from bathymetry data and the lat/lon coordinates
  j <- is.na(dat$Seafloor.Depth) & !is.na(dat$WaterDepth_m)
  dat$Seafloor.Depth[j] <- dat$WaterDepth_m[j]
  omit <- c('WaterDepth','WaterDepth_m')
  dat[omit] <- NULL
  
  
  #' ~~~~
  #' Gear
  #' ~~~~
  
  message('\n', '-- Sampling gear')
  
  i <- unique(c(grep('samp', names(dat), ignore.case = TRUE),
                grep('net', names(dat), ignore.case = TRUE),
                grep('gear', names(dat), ignore.case = TRUE)))
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # head(dat[,i, drop = FALSE])
  j <- is.na(dat$samplingProtocol) & !is.na(dat$Net.type)
  dat$samplingProtocol[j] <- dat$Net.type[j]
  j <- is.na(dat$samplingProtocol) & !is.na(dat$Net.Type)
  dat$samplingProtocol[j] <- dat$Net.Type[j]
  j <- is.na(dat$samplingProtocol) & !is.na(dat$SamplingGear)
  dat$samplingProtocol[j] <- dat$SamplingGear[j]
  j <- is.na(dat$samplingProtocol) & !is.na(dat$Gear)
  dat$samplingProtocol[j] <- dat$Gear[j]
  j <- is.na(dat$samplingProtocol) & !is.na(dat$Net)
  dat$samplingProtocol[j] <- dat$Net[j]
  
  dat$Sample.Gear <- dat$samplingProtocol
  
  omit <- c('samplingProtocol', 'Net', 'Net.type', 'Net.Type', 'SamplingGear',
            'Gear')
  dat[omit] <- NULL
  
  #' Regularise gear names
  dat$Sample.Gear[grepl('Bogorov-Rass', dat$Sample.Gear)] <- 'Bogorov-Rass net'
  dat$Sample.Gear[grepl('bongo', dat$Sample.Gear, ignore.case = TRUE)] <- 'Bongo net'
  dat$Sample.Gear[dat$Sample.Gear == 'bottle'] <- 'Bottle'
  dat$Sample.Gear[grepl('Clarke-Bumpus', dat$Sample.Gear)] <- 'Clarke-Bumpus sampler'
  dat$Sample.Gear[dat$Sample.Gear == 'dredge'] <- 'Dredge'
  dat$Sample.Gear[dat$Sample.Gear == 'K100 Net'] <- 'K100 net'
  dat$Sample.Gear[grepl('micro net', dat$Sample.Gear, ignore.case = TRUE)]  <- 'Micro net '
  dat$Sample.Gear[dat$Sample.Gear == 'multinet'] <- 'Multinet'
  dat$Sample.Gear[grepl('nansen', dat$Sample.Gear, ignore.case = TRUE) & 
                    !grepl('bottle', dat$Sample.Gear, ignore.case = TRUE)] <- 'Nansen net'
  dat$Sample.Gear[grepl('norpac', dat$Sample.Gear, ignore.case = TRUE)] <- 'NorPac'
  dat$Sample.Gear[dat$Sample.Gear == 'ORI-C Net'] <- 'ORI-C net'
  dat$Sample.Gear[grepl('plankton net', dat$Sample.Gear, ignore.case = TRUE)] <- 'Plankton net'
  dat$Sample.Gear[dat$Sample.Gear == 'ring net'] <- 'Ring net'
  dat$Sample.Gear[dat$Sample.Gear == 'water pump'] <- 'Water pump'
  dat$Sample.Gear <- gsub('WP-2', 'WP2', gsub('WPII', 'WP2', dat$Sample.Gear))
  dat$Sample.Gear[dat$Sample.Gear %in% c('Other','unknown gear','Unspecified net')] <- 'Unspecified'
  
  # paste(sort(unique(dat$Sample.Gear)), collapse = ", ")
  
  #' Net mouth opening - get info from some entries in Sample.Gear, and any other data columns...
  i <- unique(c(grep('mouth', names(dat), ignore.case = TRUE),
                grep('open', names(dat), ignore.case = TRUE),
                grep('area', names(dat), ignore.case = TRUE),
                grep('net', names(dat), ignore.case = TRUE),
                grep('gear', names(dat), ignore.case = TRUE),
                grep('mesh', names(dat), ignore.case = TRUE)))
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  
  dat$Net.Mouth.Area <- NA
  
  j <- is.na(dat$Net.Mouth.Area) & !is.na(dat$Net.Area)
  dat$Net.Mouth.Area[j] <- dat$Net.Area[j]
  dat$Net.Area <- NULL
  
  # unique(dat$Sample.Gear)
  
  dat$Net.Mouth.Area[dat$Sample.Gear == 'RMT1+8'] <- '1 and 8 m2'
  dat$Net.Mouth.Area[dat$Sample.Gear == 'RMT8'] <- '8 m2'
  dat$Net.Mouth.Area[dat$Sample.Gear == 'RMT25'] <- '25 m2'
  dat$Sample.Gear[grepl('RMT', dat$Sample.Gear)] <- 'RMT'
  
  dat$Net.Mouth.Area[grepl('mouth area = 0.1 sq-meters', dat$Sample.Gear)] <- '0.1 m2'
  dat$Net.Mouth.Area[dat$Sample.Gear == 'JUDAY38 NET'] <- '0.1 m2'
  dat$Net.Mouth.Area[dat$Sample.Gear == 'JUDAY 80/113 Oceanic Model'] <- '0.5 m2'
  dat$Net.Mouth.Area[grepl('juday small', dat$Sample.Gear, ignore.case = TRUE)] <- '0.1 m2'
  dat$Sample.Gear[grepl('juday', dat$Sample.Gear, ignore.case = TRUE)] <- 'Juday net'
  # sort(unique(dat$Sample.Gear))
  
  dat$Sample.Gear <- gsub('see cruise report', 'unspecified', dat$Sample.Gear)
  
  
  #' ~~~~~~~~~
  #' Mesh size
  #' ~~~~~~~~~
  
  message('\n', '-- Net mesh size')
  
  i <- unique(c(grep('mouth', names(dat), ignore.case = TRUE),
                grep('size', names(dat), ignore.case = TRUE),
                grep('open', names(dat), ignore.case = TRUE),
                grep('area', names(dat), ignore.case = TRUE),
                grep('net', names(dat), ignore.case = TRUE),
                grep('gear', names(dat), ignore.case = TRUE),
                grep('mesh', names(dat), ignore.case = TRUE)))
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # head(dat[,i, drop = FALSE])
  j <- is.na(dat$Net.mesh.size) & !is.na(dat$Mesh.Size)
  dat$Net.mesh.size[j] <- dat$Mesh.Size[j]
  j <- is.na(dat$Net.mesh.size) & !is.na(dat$Mesh)
  dat$Net.mesh.size[j] <- dat$Mesh[j]
  j <- !is.na(dat$MeshSize)
  dat$Net.mesh.size[j] <- dat$MeshSize[j]
  
  omit <- c('Mesh.Size', 'Mesh', 'MeshSize')
  dat[omit] <- NULL
  names(dat)[names(dat) == 'Net.mesh.size'] <- 'Net.Mesh.Size'
  
  #' Search dynamic properties for mesh size
  j <- is.na(dat$Net.Mesh.Size) &
    grepl('MeshSizeInMicrometer', dat$dynamicProperties)
  dat$Net.Mesh.Size[j] <- paste(substr(dat$dynamicProperties[j], 23, 25), 'μm')
  
  #' Search occurence remarks for mesh size
  j <- grepl('mesh', dat$occurrenceRemarks, ignore.case = TRUE)
  # unique(dat$occurrenceRemarks[j])
  k <- j & grepl('µm monyl mesh', dat$occurrenceRemarks)
  dat$Net.Mesh.Size[k] <- substr(dat$occurrenceRemarks[k], 27, 32)
  dat$Net.Mouth.Area[k] <- '0.2 m2'
  # unique(dat$occurrenceRemarks[j & !k])
  k <- grepl('Mesh size: ', dat$occurrenceRemarks) & 
    !grepl('Mesh size:  ', dat$occurrenceRemarks)
  i <- unlist(gregexec('Mesh size: ', dat$occurrenceRemarks[k]))
  dat$Net.Mesh.Size[k] <- substr(dat$occurrenceRemarks[k], i+11, i+17)
  
  #' Remove extra white space
  nc <- nchar(dat$Net.Mesh.Size)
  i <- substr(dat$Net.Mesh.Size, nc, nc) == ' '
  i[is.na(i)] <- FALSE
  while(any(i)){
    dat$Net.Mesh.Size[i] <- substr(dat$Net.Mesh.Size[i], 1, nc[i] - 1)
    nc <- nchar(dat$Net.Mesh.Size)
    i <- substr(dat$Net.Mesh.Size, nc, nc) == ' '
    i[is.na(i)] <- FALSE
  }
  i <- grepl('  ', dat$Net.Mesh.Size)
  i[is.na(i)] <- FALSE
  while(any(i)){
    dat$Net.Mesh.Size <- gsub('  ', ' ', dat$Net.Mesh.Size)
    i <- grepl('  ', dat$Net.Mesh.Size)
    i[is.na(i)] <- FALSE
  }
  
  #' All mesh sizes are reported in units of µm, but the µ character may not be
  #' consistent -- ensure that it is
  i <- !is.na(dat$Net.Mesh.Size) 
  j <- nchar(dat$Net.Mesh.Size)
  dat$Net.Mesh.Size[i] <- paste(substr(dat$Net.Mesh.Size[i], 1, j[i] - 3), 'µm')
  
  
  #' ~~~~~~~~~
  #' Date/time
  #' ~~~~~~~~~
  
  message('\n', '-- Date/time')
  
  i <- unique(c(grep('date', names(dat), ignore.case = TRUE),
                grep('time', names(dat), ignore.case = TRUE),
                grep('start', names(dat), ignore.case = TRUE),
                grep('end', names(dat), ignore.case = TRUE),
                grep('dhms', names(dat), ignore.case = TRUE)))
  
  # x <- data.frame(Data.Source = dat$Data.Source, as.data.frame(dat[i]))
  # head(x)
  # y <- setNames(lapply(unique(x$Data.Source), function(z){
  #   v <- x[x$Data.Source == z,-1]
  #   names(v)[apply(!is.na(v), 2, any)]}), unique(x$Data.Source))
  # print(y)
  
  if(!{'Date' %in% names(dat)}) dat$Date <- NA
  if(!{'Date.Start' %in% names(dat)}) dat$Date.Start <- NA
  if(!{'Date.End' %in% names(dat)}) dat$Date.End <- NA
  
  j <- is.na(dat$Date) & !is.na(dat$date_mid) # OBIS
  if(any(j)) dat$Date[j] <- dat$date_mid[j]
  dat$date_mid <- NULL
  
  j <- is.na(dat$Date) & !is.na(dat$eventDate) # GBIF
  if(any(j)) dat$Date[j] <- dat$eventDate[j]
  dat$eventDate <- NULL
  
  j <- is.na(dat$Date) & !is.na(dat$Date.Time) # Schnack-Schiel
  if(any(j)) dat$Date[j] <- format(strptime(dat$Date.Time[j],
                                            format = '%Y-%m-%d'), '%Y-%m-%d')
  
  
  j <- is.na(dat$Date.Start) & !is.na(dat$date_start) #' OBIS
  if(any(j)) dat$Date.Start[j] <- dat$date_start[j]
  j <- is.na(dat$Date.End) & !is.na(dat$date_end)
  if(any(j)) dat$Date.End[j] <- dat$date_end[j]
  omit <- c('date_start', 'date_end')
  dat[omit] <- NULL
  
  j <- is.na(dat$Date.Start) & !is.na(dat$Start.of.event) # BAS bongo/rmt
  if(any(j)) dat$Date.Start[j] <- format(strptime(dat$Start.of.event[j],
                                                  format = '%Y-%m-%d'), '%Y-%m-%d')
  j <- is.na(dat$Date.End) & !is.na(dat$End.of.event)
  if(any(j)) dat$Date.End[j] <- format(strptime(dat$End.of.event[j],
                                                format = '%Y-%m-%d'), '%Y-%m-%d')
  
  
  #' Infill missing Date from Start.Date
  j <- is.na(dat$Date) & !is.na(dat$Date.Start)
  if(any(j)) dat$Date[j] <- dat$Date.Start[j]
  j <- is.na(dat$Date) & !is.na(dat$Date.End)
  if(any(j)) dat$Date[j] <- dat$Date.End[j]
  omit <- c('Date.Start', 'Date.End')
  dat[omit] <- NULL
  
  
  if(!{'Time' %in% names(dat)}) dat$Date <- NA
  
  j <- is.na(dat$Time) & !is.na(dat$Date.Time)
  if(any(j)) dat$Time[j] <- sapply(strsplit(dat$Date.Time[j], ' '), function(z) z[2])
  dat$Date.Time <- NULL
  
  j <- is.na(dat$Time) & !is.na(dat$eventTime)
  if(any(j)) dat$Time[j] <- dat$eventTime[j]
  dat$eventTime <- NULL
  
  j <- is.na(dat$Time) & !is.na(dat$time_mid)
  if(any(j)) dat$Time[j] <- dat$time_mid[j]
  dat$time_mid <- NULL
  
  j <- is.na(dat$Time) & !is.na(dat$Time.Day.Night)
  if(any(j)){
    nc <- nchar(dat$Time.Day.Night[j])
    k <- nc == 5
    dat$Time.Day.Night[j][k] <- paste0(dat$Time.Day.Night[j][k], ':00')
    dat$Time[j] <- dat$Time.Day.Night[j]
    rm(nc, k)
  }
  dat$Time.Day.Night <- NULL
  
  
  j <- is.na(dat$Time) & !is.na(dat$time_start)
  if(any(j)) dat$Time[j] <- dat$time_start[j]
  omit <- c('time_start', 'time_end')
  dat[omit] <- NULL
  
  j <- is.na(dat$Time) & !is.na(dat$Start.of.event)
  if(any(j)) dat$Time[j] <- format(strptime(dat$Start.of.event[j],
                                            '%Y-%m-%d %H:%M:%S'), '%H:%M:%S')
  omit <- c('Start.of.event', 'End.of.event')
  dat[omit] <- NULL
  
  j <- is.na(dat$Time) & !is.na(dat$TimeStart)
  if(any(j)) dat$Time[j] <- dat$TimeStart[j]
  omit <- c('TimeStart', 'TimeEnd')
  dat[omit] <- NULL
  
  omit <- c('verbatimEventDate', 'dateIdentified', 'OpenTime', 'minDHMSNet',
            'maxDHMSNet')
  dat[omit] <- NULL
  
  
  i <- unique(c(grep('year', names(dat), ignore.case = TRUE),
                grep('month', names(dat), ignore.case = TRUE),
                grep('day', names(dat), ignore.case = TRUE),
                grep('date', names(dat), ignore.case = TRUE)))
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # x <- dat[,i, drop = FALSE]
  # head(x)
  
  
  j <- as.Date(dat$Date)
  dat$Year <- as.numeric(format(j, '%Y'))
  dat$Month <- format(j, '%b')
  dat$Day.Of.Year <- as.numeric(format(j, '%j'))
  omit <- c('year', 'dayOfYear', 'startDayOfYear', 'endDayOfYear', 'Day', 'month', 'day')
  dat[omit] <- NULL
  
  j <- is.na(dat$Time)
  dat$Time[j] <- ''
  
  # j <- rowSums(sapply(dat[c('Date.Start','Date.End')], is.na)) == 2
  # dat$Date.Start[j] <- dat$Date[j]
  # dat$Date.End[j] <- dat$Date[j]
  
  
  #' ~~~~~~~~~~~~~~~~
  #' Abundance values
  #' ~~~~~~~~~~~~~~~~
  
  message('\n', '-- Abundance values')
  
  i <- unique(c(grep('quantity', names(dat), ignore.case = TRUE),
                grep('abundance', names(dat), ignore.case = TRUE),
                grep('size', names(dat), ignore.case = TRUE),
                grep('value', names(dat), ignore.case = TRUE),
                grep('measure', names(dat), ignore.case = TRUE),
                grep('occur', names(dat), ignore.case = TRUE),
                grep('unit', names(dat), ignore.case = TRUE)))
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  # x <- dat[,i, drop = FALSE]
  # head(x)
  
  j <- is.na(dat$Value) & !is.na(dat$Abundance)
  dat$Value[j] <- dat$Abundance[j] # BAS
  
  j <- is.na(dat$Value) & !is.na(dat$organismQuantity)
  dat$Value[j] <- dat$organismQuantity[j] # OBIS GBIF
  
  #' Get more abundance values from dynamicProperties & occurrenceRemarks
  j <- is.na(dat$Value)
  sum(j); sum(j) / length(j)
  sum(!is.na(dat$dynamicProperties[j]))
  sum(!is.na(dat$occurrenceRemarks[j]))
  
  k <- j & !is.na(dat$occurrenceRemarks)
  # unique(dat$occurrenceRemarks[k])
  dat$occurrenceRemarks[dat$occurrenceRemarks == 'Abundence='] <- NA
  k <- j & !is.na(dat$occurrenceRemarks)
  i <- k & grepl('Abundence', dat$occurrenceRemarks)
  n <- nchar(dat$occurrenceRemarks[i])
  x <- unlist(gregexec('Abundence', dat$occurrenceRemarks[i]))
  x <- substr(dat$occurrenceRemarks[i], x, n)
  x <- gsub('Abundence', '', x)
  run <- TRUE
  while(run){
    n <- nchar(x)
    x1 <- suppressWarnings(as.numeric(substr(x,1,1)))
    y <- is.na(x1)
    x[y] <- substr(x[y], 2, n[y])
    run <- any(is.na(suppressWarnings(as.numeric(substr(x,1,1)))))
  }
  x <- strsplit(x, ' ')
  xn <- suppressWarnings(as.numeric(sapply(x, function(z) z[1]))) #' values given as a range will be converted into NA
  xu <- sapply(x, function(z) z[2])
  # unique(xu)
  xn[grepl('50m', xu)] <- xn[grepl('50m', xu)] / 50
  xn[grepl('500m', xu)] <- xn[grepl('500m', xu)] / 500
  dat$Value[i] <- xn
  dat$Measurement.Unit[i] <- 'ind/m3'
  
  j <- is.na(dat$Value)
  k <- j & !is.na(dat$occurrenceRemarks)
  i <- k & grepl('Observed density', dat$occurrenceRemarks)
  n <- nchar(dat$occurrenceRemarks[i])
  x <- unlist(gregexec('density', dat$occurrenceRemarks[i]))
  x <- substr(dat$occurrenceRemarks[i], x, n)
  x <- gsub('density', '', x)
  run <- TRUE
  while(run){
    n <- nchar(x)
    x1 <- suppressWarnings(as.numeric(substr(x,1,1)))
    y <- is.na(x1)
    x[y] <- substr(x[y], 2, n[y])
    run <- any(is.na(suppressWarnings(as.numeric(substr(x,1,1)))))
  }
  x <- strsplit(x, 'ind')
  xn <- as.numeric(gsub(' ', '', sapply(x, function(z) z[1])))
  dat$Value[i] <- xn
  dat$Measurement.Unit[i] <- 'ind/m3'
  
  
  j <- is.na(dat$Value)
  k <- j & !is.na(dat$dynamicProperties)
  #unique(dat$dynamicProperties[k])
  i <- k & grepl('observedindividualcount', dat$dynamicProperties)
  x <- dat$dynamicProperties[i]
  n <- nchar(x)
  x[substr(x,n,n) == ';'] <- substr(x,1,n-1)
  x <- strsplit(x, ';')
  xs <- sapply(x, function(z) z[1])
  xn <- sapply(x, function(z) z[2])
  xs <- gsub('samplesize=','',xs)
  xs <- as.numeric(substr(xs, 1, nchar(xs)-3)) #' volume of water (m^3)
  xn <- as.numeric(gsub('observedindividualcount=','',xn))
  y <- xn / xs #' ind/m3
  dat$Value[i] <- y
  dat$Measurement.Unit[i] <- 'ind/m3'
  
  
  #' ~~~~~~~~~~~~~~~
  #' Abundance units
  #' ~~~~~~~~~~~~~~~
  i <- unique(c(grep('unit', names(dat), ignore.case = TRUE),
                grep('type', names(dat), ignore.case = TRUE),
                grep('size', names(dat), ignore.case = TRUE),
                grep('value', names(dat), ignore.case = TRUE),
                grep('measure', names(dat), ignore.case = TRUE),
                grep('occur', names(dat), ignore.case = TRUE)))
  # head(as.data.frame(lapply(dat[i], function(z) z[1:6])))
  
  j <- is.na(dat$Unit) & !is.na(dat$Measurement.Unit)
  dat$Unit[j] <- dat$Measurement.Unit[j] # SS
  
  j <- is.na(dat$Unit) & !is.na(dat$MeasurementUnit)
  dat$Unit[j] <- dat$MeasurementUnit[j] # Palmer
  
  j <- is.na(dat$Unit) & !is.na(dat$Units)
  dat$Unit[j] <- dat$Units[j] # COPEPOD
  
  j <- is.na(dat$Unit) & !is.na(dat$organismQuantityType)
  dat$Unit[j] <- dat$organismQuantityType[j] # OBIS GBIF
  
  omit <- c('organismQuantity', 'organismQuantityType', 'relativeOrganismQuantity',
            'Abundance', 'Measurement.Unit', 'MeasurementUnit', 'Units')
  dat[omit] <- NULL
  
  names(dat)[names(dat) == 'Value'] <- 'Measurement.Value'
  names(dat)[names(dat) == 'Unit'] <- 'Measurement.Unit'
  
  #' Presence/absence records need treated a little differently, requiring their
  #' own columns
  dat$Occurrence.Status <- NA
  dat$Occurrence.Status[!is.na(dat$Measurement.Value) & 
                          dat$Measurement.Value > 0] <- 'present'
  dat$Occurrence.Status[!is.na(dat$Measurement.Value) & 
                          dat$Measurement.Value == 0] <- 'absent'
  dat$Occurrence.Status[is.na(dat$Measurement.Value) & 
                          grepl('present', dat$occurrenceStatus, ignore.case = TRUE)] <- 'present'
  dat$Occurrence.Status[is.na(dat$Measurement.Value) & 
                          grepl('absent', dat$occurrenceStatus, ignore.case = TRUE)] <- 'absent'
  i <- rowSums(vgrepl(c('abundant', 'common', 'present', 'rare', 'very abundant', 'very rare'),
                      dat$Occurrence, ignore.case = TRUE)) == 1
  dat$Occurrence.Status[is.na(dat$Measurement.Value) & i] <- 'present'
  dat$Occurrence.Status[is.na(dat$Measurement.Value) &
                          grepl('absent', dat$Occurrence, ignore.case = TRUE)] <- 'absent'
  dat$Occurrence.Status[is.na(dat$Occurrence.Status) &
                          dat$Measurement.Unit == 'present'] <- 'present'
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Regularise the measurement units
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message('\n', '-- Abundance units')
  
  dat$Measurement.Unit <- gsub('individuals', 'ind', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('number', 'ind', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('num', 'ind', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('#', 'ind', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('ind', 'individuals', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('/m2', ' / m2', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('/m3', ' / m3', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('Per', ' / ', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('individuals/', 'individuals / ', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('individuals / haul', 'individuals', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('code/haul', 'code / haul', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('mg/haul', 'mg / haul', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('ml/haul', 'ml / haul', dat$Measurement.Unit)
  dat$Measurement.Unit <- gsub('mL', 'ml', dat$Measurement.Unit)
  i <- !is.na(dat$Measurement.Unit) & 
    dat$Measurement.Unit %in% c('individuals / 1000m3', 'individuals / 1000 m3')
  dat$Measurement.Value[i] <- dat$Measurement.Value[i] / 1000
  dat$Measurement.Unit[i] <- 'individuals / m3'
  i <- !is.na(dat$Measurement.Unit) & dat$Measurement.Unit == 'ml / 1000m3'
  dat$Measurement.Value[i] <- dat$Measurement.Value[i] / 1000
  dat$Measurement.Unit[i] <- 'ml / m3'
  i <- !is.na(dat$Measurement.Unit) & dat$Measurement.Unit == 'individuals / ml'
  dat$Measurement.Value[i] <- dat$Measurement.Value[i] * 1e6
  dat$Measurement.Unit[i] <- 'individuals / m3'
  i <- !is.na(dat$Measurement.Unit) & dat$Measurement.Unit == 'g / m2'
  dat$Measurement.Value[i] <- dat$Measurement.Value[i] * 1e3
  dat$Measurement.Unit[i] <- 'mg / m2'
  
  dat$Measurement.Unit[!is.na(dat$Occurrence.Status) &
                         is.na(dat$Measurement.Value)] <- ''
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~
  #' Measurement type column
  #' ~~~~~~~~~~~~~~~~~~~~~~~
  dat$measurement <- rep('', length(dat$Measurement.Value))
  j <- !is.na(dat$Measurement.Unit)
  dat$measurement[j & dat$Measurement.Unit == 'DNA sequence reads'] <- 'DNA'
  dat$measurement[j & dat$Measurement.Unit == 'individuals'] <- 'abundance'
  dat$measurement[j & dat$Measurement.Unit == 'individuals / haul'] <- 'abundance'
  dat$measurement[j & dat$Measurement.Unit == 'individuals / m2'] <- 'abundance density'
  dat$measurement[j & dat$Measurement.Unit == 'individuals / m3'] <- 'abundance concentration'
  dat$measurement[j & dat$Measurement.Unit == 'mg / haul'] <- 'biomass'
  dat$measurement[j & dat$Measurement.Unit == 'mg / m2'] <- 'biomass density'
  dat$measurement[j & dat$Measurement.Unit == 'mg / m3'] <- 'biomass concentration'
  dat$measurement[j & dat$Measurement.Unit == 'ml / haul'] <- 'biovolume'
  dat$measurement[j & dat$Measurement.Unit == 'ml / m2'] <- 'biovolume density'
  dat$measurement[j & dat$Measurement.Unit == 'ml / m3'] <- 'biovolume concentration'
  i <- dat$measurement == '' & is.na(dat$Measurement.Value) & !is.na(dat$Occurrence.Status)
  dat$measurement[i] <- 'presence / absence'
  
  omit <- c('Measurement', 'Measurement.Type', 'occurrenceStatus')
  dat[omit] <- NULL
  
  names(dat)[names(dat) == 'measurement'] <- 'Measurement'
  
  
  #' ~~~~~~~~~~~~~
  #' Cruise report
  #' ~~~~~~~~~~~~~
  names(dat)[names(dat) == 'cruiseReport'] <- 'Cruise.Report'
  dat$Cruise.Report[is.na(dat$Cruise.Report)] <- ''
  
  x <- dat$Cruise.Report
  i <- grepl('DOI:', x)
  y <- strsplit(x[i], 'DOI:')
  y <- paste0('https://doi.org/', sapply(y, function(z) z[2]))
  x[i] <- y
  dat$Cruise.Report <- x
  
  
  #' Omit entries lacking any measurement
  # ddd <- as.data.frame(dat[c('Measurement','Measurement.Unit','Occurrence.Status')])
  # unique(ddd)
  i <- !is.na(dat$Occurrence.Status)
  dat <- lapply(dat, function(z) z[i])
  
  #' Omit entries lacking lacking information for any vital fields
  vital.fields <- c('Species', 'Date', 'Longitude', 'Latitude')
  i <- sapply(vital.fields, function(z){
    x <- as.character(dat[[z]])
    x <- is.na(x) | x == ''
    return(x)})
  i <- rowSums(i) == 0
  dat <- lapply(dat, function(z) z[i])
  
  
  #' ~~~~~~~~~~~~
  #' Sample event
  #' ~~~~~~~~~~~~
  #' Assign unique numbers for every event rather than a sequence of numbers
  #' within each data source, to avoid separate events having the same number.
  #' This should be redone after filtering duplicates.
  dat$Time[is.na(dat$Time)] <- ''
  o <- order(dat$Date, dat$Time) # put all records in order of time
  dat <- lapply(dat, function(z) z[o])
  dat$Sample.Event <- NA
  x <- as.data.frame(dat[c('Data.Source', 'Sample.event')]) %>% distinct()
  x$Sample.Event <- 1:nrow(x)
  for(i in unique(dat$Data.Source)){
    j <- dat$Data.Source == i
    xi <- x[x$Data.Source == i,]
    xi <- setNames(xi$Sample.Event, xi$Sample.event)
    s <- dat$Sample.event[j]
    snew <- unname(xi[as.character(s)])
    dat$Sample.Event[j] <- snew
  }
  rm(x, i, j, xi, s, snew)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Select which data columns to retain
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  message('\n', 'Selecting columns to retain in data set')
  
  fields2keep <- c(
    'Data.Source', 'Sample.Event', 'Date', 'Year', 'Month', 'Day.Of.Year', 'Time',
    'Time.Flag', 'Species', 'Maturity', 'Life.Stage', 'Copepodite.Stage', 'Sex',
    'Longitude', 'Latitude', 'Longitude.Start', 'Latitude.Start', 'Longitude.End',
    'Latitude.End', 'Seafloor.Depth', 'Depth', 'Depth.Top', 'Depth.Bottom',
    'Tow.Depth.Target', 'Sample.Gear', 'Net.Mesh.Size', 'Net.Mouth.Area',
    'Tow.Orientation', 'Measurement', 'Measurement.Value', 'Measurement.Unit',
    'Occurrence.Status', 'Cruise.Report')
  
  #' Do these selected fields appear in the data set
  if(!all(fields2keep %in% names(dat))){
    warning('Some fields missing from main data frame. Check selected fields.')}
  
  dat <- dat[fields2keep]
  

  # Omit duplicates ---------------------------------------------------------

  #' ~~~~~~~~~~~~~~~~~~~~~
  #' Remove duplicate rows
  #' ~~~~~~~~~~~~~~~~~~~~~
  message('\n', 'Data scraping & removing duplicate rows')
  
  #' There are some duplicated rows due to records being reported to multiple data
  #' portals, and reported multiple times within data portals. These duplicates
  #' need to be removed, but unfortunately different data portals include varying
  #' amounts of information so, given that we don't want to lose info, we need to
  #' take some care here...
  
  dat <- as.data.frame(dat) #' convert list to data frame
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove duplicates within each data source
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' Use all fields (removes duplicates recorded within the same sample events).
  #' Portal data are treated slightly differently because their sample events
  #' were calculated from coordinates and times.
  portals <- c('OBIS','GBIF')
  round.time <- function(x, inc = 30){
    y <- strsplit(x, ':')
    h <- as.numeric(sapply(y, function(z) z[1]))
    m <- as.numeric(sapply(y, function(z) z[2]))
    s <- as.numeric(sapply(y, function(z) z[3]))
    return(round({h*60+m+s/60}/inc))}
  
  d <- dat %>%
    mutate(Lon = round(Longitude, 2),
           Lat = round(Latitude, 2),
           Time.round = round.time(Time))
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}
  d[is.na(d)] <- -Inf #' replace NA values as these may mess up comparisons
  d$dup <- FALSE
  for(i in unique(d$Data.Source)){
    j <- d$Data.Source == i
    x <- d %>% filter(Data.Source == i)
    if(i %in% portals){
      x <- x %>% select(-Longitude, -Latitude, -Longitude.Start, -Latitude.Start,
                        Longitude.End, -Latitude.End, -Time)
    }else{
      x <- x %>% select(-Lon, -Lat, -Time.round)}
    dup <- duplicated(x)
    d$dup[j] <- dup
  }
  dat$dup <- d$dup
  rm(d)
  dat <- dat %>% filter(!dup) %>% select(-dup)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' As above, but exclude the event field
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' This eliminates identical data that were assigned (probably wrongly)
  #' different sample events.
  d <- dat %>%
    mutate(Lon = round(Longitude, 2),
           Lat = round(Latitude, 2),
           Time.round = round.time(Time))
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}
  d[is.na(d)] <- -Inf
  d$dup <- FALSE
  
  for(i in unique(d$Data.Source)){
    j <- d$Data.Source == i
    x <- d %>% filter(Data.Source == i) %>% select(-Sample.Event)
    if(i %in% portals){
      x <- x %>% select(-Longitude, -Longitude.Start, -Longitude.End, -Latitude,
                        -Latitude.Start, -Latitude.End, -Time)
    }else{
      x <- x %>% select(-Lon, -Latitude, -Time.round)}
    dup <- duplicated(x)
    d$dup[j] <- dup
  }
  dat$dup <- d$dup
  rm(d)
  dat <- dat %>% filter(!dup) %>% select(-dup)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove data portal (OBIS & GBIF) entries identical to original sources
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' Any records from portals that share a date with original sources and are
  #' similarly located can be considered duplicates.
  dupFields <- c('Species', 'Longitude', 'Latitude', 'Date') #' fields to test for duplicate rows
  dupFields_ <- sub('Latitude', 'Lat', sub('Longitude', 'Lon', dupFields)) #' use rounded coordinates for duplicate testing
  
  d <- dat %>%
    mutate(Lon = round(Longitude, 2),
           Lat = round(Latitude, 2)) %>%
    select(all_of(c('Data.Source', dupFields_))) %>%
    distinct()
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}  
  d[is.na(d)] <- -Inf
  
  d$index <- 1:nrow(d)
  d$dup.portal <- FALSE
  for(i in portals){
    j <- unique(d$Data.Source)
    j <- c(j[!j %in% portals], i)
    x <- d %>% filter(Data.Source %in% j) %>% select(-dup.portal)
    y <- x %>% select(-Data.Source, -index)
    dup <- duplicated(y) | duplicated(y, fromLast = TRUE)
    dup[x$Data.Source != i] <- FALSE
    d$dup.portal[d$index %in% x$index[dup]] <- TRUE
  }
  d$index <- NULL
  #' Remove portal records that are duplicates of original sources
  dat <- dat %>%
    mutate(Lon = round(Longitude, 2),
           Lat = round(Latitude, 2)) %>%
    left_join(d, by = c('Data.Source', 'Date', 'Species', 'Lon', 'Lat')) %>%
    filter(!dup.portal) %>%
    select(-Lon, -Lat, -dup.portal)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Scan for duplicates using field subset
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' Scan first within sample events, then across sample events. There should be
  #' few, if any, duplicates found here.
  dupFields <- c(
    'Sample.Event', 'Species', 'Maturity', 'Life.Stage', 'Copepodite.Stage', 'Sex',
    'Longitude', 'Latitude', 'Depth', 'Date', 'Time', 'Measurement.Value',
    'Measurement.Unit') #' fields to test for duplicate rows
  exDupFields <- c('Depth.Top', 'Depth.Bottom', 'Sample.Gear', 'Net.Mesh.Size',
                   'Net.Mouth.Area', 'Cruise.Report') #' extra fields that may also be included, and used to scrap metadata
  dupFields2 <- unique(c(dupFields, exDupFields))
  
  d <- dat %>%
    select(all_of(dupFields2))
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}  
  d[is.na(d)] <- -Inf
  dup <- duplicated(d)
  dat <- dat[!dup,]
  rm(d, dup)
  
  #' And again, across sampling events
  dupFields <- c(
    'Species', 'Maturity', 'Life.Stage', 'Copepodite.Stage', 'Sex', 'Longitude',
    'Latitude', 'Depth', 'Date', 'Time', 'Measurement.Value', 'Measurement.Unit')
  exDupFields <- c('Depth.Top', 'Depth.Bottom', 'Sample.Gear', 'Net.Mesh.Size',
                   'Net.Mouth.Area', 'Cruise.Report')
  dupFields2 <- unique(c(dupFields, exDupFields))
  
  d <- dat %>%
    select(all_of(dupFields2))
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}  
  d[is.na(d)] <- -Inf
  dup <- duplicated(d)
  dat <- dat[!dup,]
  rm(d, dup)
  
  
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' Remove duplicates from portal data
  #' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #' OBIS and GBIF share many records, but they report copepodite stage
  #' differently, which is a problem that needs resolved. GBIF seems to only
  #' record records as C6 or unspecified whereas OBIS records other distinct
  #' stages. This mismatch leads to duplicated records. There are also duplicates
  #' within each data portal where the same measurements were reported multiple
  #' times with different values of copepodite stage and other metadata.
  #' Scan through all of the sample events to identify duplicates between the
  #' portals, and for each sample event record as much meta data as available.
  #' Preferentially select OBIS records because GBIF did not report juvenile
  #' copepodite stages.
  
  dat$index <- 1:nrow(dat) #' index to track which duplicate rows to remove
  dat$dup <- FALSE
  
  #' Create a temporary `Event` variable to identify further duplicates. Unlike
  #' the existing `Sample.Event` field, records from the portals may share the
  #' same Event.
  eventFields <- c('Longitude', 'Latitude', 'Date', 'Time') #' columns determining sample event for portals
  eventFields_ <- c('Lon', 'Lat', 'Date', 'Time.round') #' round coords and times
  
  d <- dat %>%
    filter(Data.Source %in% portals) %>%
    mutate(Lon = round(Longitude, 2),
           Lat = round(Latitude, 2),
           Time.round = round.time(Time)) %>%
    select(-Sample.Event, -Longitude, -Latitude, -Longitude.Start, -Longitude.End,
           -Latitude.Start, -Latitude.Start, -Time, -dup)
  
  d_ <- d %>%
    select(all_of(eventFields_)) %>%
    distinct()
  n.ev <- nrow(d_)
  d_$Event <- 1:n.ev
  d <- left_join(d, d_, by = eventFields_)
  rm(d_)
  
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}
  
  # table(d[c('Data.Source','Copepodite.Stage')])
  cs <- sapply(portals, function(z){
    unique(d$Copepodite.Stage[d$Data.Source == z & !is.na(d$Copepodite.Stage)])})
  
  prefer <- 'OBIS'
  d$Data.Source <- factor(d$Data.Source, c(portals[portals == prefer],
                                           portals[portals != prefer]) )
  d <- d[order(d$Event, d$Data.Source),]
  d$Data.Source <- as.character(d$Data.Source)
  
  dupFields <- c('Species', 'Maturity', 'Life.Stage', 'Copepodite.Stage', 'Sex',
                 'Measurement', 'Measurement.Value')
  exDupFields <- c('Depth.Top', 'Depth.Bottom', 'Sample.Gear',
                   'Net.Mesh.Size', 'Net.Mouth.Area', 'Cruise.Report')
  dupFields2 <- unique(c(dupFields, exDupFields))
  
  for(ii in 1:n.ev){
    #' ~~~~~~~~~~~~~~~~~~~
    #' Print loop progress
    if(ii == 1) message('\n', '-- resolving data-portal duplicates:')
    s <- seq(0.1, 1, 0.1)
    p <- ii/n.ev <= s & s < {ii+1}/n.ev
    if(p[1]) cat(paste0(' ', s[p]*100, '%'))
    if(any(p[-1])) cat(paste0(', ', s[p]*100, '%'))
    #' ~~~~~~~~~~~~~~~~~~~
    x.e <- d %>% filter(Event == ii) #' all portal data for event ii
    skip <- nrow(x.e) <= 1
    if(skip) next
    gr <- x.e %>% select(Species, Depth, Measurement) %>% distinct()
    for(j in 1:nrow(gr)){
      x.j <- x.e %>%
        filter(Species == gr$Species[j], Depth == gr$Depth[j], Measurement == gr$Measurement[j])
      if(nrow(x.j) == 1) next
      x <- x.j %>% select(all_of(dupFields))
      dup <- duplicated(x) #' find the duplicates
      x2 <- x.j %>% select(all_of(exDupFields)) %>% distinct() #' find meta data to infill
      meta.infill <- rep(FALSE, length(exDupFields))
      for(g in seq_along(exDupFields)){
        infill <- FALSE
        z <- x2[,g]
        if(is.numeric(z)){
          notna <- !is.na(z)
          infill <- any(notna) & any(!notna)
          if(infill) x.j[,exDupFields[g]] <- unique(z[notna])
        }else{
          known <- !{is.na(z) | grepl('unspecified', z) | grepl('unknown', z)}
          infill <- any(known) & any(!known)
          if(infill) x.j[,exDupFields[g]] <- unique(z[known])
        }
        meta.infill[g] <- infill
      }
      if(any(dup)) dat$dup[dat$index %in% x.j$index[dup]] <- TRUE #' flag the duplicates
      if(any(meta.infill)) dat[dat$index %in% x.j$index, exDupFields[meta.infill]] <- x.j[,exDupFields[meta.infill]] #' infill meta data
    }
  }
  
  #' Remove the duplicates
  dat <- dat %>%
    filter(!dup) %>%
    select(-index, -dup)
  
  
  #' Now that extra info has been scraped from portal data, remove any more
  #' duplicates that may have appeared within the portal sources and over the
  #' whole data set.
  d <- dat %>%
    mutate(Lon = round(Longitude, 2),
           Lat = round(Latitude, 2),
           Time.round = round.time(Time))
  for(i in 1:ncol(d)){
    if(is.factor(d[,i])) d[,i] <- as.character(d[,i])}
  d[is.na(d)] <- -Inf
  d$dup <- FALSE
  for(i in portals){
    j <- d$Data.Source == i
    if(!any(j)) next
    x <- d %>% filter(Data.Source == i)
    x <- x %>% select(-Longitude, -Latitude, -Longitude.Start, -Latitude.Start,
                      Longitude.End, -Latitude.End, -Time)
    dup <- duplicated(x)
    d$dup[j] <- dup
  }
  dat$dup <- d$dup
  rm(d)
  dat <- dat %>% filter(!dup) %>% select(-dup)
  
  
  #' Final sweep over whole data set
  d <- dat %>%
    select(-Data.Source, -Sample.Event)
  dup <- duplicated(d)
  rm(d)
  dat <- dat[!dup,]
  
  #' Sort the data
  dat <- dat[order(dat$Date, dat$Time, dat$Depth, dat$Species, dat$Maturity,
                   dat$Copepodite.Stage, dat$Sex),]
  
  #' Reformat the Sample.Event field
  dat$Event <- dat$Sample.Event
  dat$Sample.Event <- NA
  x <- dat[c('Data.Source', 'Event')] %>% distinct()
  x$Sample.Event <- 1:nrow(x)
  for(i in unique(dat$Data.Source)){
    j <- dat$Data.Source == i
    xi <- x[x$Data.Source == i,]
    xi <- setNames(xi$Sample.Event, xi$Event)
    s <- dat$Event[j]
    snew <- unname(xi[as.character(s)])
    dat$Sample.Event[j] <- snew
  }
  dat$Event <- NULL
  rm(x, i, j, xi, s, snew)
  
  

  # Tidy before saving ------------------------------------------------------

  #' Set all factors/dates to character
  for(i in 1:ncol(dat)){
    if(is.factor(dat[,i]) | class(dat[,i]) == 'Date'){
      dat[,i] <- as.character(dat[,i])}}
  
  #' Swap NA values for '' where appropriate (characters but not factors)
  dat$Time[is.na(dat$Time)] <- ''
  dat$Time.Flag[is.na(dat$Time.Flag)] <- ''
  
  x <- dat$Data.Source
  # any(is.na(x))
  # unique(x)
  x <- gsub('\\.', ' ', x)
  x <- gsub('_', ' ', x)
  x <- gsub('1955 1957', '1955-1957', x)
  x <- gsub('Guang Yang', 'CHINARE', x)
  # unique(x)
  dat$Data.Source <- x
  
  #' Swap periods for white space in the field names
  x <- names(dat)
  x <- gsub('\\.', ' ', x)
  names(dat) <- x
  
  # Save --------------------------------------------------------------------

  data.size <- format(object.size(dat), 'Mb')
  message('\n', 'Compiled data size = ', paste0(data.size,'.'))
  
  if(save.compiled.data){
    s <- 'all species'
    if(!all(species.selection %in% c('all','All','ALL'))) s <- species.selection
    if(all(s == 'copepods')) s <- 'all copepods'
    if(length(s) > 1){
      s <- strsplit(s, ' ')
      s <- sapply(s, function(z) paste(substr(z[1],1,1), z[length(z)], sep = ' '))
    }
    f <- paste0(paste('compiled data', paste(s, collapse = '_'), sep = '_'), '.csv.gz')
    p <- file.path(dir.data.zoo, 'compiled', 'copepoda')
    if(!dir.exists(p)) dir.create(p, recursive = TRUE)
    p <- file.path(p, f)
    message('\nSaving compiled data: ', p)
    write.csv(dat, gzfile(p), row.names = FALSE)
  }
  
  rm(list = c('compile.data'), envir = .GlobalEnv)

  return(invisible(NULL))
  
}

