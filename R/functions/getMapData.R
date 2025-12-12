# Get southern hemisphere map data

getMapData <- function(
    dataDirectory,
    fileNames = list(coastline = 'ne_10m_coastline.shp',
                     land = 'ne_10m_land.shp',
                     ocean = 'ne_10m_ocean.shp',
                     iceshelf = 'ne_10m_antarctic_ice_shelves_polys.shp'),
    lon_lim = c(-180, 180), lat_lim = c(-90, 90), hemisphere = NULL, crs = NULL,
    res = NULL, createGrid = FALSE, grid.border.ice.or.land = 'ice',
    removeFrac = 0.05, returnPlot = FALSE, verbose = FALSE,
    loadFromFile = FALSE, autoSave = TRUE, map_colour = NULL, map_linewidths = NULL
){
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load and return map data as a spatial data frame.
  # Default arguments return entire southern hemisphere.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Load packages & set directories -----------------------------------------
  # Error handle inappropriate arguments
  if(!is.null(hemisphere) & !{hemisphere %in% c('north','south')})
    error("If hemisphere is specified then it must be either 'north' or 'south'")
  
  # Required packages
  library(sf)
  library(sp)
  
  # Specify directories of stored shape files
  if(!dir.exists(dataDirectory)) stop(paste0('Specified dataDirectory (', dataDirectory, ') does not exist!'))
  subDirectories <- basename(list.dirs(dataDirectory))
  subDirectories <- subDirectories[subDirectories != basename(dataDirectory)]
  anySubDirectories <- length(subDirectories) > 0
  if(!anySubDirectories){
    coastlineFile <- paste(dataDirectory, fileNames$coastline, sep = '/')
    landFile      <- paste(dataDirectory, fileNames$land,      sep = '/')
    oceanFile     <- paste(dataDirectory, fileNames$ocean,     sep = '/')
    iceshelfFile  <- paste(dataDirectory, fileNames$iceshelf,  sep = '/')
  }else{
    coastDir <- subDirectories[grep('coast', subDirectories)]
    landDir  <- subDirectories[grep('land', subDirectories)]
    oceanDir <- subDirectories[grep('ocean', subDirectories)]
    iceDir   <- subDirectories[grep('ice', subDirectories)]
    coastlineFile <- paste(dataDirectory, coastDir, fileNames$coastline, sep = '/')
    landFile      <- paste(dataDirectory, landDir,  fileNames$land,      sep = '/')
    oceanFile     <- paste(dataDirectory, oceanDir, fileNames$ocean,     sep = '/')
    iceshelfFile  <- paste(dataDirectory, iceDir,   fileNames$iceshelf,  sep = '/')
  }
  
  # Processed map data is saved into 'temp' directory within the current working directory
  filePath <- paste(getwd(), 'temp', sep = '/')
  if(!filePath %in% list.dirs(getwd())) dir.create(filePath)
  fileName_map <- paste('map data',
                        paste('lon', paste(lon_lim, collapse = ' ')),
                        paste('lat', paste(lat_lim, collapse = ' ')), sep = '_')
  fullFile_map <- paste0(paste(filePath, fileName_map, sep = '/'), '.rds')

  fn <- paste(paste('lon', paste(lon_lim, collapse = ' ')),
          paste('lat', paste(lat_lim, collapse = ' ')),
          paste('res', paste(res, collapse = ' ')))
  fileName_grid.2ice <- paste('map grid ice border', fn, sep = '_')
  fileName_grid.2land <- paste('map grid land border', fn, sep = '_')
  fullFile_grid.2ice <- paste0(paste(filePath, fileName_grid.2ice, sep = '/'), '.rds')
  fullFile_grid.2land <- paste0(paste(filePath, fileName_grid.2land, sep = '/'), '.rds')
  
  # Load compiled map from file ---------------------------------------------
  # Load from file to save running this script
  mapIsLoaded <- FALSE
  gridIsLoaded <- FALSE
  if(loadFromFile){
    if(file.exists(fullFile_map)){
      dat <- readRDS(fullFile_map)
      crs <- st_crs(dat)
      crs_base <- st_crs(st_read(coastlineFile, quiet = !verbose))
      mapIsLoaded <- TRUE}
    if(file.exists(fullFile_grid.2ice)) map_grid.2ice <- readRDS(fullFile_grid.2ice)
    if(file.exists(fullFile_grid.2land)) map_grid.2land <- readRDS(fullFile_grid.2land)
    gridIsLoaded <- exists('map_grid.2ice') & exists('map_grid.2land')
  }
  
  # Create map --------------------------------------------------------------
  if(!mapIsLoaded){
    # Load the data
    coastline <- st_read(coastlineFile, quiet = !verbose)
    land      <- st_read(landFile,      quiet = !verbose)
    iceshelf  <- st_read(iceshelfFile,  quiet = !verbose)
    
    coastline <- coastline[!is.na(coastline$featurecla),]
    land <- land[!is.na(land$featurecla),]
    iceshelf <- iceshelf[!is.na(iceshelf$featurecla),]
    
    # Merge data into single data frame
    land      <- land[c('featurecla')]
    iceshelf  <- iceshelf[c('featurecla')]
    coastline <- coastline[c('featurecla')]
    land$featurecla      <- 'land'
    iceshelf$featurecla  <- 'ice'
    coastline$featurecla <- 'coastline'
    dat <- rbind(land, iceshelf, coastline); rm(land,iceshelf,coastline)
    names(dat)[1] <- 'feature'
    
    dat <- st_make_valid(dat)
    
    # Set the coordinate reference system (CRS)
    crs_base <- st_crs(dat)
    if(is.null(crs)){
      if(!is.null(hemisphere) && hemisphere == 'south') crs <- 6932
    }
    if(is.numeric(crs)) crs <- st_crs(crs)
    
    # Create map bounding line
    londiff <- diff(lon_lim)
    latdiff <- diff(lat_lim)
    londiff10 <- 10 * londiff
    latdiff10 <- 10 * latdiff
    if(londiff == 360){
      # circumpolar map
      mapLimit_line <- matrix(c(seq(lon_lim[1], lon_lim[2], length.out = londiff10),
                                rep(lat_lim[2], londiff10)),
                              ncol = 2, dimnames = list(NULL, c('Longitude','Latitude')))
    }else{
      mapLimit_line <- matrix(c(c(seq(lon_lim[1], lon_lim[2], length.out = londiff10),
                                  rep(lon_lim[2], latdiff10),
                                  seq(lon_lim[2], lon_lim[1], length.out = londiff10),
                                  rep(lon_lim[1], latdiff10)),
                                c(rep(lat_lim[1], londiff10),
                                  seq(lat_lim[1], lat_lim[2], length.out = latdiff10),
                                  rep(lat_lim[2], londiff10),
                                  seq(lat_lim[2], lat_lim[1], length.out = latdiff10))
      ),
      ncol = 2, dimnames = list(NULL, c('Longitude','Latitude')))
    }
    
    
    mapLimit_line <- st_linestring(mapLimit_line, dim = 'XY')
    mapLimit_line <- st_sf(st_as_sfc(list(mapLimit_line), crs = crs_base))
    st_geometry(mapLimit_line) <- 'geometry'
    mapLimit_line <- st_transform(mapLimit_line, crs = crs) # transform to map coordinates
    mapLimit_poly <- st_cast(mapLimit_line, 'POLYGON')
    
    # Change to polar coords
    dat <- st_transform(dat, crs = crs)
    # Crop map data to within boundary
    crop.map <- function(x,y,verbose=FALSE) if(!verbose) suppressWarnings(st_intersection(x, y)) else st_intersection(x, y)
    dat <- crop.map(mapLimit_poly, dat, verbose)
    # ggplot() + geom_sf(data = subset(dat, feature == 'land'), fill = 'darkgreen') + geom_sf(data = subset(dat, feature == 'ice'), fill = 'lightblue') + geom_sf(data = subset(dat, feature == 'coastline'), colour = 'black')
    
    # Include map boundary line in dat
    mapLimit_line$feature <- 'boundary'
    mapLimit_line <- mapLimit_line[c('feature', 'geometry')]
    dat <- rbind(dat, mapLimit_line)
    
    rm(mapLimit_line)

    # Create an ocean polygon. First, one that borders ice shelves, then another
    # that borders land to exclude ice from the map.

    # It's tricky to eliminate narrow edges between adjacent polygons...
    # The land and ice polygons are not perfectly aligned so ocean can creep into
    # the gaps. Solve this by expanding the coastline points into tiny polygons then
    # merging all polygons to mask all dry regions.
    coastline <- subset(dat, feature == 'coastline')
    coastline <- lapply(1:nrow(coastline), function(z) suppressWarnings(st_cast(coastline[z,], 'LINESTRING')))
    coastline <- do.call('rbind', coastline)
    xy <- st_coordinates(coastline)
    if(!verbose){
      coastline <- as.data.frame(suppressWarnings(st_cast(coastline, 'POINT')))}else{
        coastline <- as.data.frame(st_cast(coastline, 'POINT'))}
    coastline <- coastline[,names(coastline) != 'geometry', drop = FALSE]
    bb <- st_bbox(dat)
    x <- outer(xy[,'X'], c(-1,1) * diff(bb[c('xmin','xmax')]) * 1e-3, '+')
    y <- outer(xy[,'Y'], c(-1,1) * diff(bb[c('ymin','ymax')]) * 1e-3, '+')
    nxy <- nrow(xy)
    ids <- rownames(coastline)
    xy <- lapply(1:nxy, function(z){
      Polygons(list(
        Polygon(matrix(c(x[z,1], y[z,1],
                         x[z,2], y[z,1],
                         x[z,2], y[z,2],
                         x[z,1], y[z,2],
                         x[z,1], y[z,1]), ncol = 2, byrow = TRUE))), ids[z])
    })
    xy <- SpatialPolygons(xy)
    xy <- SpatialPolygonsDataFrame(xy, data = coastline)
    xy <- st_as_sf(xy)
    # xy$feature <- paste0(xy$feature, '.pol')
    st_crs(xy) <- crs

    dat_ <- dat[dat$feature != 'coastline',]
    ocean.2ice <- st_difference(mapLimit_poly, st_make_valid(st_union(rbind(dat_, xy)))); rm(dat_)
    ocean.2ice$feature <- 'ocean.bordersIce'
    ocean.2ice <- ocean.2ice[c('feature','geometry')]
    dat <- rbind(dat, ocean.2ice); rm(ocean.2ice)
    
    # ggplot() + geom_sf(data = subset(dat, feature == 'ocean.bordersIce'), fill = 'blue') + geom_sf(data = subset(dat, feature == 'land'), fill = 'darkgreen') + geom_sf(data = subset(dat, feature == 'ice'), fill = 'lightblue') + geom_sf(data = subset(dat, feature == 'coastline'), colour = 'black') + geom_sf(data = subset(dat, feature == 'boundary'), colour = 'black', linewidth = 1)
    
    dat_ <- dat[!{dat$feature %in% c('coastline', 'ice', 'ocean.bordersIce')},]
    ocean.2land <- st_difference(mapLimit_poly, st_make_valid(st_union(rbind(dat_, xy)))); rm(dat_)
    ocean.2land$feature <- 'ocean.bordersLand'
    ocean.2land <- ocean.2land[c('feature','geometry')]
    dat <- rbind(dat, ocean.2land); rm(ocean.2land)
    
    # ggplot() + geom_sf(data = subset(dat, feature == 'ocean.bordersLand'), fill = 'blue') + geom_sf(data = subset(dat, feature == 'land'), fill = 'darkgreen') + geom_sf(data = subset(dat, feature == 'coastline'), colour = 'black') + geom_sf(data = subset(dat, feature == 'boundary'), colour = 'black', linewidth = 1)
    
  }
  
  # Create grid -------------------------------------------------------------
  # Define map grid using resolution 'res'
  made_grid <- FALSE
  if(!gridIsLoaded){
    if(createGrid){
      if(is.null(res)){
        warning("Must specify 'res' to create grid.")}else{
          made_grid <- TRUE
          bb <- matrix(c(lon_lim[1], lat_lim[1],
                         lon_lim[2], lat_lim[1],
                         lon_lim[2], lat_lim[2],
                         lon_lim[1], lat_lim[2],
                         lon_lim[1], lat_lim[1]
          ), ncol = 2, byrow = TRUE)
          bb <- st_linestring(bb, dim = 'XY')
          bb <- st_sf(st_as_sfc(list(bb), crs = crs_base))
          st_geometry(bb) <- 'geometry'
          bb <- st_cast(bb, 'POLYGON')
          map_grid <- st_make_grid(bb, cellsize = res)
          map_grid <- st_sf(map_grid)
          st_geometry(map_grid) <- 'geometry'
          # Get grid cell mid points (not centroids, but midway between lat-lon bounds)
          get.centroids <- function(x, verbose=FALSE) if(!verbose) suppressWarnings(st_centroid(x)) else st_centroid(x)
          cell_centroids <- get.centroids(map_grid, verbose)
          centroid_coords <- st_coordinates(cell_centroids)
          lon_mids <- seq(lon_lim[1] + 0.5 * res['lon'], lon_lim[2] - 0.5 * res['lon'], res['lon'])
          map_grid$mid_lon_box <- lon_mids[apply(outer(centroid_coords[,'X'], lon_mids, FUN = function(x,y) abs(x-y)), 1, which.min)]
          lat_mids <- seq(lat_lim[1] + 0.5 * res['lat'], lat_lim[2] - 0.5 * res['lat'], res['lat'])
          map_grid$mid_lat_box <- lat_mids[apply(outer(centroid_coords[,'Y'], lat_mids, FUN = function(x,y) abs(x-y)), 1, which.min)]
          cell_centroids <- st_transform(cell_centroids, crs)
          centroid_coords <- st_coordinates(cell_centroids)
          include.centroids <- function(x,y,type='lat_lon'){
            colnames(y) <- rbind(c('centroid_lon_true','centroid_lat_true'), c('centroid_x_true','centroid_y_true'), c('centroid_x_box','centroid_y_box'))[rep(type, 3) == c('lat_lon','xy','xy.box'),]
            cbind(x, as.data.frame(y))}
          map_grid <- include.centroids(map_grid, centroid_coords, type = 'xy.box')
          map_grid <- st_transform(map_grid, crs = crs) # grid covers entire map domain
          
          map_grid <- st_make_valid(map_grid)
          
          # Adjust grid to remove (portions of) cells masked by land/ice
          grid.adjust <- function(x,y,verbose=FALSE) if(!verbose) suppressWarnings(st_difference(x, y)) else st_difference(x, y)
          mask.2ice <- grid.adjust(mapLimit_poly, subset(dat, feature == 'ocean.bordersIce'), verbose)
          mask.2land <- grid.adjust(mapLimit_poly, subset(dat, feature == 'ocean.bordersLand'), verbose)
          map_grid.2ice <- grid.adjust(map_grid, mask.2ice, verbose)
          map_grid.2land <- grid.adjust(map_grid, mask.2land, verbose)
          
          # Get true grid cell centroids (post cropping) and grid cell areas
          cell_centroids.2ice <- get.centroids(map_grid.2ice, verbose)
          cell_centroids.2land <- get.centroids(map_grid.2land, verbose)
          centroid_coords.2ice <- st_coordinates(cell_centroids.2ice)
          centroid_coords.2land <- st_coordinates(cell_centroids.2land)
          map_grid.2ice <- include.centroids(map_grid.2ice, centroid_coords.2ice, type = 'xy')
          map_grid.2land <- include.centroids(map_grid.2land, centroid_coords.2land, type = 'xy')
          # Convert CRS to standard map projection, and convert geometry to polygons
          map_grid.2ice <- st_transform(map_grid.2ice, crs = crs_base)
          map_grid.2land <- st_transform(map_grid.2land, crs = crs_base)
          cast.grid.2.polygon <- function(x){
            y <- lapply(1:nrow(x), function(z){
              if(class(x$geometry[z][[1]])[2] == 'POLYGON') x[z,] else{
                suppressWarnings(st_cast(x[z,], 'POLYGON'))}})
            do.call('rbind', y)}
          # map_grid.2ice <- cast.grid.2.polygon(map_grid.2ice)
          # map_grid.2land <- cast.grid.2.polygon(map_grid.2land)
          # map_grid.2ice <- st_make_valid(map_grid.2ice)
          # map_grid.2land_ <- st_make_valid(map_grid.2land)
          # Get cell centroids in standard lat-lon coordinates
          cell_centroids.2ice <- get.centroids(map_grid.2ice, verbose)
          cell_centroids.2land <- get.centroids(map_grid.2land, verbose)
          centroid_coords.2ice <- st_coordinates(cell_centroids.2ice)
          centroid_coords.2land <- st_coordinates(cell_centroids.2land)
          map_grid.2ice <- include.centroids(map_grid.2ice, centroid_coords.2ice, type = 'lat_lon')
          map_grid.2land <- include.centroids(map_grid.2land, centroid_coords.2land, type = 'lat_lon')
          cell_areas.2ice <- st_area(map_grid.2ice)
          cell_areas.2land <- st_area(map_grid.2land)
          m2.to.km2 <- function(x) if(all(attr(x, 'units')$numerator == 'm')) x * 1e-6 else {x; warning('Units were not metres as expected!')}
          cell_areas.2ice <- m2.to.km2(cell_areas.2ice)
          cell_areas.2land <- m2.to.km2(cell_areas.2land)
          
          map_grid.2ice$area_km2 <- as.numeric(cell_areas.2ice)
          map_grid.2land$area_km2 <- as.numeric(cell_areas.2land)
          map_grid.2ice <- st_transform(map_grid.2ice, crs = crs)
          map_grid.2land <- st_transform(map_grid.2land, crs = crs)
          
          # Omit tiny cells that may appear within intricate coastline. Remove cells that
          # are smaller than 0<removeFrac<1 of their expected (uncropped) area.
          lat_include.2ice <- lat_mids %in% map_grid.2ice$mid_lat_box[map_grid.2ice$feature == 'ocean.bordersIce']
          lat_include.2land <- lat_mids %in% map_grid.2land$mid_lat_box[map_grid.2land$feature == 'ocean.bordersLand']
          # lat_expected <- seq(lat_lim[1] + 0.5 * res['lat'], lat_lim[2] - 0.5 * res['lat'], res['lat'])
          # # There are (were: centroids no-longer used) computational errors that creep in to map transforms, so the mapped latitudes are not exactly as expected.
          # lat_diff <- abs(outer(map_grid$mid_lat_box, lat_expected, '-'))
          # lat_include <- apply(lat_diff, 2, function(z) any(z < 0.5 * res['lat']))
          # lat_diff <- lat_diff[,lat_include]

          j <- list(sum(lat_include.2ice), sum(lat_include.2land)) # number of expected areas to calculate (if cell area varied only with latitude)
          y_ <- list(lat_mids[lat_include.2ice], lat_mids[lat_include.2land])
          x_ <- lapply(1:2, function(z) rep(0, length(y_[[z]])))
          x <- lapply(1:2, function(z) outer(x_[[z]], 0.5 * c(-1,1) * res['lon'], '+'))
          y <- lapply(1:2, function(z) outer(y_[[z]], 0.5 * c(-1,1) * res['lat'], '+'))
          pol <- lapply(1:2, function(k){
            lapply(1:j[[k]], function(z){
              X<-x[[k]]
              Y<-y[[k]]
              Polygons(list(Polygon(matrix(c(X[z,1],Y[z,1],
                                             X[z,2],Y[z,1],
                                             X[z,2],Y[z,2],
                                             X[z,1],Y[z,2],
                                             X[z,1],Y[z,1]), ncol = 2, byrow = TRUE))), as.character(z))
            })
          })
          pol <- lapply(1:2, function(k) SpatialPolygons(pol[[k]]))
          pol <- lapply(1:2, function(k) SpatialPolygonsDataFrame(pol[[k]], data = data.frame(pol = seq_along(pol[[k]]))))
          pol <- lapply(1:2, function(k) st_as_sf(pol[[k]]))
          for(k in 1:length(pol)) st_crs(pol[[k]]) <- crs_base
          a <- lapply(1:2, function(k) st_area(pol[[k]]))
          a <- lapply(1:2, function(k) as.numeric(m2.to.km2(a[[k]]))) # expected areas

          j.ice <- apply(outer(map_grid.2ice$mid_lat_box, lat_mids[lat_include.2ice], '=='), 1, which)
          j.land <- apply(outer(map_grid.2land$mid_lat_box, lat_mids[lat_include.2land], '=='), 1, which)
          # remove <- map_grid$area_km2 / a[j] < removeFrac
          remove.ice <- map_grid.2ice$area_km2 / a[[1]][j.ice] < removeFrac
          remove.land <- map_grid.2land$area_km2 / a[[2]][j.land] < removeFrac
          map_grid.2ice <- map_grid.2ice[!remove.ice,]
          map_grid.2land <- map_grid.2land[!remove.land,]

          # Put grid cells in order
          rowOrder <- function(x) order(x$mid_lat_box, x$mid_lon_box, decreasing = FALSE)
          colOrder <- c('feature','mid_lon_box','mid_lat_box','centroid_x_box','centroid_y_box', 'centroid_lon_true','centroid_lat_true','centroid_x_true','centroid_y_true','area_km2','geometry')
          map_grid.2ice <- map_grid.2ice[rowOrder(map_grid.2ice),colOrder]
          map_grid.2land <- map_grid.2land[rowOrder(map_grid.2land),colOrder]

          # ggplot() + geom_sf(data = subset(dat, feature == 'ocean.bordersIce'), fill = 'skyblue') + geom_sf(data = st_cast(map_grid.2ice, 'LINESTRING'), linewidth = 0.1, colour = 'red') + geom_sf(data = subset(dat, feature == 'land'), fill = 'darkgreen') + geom_sf(data = subset(dat, feature == 'ice'), fill = 'white') + geom_sf(data = subset(dat, feature == 'coastline'), colour = 'black') + geom_sf(data = subset(dat, feature == 'boundary'), colour = 'black', linewidth = 1)
          # ggplot() + geom_sf(data = subset(dat, feature == 'ocean.bordersLand'), fill = 'skyblue') + geom_sf(data = st_cast(map_grid.2land, 'LINESTRING'), linewidth = 0.1, colour = 'red') + geom_sf(data = subset(dat, feature == 'land'), fill = 'darkgreen') + geom_sf(data = subset(dat, feature == 'coastline'), colour = 'black') + geom_sf(data = subset(dat, feature == 'boundary'), colour = 'black', linewidth = 1)
        }
    }
  }
  
  # Store output ------------------------------------------------------------
  output <- switch(grid.border.ice.or.land,
                   ice = {
                     x <- list()
                     x$map_data <- dat[!{dat$feature %in% c('ocean.bordersLand')},]
                     x$map_data$feature[grepl('ocean', x$map_data$feature)] <- 'ocean'
                     if(createGrid){
                       x$map_grid <- map_grid.2ice
                       x$map_grid$feature[grepl('ocean', x$map_grid$feature)] <- 'ocean'
                       x$grid_res <- res}
                     x
                   },
                   land = {
                     x <- list()
                     x$map_data <- dat[!{dat$feature %in% c('ocean.bordersIce', 'ice')},]
                     x$map_data$feature[grepl('ocean', x$map_data$feature)] <- 'ocean'
                     if(createGrid){
                       x$map_grid <- map_grid.2land
                       x$map_grid$feature[grepl('ocean', x$map_grid$feature)] <- 'ocean'
                       x$grid_res <- res}
                     x
                   },
                   both = {
                     x <- list()
                     x$map_data <- dat
                     if(createGrid){
                       x$map_grid.borders_ice <- subset(map_grid.2ice, feature != 'ocean.bordersLand')
                       x$map_grid.borders_land <- subset(map_grid.2land, feature != 'ocean.bordersIce')
                       x$grid_res <- res}
                     x
                   }
  )
  
  output$crs <- crs
  output$crs_base <- crs_base
  output$lon_lim <- lon_lim
  output$lat_lim <- lat_lim
  
  # Plots -------------------------------------------------------------------
  if(returnPlot){
    if(is.null(map_colour)){
      map_colour <- c(land = 'lightgrey', ice = 'skyblue', ocean = 'white',
                      coastline = 'black', boundary = 'black')}
    if(is.null(map_linewidths)){
      map_linewidths = c(coastline = 0.1, boundary = 1, grid = 0.1)}

    # if(exists('plotMap', .GlobalEnv)){
    if(exists('plotMap')){
        output$map_plot <- plotMap(dat,map_colour = map_colour,
                                 map_linewidths = map_linewidths)
      output$map_plot_no_ice <- plotMap(subset(dat, feature != 'ice'),
                                        map_colour = map_colour,
                                        map_linewidths = map_linewidths)
      if(createGrid){
        output$map_plot_gridded <- plotMap(
          dat, map_colour = map_colour, map_linewidths = map_linewidths,
          gridData = map_grid.2ice, overlayGrid = TRUE)
        output$map_plot_gridded_no_ice <- plotMap(
          subset(dat, feature != 'ice'), map_colour = map_colour,
          map_linewidths = map_linewidths, gridData = map_grid.2land,
          overlayGrid = TRUE)
      }
      if(grid.border.ice.or.land == 'ice'){
        output$map_plot_no_ice <- NULL
        output$map_plot_gridded_no_ice <- NULL
      }
      if(grid.border.ice.or.land == 'land'){
        output$map_plot <- output$map_plot_no_ice
        output$map_plot_no_ice <- NULL
        output$map_plot_gridded <- output$map_plot_gridded_no_ice
        output$map_plot_gridded_no_ice <- NULL
      }
    }else{warning("Cannot create map plot because function 'plotMap' is not loaded into the global environment")}
  }
  
  # Save --------------------------------------------------------------------
  if(autoSave){
    # Save map data into 'temp' directory within current directory
    if(!mapIsLoaded) saveRDS(dat, fullFile_map)
    # Save the grid
    if(made_grid){
      if(!gridIsLoaded){
        saveRDS(map_grid.2ice, fullFile_grid.2ice)
        saveRDS(map_grid.2land, fullFile_grid.2land)
      }
    }
  }
  
  # End ---------------------------------------------------------------------
  return(output)
}

