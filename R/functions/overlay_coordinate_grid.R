overlay_coordinate_grid <- function(
    plt, latlim = NULL, lat_increments = NULL, lat_step = 10,
    lat_step_minor = 0.5 * lat_step, lon_vals = NULL, singleLatAxis = FALSE,
    textSize = 4, lineWidth = 1, minorTicks = TRUE,
    crs.base = NULL, crs.use = NULL, BBox = NULL, colour = 'black'){
  # Round latlim to nearest lat_step
  if(!is.null(latlim))
    latlim[2] <- ceiling(latlim[2] / lat_step_minor) * lat_step_minor
  # Set default lat/lon values for axes
  if(is.null(latlim)) latlim <- c(-90, -50)
  if(is.null(lon_vals)) lon_vals <- c(0, 90, 180, 270)
  if(is.null(lat_increments))
    lat_increments <- seq(latlim[1], latlim[2], lat_step)
  if(minorTicks)
    lat_increments_minor <- seq(latlim[1] + lat_step_minor, latlim[2], lat_step)
  #~~~~~~~~~
  # Latitude
  #~~~~~~~~~
  # axis lines
  if(singleLatAxis) lons <- 0 else lons <- lon_vals
  nlon <- length(lons)
  lat_lines <- matrix(c(rep(lons, each = 2), rep(latlim,4)), ncol = 2)
  lat_lines <- st_linestring(x = lat_lines, dim = 'XY')
  lat_lines <- st_sfc(lat_lines, crs = crs.base)
  lat_lines <- st_transform(lat_lines, crs.use)
  plt <- plt + geom_sf(data = lat_lines, linewidth = lineWidth, colour = colour) # axis line
  # axis ticks
  nlat <- length(lat_increments)
  lr <- 1 / {{lat_increments - min(lat_increments)} / 
      diff(range(lat_increments))}
  lr[1] <- 0
  lon_width0 <- 1.4
  lon_width <- lr * lon_width0
  gridPos <- vector('list', length = nlon * nlat)
  for(w in 1:nlon){
    lon <- lons[w]
    for(z in 1:nlat){
      u <- {w-1} * nlat + z
      gridPos[[u]] <- matrix(c(lon + c(-1,1) * lon_width[z],
                               rep(lat_increments[z], 2)), ncol = 2)
    }}
  gridPos <- st_multilinestring(x = gridPos, dim = 'XY')
  gridPos <- st_sfc(gridPos, crs = crs.base)
  gridPos <- st_transform(gridPos, crs = crs.use)
  plt <- plt + geom_sf(data = gridPos, linewidth = lineWidth, colour = colour)
  # Repeat for minor ticks
  if(minorTicks){
    nlat_m <- length(lat_increments_minor)
    lr <- 1 / {{lat_increments_minor - min(lat_increments)} /
        diff(range(lat_increments))}
    lon_width <- 0.4 * lon_width0
    lon_width <- lr * lon_width
    gridPos_m <- vector('list', length = nlon * nlat_m)
    for(w in 1:nlon){
      lon <- lons[w]
      for(z in 1:nlat_m){
        u <- {w-1} * nlat_m + z
        gridPos_m[[u]] <- matrix(c(lon + c(-1,1) * lon_width[z],
                                   rep(lat_increments_minor[z], 2)), ncol = 2)
      }}
    gridPos_m <- st_multilinestring(x = gridPos_m, dim = 'XY')
    gridPos_m <- st_sfc(gridPos_m, crs = crs.base)
    gridPos_m <- st_transform(gridPos_m, crs = crs.use)
    plt <- plt + geom_sf(data = gridPos_m, linewidth = lineWidth, colour = colour) # axis ticks
  }
  
  gridPos <- st_cast(gridPos, "LINESTRING")
  
  tick_labs <- paste(abs(lat_increments), '* degree ~ S')
  xnudge <- 0.025 * diff(BBox[c('xmin','xmax')])
  plt <- plt +
    geom_sf_text(
      data = gridPos[2:nlat,], label = tick_labs[2:nlat],
      parse = TRUE, hjust = 0, nudge_x = xnudge, size = textSize, colour = colour
    )
  
  #~~~~~~~~~~
  # Longitude
  #~~~~~~~~~~
  nlon <- length(lon_vals)
  lon_points <- matrix(c(lon_vals, rep(max(latlim), nlon)), ncol = 2)
  
  gridPos <- st_multipoint(lon_points, dim = 'XY')
  gridPos <- st_sfc(gridPos, crs = crs.base)
  gridPos <- st_transform(gridPos, crs = crs.use)  
  
  gridPos <- st_cast(gridPos, 'POINT')
  
  lon_labs <- paste(lon_vals %% 360, '* degree ~ E') 
  # ynudge <- 0.0175 * diff(BBox[c('ymin','ymax')])
  ynudge <- 0.025 * diff(BBox[c('ymin','ymax')])
  vj <- numeric(nlon)
  xn <- numeric(nlon)
  yn <- numeric(nlon)
  r <- numeric(nlon)
  
  for(j in 1:nlon){
    vj[j] <- 0
    lv <- lon_vals[j] %% 360
    if({0 <= lv & lv <= 90} | 270 <= lv) inv <- FALSE else inv <- TRUE
    r[j] <- {360 - lv} %% 360 # rotation angle for longitude labels
    xn[j] <- xnudge * sin(lv * pi / 180)
    yn[j] <- ynudge * cos(lv * pi / 180)
    if(inv){
      r[j] <- {r[j] - 180} %% 360
      vj[j] <- 1
    }
  }
  plt <- plt + 
    geom_sf_text(
      data = gridPos, label = lon_labs, parse = TRUE, vjust = vj,
      nudge_x = xn, nudge_y = yn, size = textSize, angle = r, colour = colour)
  plt <- plt + theme(axis.title = element_blank())
  return(plt)
}
