#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create map using geographical shape files converted into spatial data frames
#' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plotMap <- function(map_data,
                    map_colour = NULL,
                    map_linewidths = c(coastline = 0.1, boundary = 1, grid = 0.1),
                    gridData = NULL, overlayGrid = FALSE,
                    base_layer = NULL, base_factor = NULL, base_aes = NULL,
                    base_colour = NULL, base_fill = NULL, base_linewidth = NULL,
                    legend_title = NULL, legend_labels = NULL,
                    dropFactorLevels = FALSE, excludeIce = FALSE
                    ){
  library(ggplot2)
  library(cowplot)
  library(ggnewscale)
  
  if(is.null(map_colour)) map_colour <- c(land = 'lightgrey', ice = 'skyblue',
                                          ocean = 'white', coastline = 'black',
                                          boundary = 'black')
  names(map_colour)[names(map_colour) == 'ocean'] <- 'ocean.bordersLand'
  
  pltDat0 <- subset(map_data, feature == 'ocean.bordersLand')
  pltDat1 <- subset(map_data, feature %in% c('land','ice')[c(TRUE,!excludeIce)])
  pltDat2 <- subset(map_data, feature %in% c('coastline', 'boundary'))
  plt <- ggplot()
  
# Base layer -- contours/density ------------------------------------------
  if(!is.null(base_layer)){
    if(is.null(base_factor)) plt <- plt + geom_sf(data = base_layer) else{
      base_layer$factor <- as.data.frame(base_layer)[,base_factor]
      if(is.null(base_aes)){
        warning("A mapping factor has been selected for the base layer but the aesthetic, 'base_aes', has not been chosen: using base_aes = 'fill' as default")
        base_aes <- 'fill'}
      base_aes <- paste(sort(strsplit(base_aes, '_')[[1]]), collapse = '_')
      plt <- switch(
        base_aes,
        colour = {
          p <- plt + 
            geom_sf(data = base_layer, aes(colour = factor)) +
            guides(colour = guide_legend(title = base_factor))
          if(!is.null(legend_labels)){
            p <- p + scale_colour_manual(values = viridis(length(legend_labels)),
                                         labels = legend_labels, drop = dropFactorLevels)
          }
          p
        },
        fill = {
          p <- plt +
            geom_sf(data = base_layer, aes(fill = factor)) +
            guides(fill = guide_legend(title = base_factor))
          if(!is.null(legend_labels)){
            p <- p + scale_fill_manual(values = viridis(length(legend_labels)),
                                       labels = legend_labels, drop = dropFactorLevels)
          }
          p
        },
        colour_fill = {
          p <- plt +
            geom_sf(data = base_layer, aes(fill = factor, colour = factor)) +
            guides(fill = guide_legend(title = base_factor), colour = 'none')
          if(!is.null(legend_labels) & !is.null(base_fill)){
            p <- p + scale_fill_manual(values = base_fill, labels = legend_labels,
                                       drop = dropFactorLevels) + 
              scale_colour_manual(values = base_fill, drop = dropFactorLevels)
          }else{
            if(!is.null(legend_labels) & is.null(base_fill)){
              p <- p + scale_fill_manual(values = viridis(length(legend_labels)),
                                         labels = legend_labels, drop = dropFactorLevels) + 
                scale_colour_manual(values = viridis(length(legend_labels)), drop = dropFactorLevels)
            }else{
              if(is.null(legend_labels) & !is.null(base_fill)){
                p <- p + scale_fill_manual(values = base_fill, drop = dropFactorLevels) + 
                  scale_colour_manual(values = base_fill, drop = dropFactorLevels)}}}
          p
        }
      )
    }
  }
  
# Ocean -------------------------------------------------------------------
#' Only plot an ocean colour if there is no base layer.
  plotOcean <- is.null(base_layer) & !{map_colour['ocean.bordersLand'] %in% c('white','transparent')}
  if(plotOcean){
    plt <- plt +
      geom_sf(data = pltDat0, aes(colour = feature, fill = feature),
              show.legend = FALSE) +
      scale_colour_manual(values = map_colour) + 
      scale_fill_manual(values = map_colour)
  }
  
# Land masses -------------------------------------------------------------
  baseFactor <- length(plt$layers) > 0 & !is.null(base_factor)
  if(baseFactor){
    plt <- switch(base_aes,
                  colour = plt + new_scale_colour(),
                  fill = plt + new_scale_fill(),
                  colour_fill = {plt + new_scale_colour() + new_scale_fill()}
                  # colour_fill = {plt + new_scale_fill()}
    )
  }
  if(plotOcean) plt <- plt + new_scale_colour() + new_scale_fill()
  plt <- plt +
    geom_sf(data = pltDat1, aes(colour = feature, fill = feature),
            show.legend = FALSE) +
    geom_sf(data = pltDat2, aes(colour = feature, linewidth = feature),
            show.legend = FALSE) +
    scale_colour_manual(values = map_colour) + 
    scale_fill_manual(values = map_colour) + 
    scale_linewidth_manual(values = map_linewidths)
  

# Grid --------------------------------------------------------------------
  if(overlayGrid & !is.null(gridData)){
    plt <- plt + geom_sf(data = gridData, colour = 'black', alpha = 0, linewidth = map_linewidths['grid'])
  }
  
# Set appearance & theme -- output -----------------------------------------------------
  if(!is.null(base_aes)){
    plt <- switch(
      base_aes,
      colour = {
        plt + guides(colour_new = guide_legend(title = legend_title))
      },
      fill = {
        plt + guides(fill_new = guide_legend(title = legend_title))
      },
      colour_fill = {
        plt + guides(fill_new = guide_legend(title = legend_title), colour_new = 'none')
      }
    )
  }
  
  plt <- plt + theme_map()
  
  if(map_colour['ocean.bordersLand'] == 'transparent'){
    plt <- plt + 
    theme(panel.background = element_rect(fill = map_colour['ocean.bordersLand'], colour = NA))
  }
  plt
}

