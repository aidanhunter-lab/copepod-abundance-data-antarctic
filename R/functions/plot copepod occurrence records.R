#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create the Southern Ocean copepod prevalence data plots and tables that
#' feature in the associated data paper
#'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot.data <- function(
    data.file = 'compiled data_all copepods.csv.gz',
    measures = c('presence / absence', 'abundance', 'abundance density',
                 'abundance concentration'),
    res = 200,
    save.plots = TRUE,
    return.plots = FALSE,
    display.summary.stats = FALSE
){
  
  # Load packages -----------------------------------------------------------
  library(ggplot2)
  library(ggrepel)
  library(ggpp)
  library(ggthemes) #' colourblind friendly palette (colorblind_pal())
  library(ggh4x) #' for directly modifying facets
  library(ggpmisc) #' for annotations in relative plot coordinates
  library(ggtext) #' for mixtures of font faces
  library(cowplot)
  library(patchwork)
  library(dplyr)
  library(sf)
  library(ggnewscale)
  library(RColorBrewer)
  library(Cairo) #' unicode
  
  # Directories -------------------------------------------------------------
  
  dir.R <- getwd()
  dir.project <- dirname(dir.R)
  dir.functions <- file.path(dir.R, 'functions')
  dir.data <- file.path(dir.project, 'data')
  dir.data.compiled <- file.path(dir.data, 'zooplankton', 'compiled', 'copepoda')
  dir.data.map <- file.path(dir.data, 'map files', 'Natural Earth')
  dir.plots <- file.path(dir.project, 'plots')
  if(save.plots & !dir.exists(dir.plots)) dir.create(dir.plots)

  # Load functions ----------------------------------------------------------

  omit.funs <- c('clean copepod occurrence records.R',
                 'compile copepod occurrence records.R',
                 'plot copepod occurrence records.R')
  R_functions <- list.files(dir.functions, pattern = "*.R$", ignore.case = TRUE)
  R_functions <- R_functions[!R_functions %in% omit.funs]
  get.functions <- function(dir, n, e){
    invisible(sapply(paste(dir, n, sep = '/'), source, local = e))}
  env <- environment(get.functions)
  get.functions(dir.functions, R_functions, env)
  
  # Load data ---------------------------------------------------------------
  
  dat <- read.csv(gzfile(file.path(dir.data.compiled, data.file)))
  
  # Set global plot theme ---------------------------------------------------
  theme_set(
    theme_bw() + 
      theme(
        strip.text = element_text(family = 'serif'),
        axis.title = element_text(family = 'serif'),
        legend.title = element_text(family = 'serif'),
        strip.background = element_blank(),
        axis.ticks = element_line(linewidth = 0.3),
        panel.grid = element_line(linewidth = 0.3),
        axis.text.y = element_text(angle = 90, hjust = 0.5)
      ))
  
  # Map plot ----------------------------------------------------------------
  
  #' Display every unique sampling event on a map, colour/shape the points by
  #' one or more grouping variables.
  
  #' Make the base map -- extract/upload parts needed to plot
  map.colour <- c(land = 'seashell', ice = 'lightblue1', ocean = 'white',
                  coastline = '#323232', boundary = '#323232')
  map.linewidths <- c(coastline = 0.1, boundary = 0.25, grid = 0.1) 
  
  lat.lim <- c(-90, -27.5) #' adjust northern limit to avoid overlap of boundary and data points
  map.dat <- getMapData(dataDirectory = dir.data.map, hemisphere = 'south',
                        lat_lim = lat.lim, map_colour = map.colour,
                        map_linewidths = map.linewidths, autoSave = FALSE,
                        loadFromFile = FALSE, returnPlot = TRUE)
  
  map <- map.dat$map_plot
  crs.base <- map.dat$crs_base
  crs <- map.dat$crs
  
  #' Variables needed to plot sample events on the map
  u <- c('Sample.Event', 'Longitude', 'Latitude')
  
  #' Also display sample gear type
  w <- 'Sample.Gear'
  
  d <- dat %>%
    select(all_of(c(u,w))) %>%
    distinct()
  
  #' Some sample events were recorded with multiple coordinates (e.g., a single
  #' MOCNESS deployment with each net having different coords), so find averages
  #' within each sample event.
  d <- d %>%
    group_by(Sample.Event) %>%
    mutate(lon.mean = mean(Longitude),
           lat.mean = mean(Latitude)) %>%
    ungroup() %>%
    select(-Longitude, -Latitude) %>%
    rename(Longitude = lon.mean, Latitude = lat.mean) %>%
    distinct()
  
  #' Set gear type abbreviations to use in plot legend
  gears <- data.frame(gear = sort(unique(d$Sample.Gear)))
  gears$new <- gears$gear
  gears$new[grepl('BPS', gears$gear)] <- 'Bathypelagic Plankton Sampler'
  gears$new[grepl('IOSN', gears$gear)] <- 'Indian Ocean Standard Net'
  gears$new[grepl('MTD', gears$gear)] <- 'Motoda Horizontal Net'
  gears$new[grepl('MPS', gears$gear)] <- 'Multiple Plankton Sampler'
  gears$new[grepl('Other', gears$gear)] <- 'Unspecified gear'
  gears$new[grepl('unspecified', gears$gear)] <- 'Unspecified gear'
  gears$new[grepl('UNESCO', gears$gear)] <- 'WP2'
  d$Sample.Gear <- factor(d$Sample.Gear, gears$gear, gears$new)
  
  d$Sample.Gear.Long <- d$Sample.Gear
  d$Sample.Gear <- gsub('Bathypelagic Plankton Sampler', 'BPS', d$Sample.Gear)
  d$Sample.Gear <- gsub('Indian Ocean Standard Net', 'IOSN', d$Sample.Gear)
  d$Sample.Gear <- gsub('Multiple Plankton Sampler', 'MPS', d$Sample.Gear)
  d$Sample.Gear <- gsub('Motoda Horizontal Net', 'MHN', d$Sample.Gear)
  d$Sample.Gear <- gsub('Clarke-Bumpus sampler', 'C-BS', d$Sample.Gear)
  d$Sample.Gear <- gsub('Bogorov-Rass net', 'B-RN', d$Sample.Gear)
  d$Sample.Gear <- gsub('Unspecified gear', 'Unspecified', d$Sample.Gear)
  d$Sample.Gear <- gsub('Sligsby-Gorbunov', 'S-G', d$Sample.Gear)
  d$Sample.Gear <- gsub('Modified NIPR-1', 'MNIPR-1', d$Sample.Gear)

  d <- d %>%
    rename(Group = Sample.Gear) %>%
    rename(Group.Long = Sample.Gear.Long) %>%
    select(-Sample.Event) %>%
    distinct()
  
  #' Set up colour & shape scheme to distinguish gear types
  g <- sort(unique(d$Group))
  ngroup <- length(unique(d$Group))
  
  col.pal <- colorblind_pal()
  ncolour <- 7 #' maximum number of colours handled by selected palette
  group.colour <- col.pal(ncolour+1)[-1] #' exclude black because over-plotting sometimes appears as filled shapes
  
  #' Use a clustering algorithm to determine which gear types tend to be close
  #' to each other, then, as far as possible, assign different colours & shapes
  #' to nearby gears.
  ncluster <- ceiling(ngroup / ncolour)
  ncolour <- ceiling(ngroup / ncluster) #' reduce number of colours if possible
  
  #' Sort colours from light to dark, keeping similar colours separate and
  #' removing colours if possible
  # plot(1:7, 1:7, col = group.colour, pch = 16, cex = 4)
  group.colour <- group.colour[c(4,2,7,3,6,5)]
  # plot(1:ncolour, 1:ncolour, col = group.colour, pch = 16, cex = 4)
  group.shape <- c(3,2,4,6,1,5) #' choose plotting shapes
  
  #' Convert data to spatial object
  ds <- st_as_sf(d, coords = c('Longitude','Latitude'), remove = FALSE,
                 crs = crs.base)
  ds <- st_transform(ds, crs)
  bb <- st_bbox(ds)
  #' Get coordinate centroids for each gear type
  g.centroid <- sapply(g, function(z){
    x <- ds %>% filter(Group == z)
    x <- st_centroid(st_combine(x))
    return(unlist(x))})
  g.centroid[1,] <- {g.centroid[1,] - bb['xmin']} / diff(bb[c('xmax','xmin')])
  g.centroid[2,] <- {g.centroid[2,] - bb['ymin']} / diff(bb[c('ymax','ymin')])
  g.centroid <- setNames(as.data.frame(t(g.centroid)), c('x','y'))
  
  #' Run clustering algorithm on centroids
  hc <- hclust(dist(g.centroid))
  # plot(hc)
  mem <- cutree(hc, k = ncluster) #' trim to selected number of clusters
  
  #' Reassign cluster index based on number of data rows. Highest number of data
  #' is first cluster, lowest number is last cluster.
  x <- data.frame(Group = character(ngroup),
                  n = integer(ngroup),
                  cluster = integer(ngroup))
  x$Group <- sort(unique(d$Group))
  x$n <- table(d$Group)[x$Group]
  x$cluster <- mem[x$Group]
  x <- x[order(x$n, decreasing = TRUE),]
  new.clust <- setNames(1:ncluster, unique(x$cluster))
  x$cluster2 <- new.clust[as.character(x$cluster)]
  x <- x[order(x$Group),]
  mem <- setNames(x$cluster2, x$Group)
  
  #' Build the legend data
  group.legend <- data.frame(Group = g, colour.cluster = mem)
  if(ngroup <= ncolour){
    group.legend$colour <- group.colour[1:ngroup]
    group.legend$shape <- 1
  }else{
    colour.shape <- expand.grid(colour = group.colour, shape = group.shape,
                                stringsAsFactors = FALSE)
    group.legend$sample.size <- as.vector(table(d$Group))
    group.legend <- group.legend[order(group.legend$colour.cluster,
                                       -group.legend$sample.size),]
    group.legend$colour <- NA
    group.legend$shape <- NA
    omit <- rep(FALSE, nrow(colour.shape))
    for(i in 1:ncluster){
      j <- group.legend$colour.cluster == i
      n <- sum(j)
      x <- group.legend[j,]
      x$colour <- group.colour[{{0:{n-1}}%%ncolour}+1]
      group.colour <- c(tail(group.colour, -1), group.colour[1])
      for(k in 1:n){
        cl <- x$colour[k]
        a <- colour.shape$colour == cl
        x$shape[k] <- group.shape[{{k-1}%%6}+1]
        b <- colour.shape$shape == x$shape[k]
        keepTrying <- any({a & b} & omit)
        kk <- k
        while(keepTrying){
          kk <- kk + 1
          x$shape[k] <- group.shape[{{kk-1}%%6}+1]
          b <- colour.shape$shape == x$shape[k]
          keepTrying <- any({a & b} & omit)
        }
        omit <- omit | {a & b}
      }
      group.legend[j,] <- x
    }
    group.legend <- group.legend[g,]
  }
  
  #' Swap CPR shape for '.'
  group.legend$shape[group.legend$Group == 'CPR'] <- 46
  
  group.colour <- setNames(group.legend$colour, group.legend$Group)
  group.shape <- setNames(group.legend$shape, group.legend$Group)
  group.legend$colour.cluster <- NULL
  
  d <- left_join(d, group.legend, by = 'Group')
  
  #' To avoid over-plotting, reorder the data rows according to the number of
  #' entries for each group category.
  x <- sort(table(d$Group), decreasing = TRUE)
  d$Group <- factor(d$Group, names(x))
  d <- d[order(d$Group),]
  d$Group <- factor(d$Group, sort(unique(as.character(d$Group))))
  
  #' Recreate spatial data
  ds <- st_as_sf(d, coords = c('Longitude','Latitude'), remove = FALSE,
                 crs = crs.base)
  ds <- st_transform(ds, crs)
  
  #' Make plot
  plt.map <- map + 
    new_scale_colour() +
    geom_sf(data = ds,
            mapping = aes(colour = Group, shape = Group), size = 1,
            stroke = 0.4, alpha = 1) + 
    scale_colour_manual(name = 'Sample gear', values = group.colour) + 
    scale_shape_manual(name = 'Sample gear', values = group.shape) +
    theme(legend.position = 'right',
          legend.title = element_text(size = 11, family = 'serif'),
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.3, 'cm'),
          legend.margin = margin(0,0,0,0,'cm'),
          plot.margin = unit(x = c(0,0,0,-0.7), 'cm'))
  
  #' Include coordinate axes
  plt.map <- overlay_coordinate_grid(plt.map,
                                     singleLatAxis = FALSE,
                                     latlim = c(-90, -30),
                                     crs.base = map.dat$crs_base,
                                     crs.use = map.dat$crs,
                                     BBox = st_bbox(map.dat$map_data),
                                     textSize = 2.5, lineWidth = 0.25)
  
  # print(plt.map)
  
  #' Save the plot
  
  if(save.plots){
    plt.name <- 'map.png'
    wd <- 16
    ht <- 0.7 * wd
    
    png(filename = file.path(dir.plots, plt.name), width = wd, height = ht,
        units = 'cm', res = res)
    print(plt.map)
    dev.off()
  }
  
  
  # Species sample quantities -----------------------------------------------
  
  #' In how many sample events was each species observed?
  #' Display the number of times species appear in samples, excluding repeats
  #' over copepodite stage and depth.
  
  options(scipen = 100)
  
  d <- dat %>%
    select(Species, Sample.Event) %>%
    distinct()

  #' Which taxa are unambiguously resolved to species level?
  #' Index abbreviated genus
  i <- substr(d$Species, 2, 2) == '.'
  nc <- nchar(d$Species)
  #' One or more species of particular genus
  i <- i | substr(d$Species, nc-2, nc) == ' sp' | substr(d$Species, nc-3, nc) == ' spp'
  #' Multiple species explicitly recorded
  i <- i | grepl(' and ', d$Species)
  # length(unique(d$Species[!i])) #' number of uniquely defined species
  # length(unique(d$Species[i])) #' number of non-uniquely defined species
  d$Single.Species <- !i
  
  #' First, plot taxa resolved to species-level, then more coarsely resolved
  #' taxa, then combine the plots.
  
  d.plt <- d %>% filter(Single.Species)
  d.plt <- table(d.plt$Species)
  d.plt <- data.frame(Species = names(d.plt), number = as.vector(d.plt))
  d.plt <- d.plt[order(d.plt$number),]
  d.plt$Species <- factor(d.plt$Species, d.plt$Species)
  d.plt$index <- 1:nrow(d.plt)
  d.plt$label <- ''
  #' Select some species names to display, including the `nlab` most sampled
  nlab <- 10
  i <- unique(c(head(seq(50, nrow(d.plt), 50), -1),
                {nrow(d.plt) - nlab + 1}:nrow(d.plt)))
  d.plt$label[i] <- as.character(d.plt$Species[i])
  
  xnud <- rep(0, nrow(d.plt))
  xnud[i] <- 50
  xnud[tail(i, nlab)] <- -xnud[tail(i, nlab)]
  ynud <- rep(0, nrow(d.plt))
  
  xbrk <- c(1, seq(50, nrow(d.plt), 50), nrow(d.plt))
  
  plt.species.1 <- ggplot(data = d.plt,
                        mapping = aes(x = index, y = number, label = label)) + 
    geom_point(size = 10, shape = '.') + 
    xlab('Species index') +
    ylab('Sample events') + 
    scale_x_continuous(breaks = xbrk, labels = xbrk) +
    scale_y_continuous(transform = 'log10') + 
    geom_text_repel(size = 3, fontface = 'italic', segment.size = 0.25,
                    position = position_nudge_repel(xnud, ynud),
                    segment.colour = 'grey', min.segment.length = 0,
                    max.overlaps = 100, point.padding = 0, box.padding = 0.25,
                    seed = 1
    ) + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 11))
  
  # print(plt.species.1)
  
  #' Manually position the labels
  d.lab <- d.plt %>% filter(label != '')
  d.lab <- d.lab[rev(row.names(d.lab)),]
  d.lab$x <- d.lab$index
  d.lab$y <- d.lab$number
  d.lab$n <- nchar(d.lab$label)
  i <- c(rep(TRUE, 10), rep(FALSE, nrow(d.lab) - 10)) #' index the top ten species
  d.lab$hjust <- as.numeric(i)
  #' position the less sampled species (relatively easily)
  d.lab$x[!i] <- d.lab$x[!i] + 0.4 * d.lab$n[!i] # + 30
  d.lab$y[!i] <- d.lab$y[!i] - 0.5 * d.lab$number[!i]
  #' position the top ten sampled species
  d.lab$x[i] <- d.lab$x[i] - 0.5 * d.lab$n[i] - seq(20, 100, length.out = sum(i))
  j <- log10(rev(range(d.lab$number[i])))
  pad <- 0.6
  k <- pad * abs(diff(j))
  # j <- j + c(3/2, -2/3) * k
  j <- j + c(1, -1) * k
  j <- 10 ^ seq(j[1], j[2], length.out = sum(i))
  d.lab$y[i] <- j
  #' Lines from names to points
  d.lab$x1 <- d.lab$index #' point
  d.lab$x2 <- d.lab$x #' name
  d.lab$y1 <- d.lab$number #' point
  d.lab$y2 <- d.lab$y #' name
  #' Connecting vertical indicator lines
  d.lab$linetype = 'solid'
  d.lab <- d.lab %>%
    bind_rows(data.frame(
      x1 = c(d.lab$index[!i], d.lab$index[1]), x2 = c(d.lab$index[!i], d.lab$index[1]),
      y1 = c(d.lab$number[!i], d.lab$number[1]), y2 = 0,
      linetype = 'dashed')) %>%
    mutate(linetype = factor(linetype, c('solid','dashed')))
  
  d.lab$group <- 'Resolved to species'
  d.lab$ann.x <- 0.05
  d.lab$ann.y <- 0.975
  
  x <- d.lab$label
  xn <- !is.na(x)
  j <- strsplit(x[xn], ' ')
  j <- sapply(j, function(z){
    y <- paste(paste0('_', z, '_'), collapse = ' ')
    gsub(' ', '<span style="color:white">.</span>' , y)})
  x[xn] <- j
  
  d.lab$label <- x
  
  #' To set the labelling & scaling correctly on a combined plot, note the number
  #' of taxa resolved to species
  x <- d %>% select(Species, Single.Species) %>% distinct()
  n <- c(sum(x$Single.Species), sum(!x$Single.Species))
  
  ybrk <- 10 ^ {0:5}
  
  plt.species.2 <- 
    ggplot(data = d.plt,
                          mapping = aes(x = index, y = number)) + 
    geom_segment(data = d.lab,
                 mapping = aes(x = x1, y = y1, xend = x2, yend = y2,
                               linetype = linetype),
                 linewidth = 0.25, colour = 'lightgrey', show.legend = FALSE) +
    geom_richtext(data = d.lab %>% filter(!is.na(label)),
                  mapping = aes(x = x, y = y, label = label, hjust = hjust),
                  label.padding = unit(0, 'pt'), label.margin = unit(0, 'pt'),
                  label.colour = 'white', size = 3) +
    geom_point(size = 10, shape = '.') + 
    xlab('Species index') +
    ylab('Sample events') + 
    scale_x_continuous(breaks = xbrk, labels = xbrk,
                       expand = expansion(mult = 0.05 * n[2] / n[1])) +
    scale_y_continuous(transform = 'log10', breaks = ybrk) + 
    geom_label_npc(data = d.lab[1,],
                   mapping = aes(npcx = ann.x, npcy = ann.y, label = group),
                   family = 'serif', label.size = 0#, label.padding = unit(1,'cm')
    ) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 11),
          legend.key.size = unit(rep(0,4),'pt')
    )
  
  # print(plt.species.2)
  
  
  #' Repeat for taxa not resolved to species
  d.plt <- d %>% filter(!Single.Species)
  
  d.plt <- table(d.plt$Species)
  d.plt <- data.frame(Species = names(d.plt), number = as.vector(d.plt))
  d.plt <- d.plt[order(d.plt$number),]
  d.plt$Species <- factor(d.plt$Species, d.plt$Species)
  d.plt$index <- 1:nrow(d.plt)
  d.plt$label <- ''
  #' Select some species names to display, including the `nlab` most sampled
  nlab <- 10
  xj <- ceiling(n[1] / 50) * 50 - n[1]
  i <- unique(c(head(seq(xj, nrow(d.plt), 50), -1), {nrow(d.plt) - nlab + 1}:nrow(d.plt)))
  d.plt$label[i] <- as.character(d.plt$Species[i])
  xnud <- rep(0, nrow(d.plt))
  xnud[i] <- 50
  xnud[tail(i, nlab)] <- -xnud[tail(i, nlab)]
  ynud <- rep(0, nrow(d.plt))
  
  xbrk <- c(1, seq(50, nrow(d.plt), 50), nrow(d.plt))
  
  plt.species.3 <- ggplot(data = d.plt,
                          mapping = aes(x = index, y = number, label = label)) + 
    geom_point(size = 10, shape = '.') + 
    xlab('Species index') +
    ylab('Sample events') + 
    scale_x_continuous(breaks = xbrk, labels = xbrk) +
    scale_y_continuous(transform = 'log10') + 
    geom_text_repel(size = 3, fontface = 'italic', segment.size = 0.25,
                    position = position_nudge_repel(xnud, ynud),
                    segment.colour = 'grey', min.segment.length = 0,
                    max.overlaps = 100, point.padding = 0, box.padding = 0.25,
                    seed = 1
    ) + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 11))
  
  # print(plt.species.3)
  
  
  #' Manually position the labels
  rw <- n[2] / sum(n)
  
  d.lab <- d.plt %>% filter(label != '')
  d.lab <- d.lab[rev(row.names(d.lab)),]
  d.lab$x <- d.lab$index
  d.lab$y <- d.lab$number
  i <- c(rep(TRUE, 10), rep(FALSE, nrow(d.lab) - 10)) #' index the top ten species
  d.lab$hjust <- as.numeric(i)
  #' label length
  d.lab$n <- nchar(d.lab$label)
  #' position the less sampled species (relatively easily)
  d.lab$x[!i] <- d.lab$x[!i] + 0.5 * d.lab$n[!i]
  d.lab$y[!i] <- d.lab$y[!i] - 0.5 * d.lab$number[!i]
  #' manually adjust an awkward name
  d.lab$x[head(which(!i),1)] <- d.lab$x[head(which(!i),1)] - 6
  # d.lab$y[head(which(!i),1)] <- d.lab$y[head(which(!i),1)] * 0.45
  
  #' position the top ten sampled species
  d.lab$x[i] <- d.lab$x[i] - 0.5 * d.lab$n[i] - seq(1*rw * 20, 1.5*rw * 100, length.out = sum(i))
  j <- log10(rev(range(d.lab$number[i])))
  pad <- 0.5
  k <- pad * abs(diff(j))
  j <- j + c(3.5, -2/3) * k
  # j <- j + c(1, -1) * k
  j <- 10 ^ seq(j[1], j[2], length.out = sum(i))
  d.lab$y[i] <- j
  #' Lines from names to points
  d.lab$x1 <- d.lab$index #' point
  d.lab$x2 <- d.lab$x #' name
  d.lab$y1 <- d.lab$number #' point
  d.lab$y2 <- d.lab$y #' name

  #' Connecting vertical indicator lines
  d.lab$linetype = 'solid'
  d.lab <- d.lab %>%
    bind_rows(data.frame(
      x1 = c(d.lab$index[!i], d.lab$index[1]), x2 = c(d.lab$index[!i], d.lab$index[1]),
      y1 = c(d.lab$number[!i], d.lab$number[1]), y2 = 0,
      linetype = 'dashed')) %>%
    mutate(linetype = factor(linetype, c('solid','dashed')))
  
  d.lab$group <- 'Nonspecific species'

  d.lab$ann.x <- 0.05
  d.lab$ann.y <- 0.975
  
  x <- d.lab$label
  xn <- !is.na(x)
  j <- strsplit(x[xn], ' ')
  j <- sapply(j, function(z){
    y <- paste(paste0('_', z, '_'), collapse = ' ')
    gsub(' ', '<span style="color:white">.</span>' , y)})
  x[xn] <- j
  x <- gsub('_and_', '<span style="color:white">.</span>and<br>', x)
  d.lab$label <- x
  
  ybrk <- 10 ^ {0:5}
  
  plt.species.4 <- ggplot(data = d.plt,
                          mapping = aes(x = index, y = number)) + 
    geom_segment(data = d.lab,
                 mapping = aes(x = x1, y = y1, xend = x2, yend = y2,
                               linetype = linetype),
                 linewidth = 0.25, colour = 'lightgrey', show.legend = FALSE) +
    geom_richtext(data = d.lab %>% filter(!is.na(label)),
                  mapping = aes(x = x, y = y, label = label, hjust = hjust),
                  label.padding = unit(0, 'pt'), label.margin = unit(0, 'pt'),
                  label.colour = 'white', size = 3) +
    geom_point(size = 10, shape = '.') + 
    xlab('Species index') +
    ylab('Sample events') + 
    scale_x_continuous(breaks = xbrk, labels = xbrk) +
    scale_y_continuous(transform = 'log10', breaks = ybrk) + 
    geom_label_npc(data = d.lab[1,],
                   mapping = aes(npcx = ann.x, npcy = ann.y, label = group),
                   family = 'serif', label.size = 0#, label.padding = unit(1,'cm')
    ) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 11))
  
  # print(plt.species.4)
  
  #' Combine the plots
  
  #' Find the largest y-axis range
  yr1 <- suppressWarnings(ggplot_build(plt.species.2)$layout$panel_params[[1]]$y.range)
  yr2 <- suppressWarnings(ggplot_build(plt.species.4)$layout$panel_params[[1]]$y.range)
  yr <- c(max(c(yr1[1], yr2[1])), max(c(yr1[2], yr2[2])))
  
  #' Make plot widths relative to number of species
  x <- d %>% select(Species, Single.Species) %>% distinct()
  w <- c(sum(x$Single.Species), sum(!x$Single.Species))
  
  #' Reformat x-axis text of plot 2
  xj <- ceiling({w[1]+1} / 50) * 50
  # xlabs2 <- c(w[1]+1, seq(xj, sum(w), 50), sum(w))
  # xbrks2 <- c(1, seq(xj-w[1], w[2], 50), w[2])
  xlabs2 <- c(seq(xj, sum(w), 50), sum(w))
  xbrks2 <- c(seq(xj-w[1], w[2], 50), w[2])
  
  plt.species.2.2 <- suppressMessages(
    plt.species.2 + 
      scale_y_continuous(trans = 'log10', limits = 10^yr, breaks = ybrk) +
      labs(tag = 'A') +
      theme(axis.title.x = element_blank(),
            plot.margin = unit(rep(0,4),'pt'),
            plot.tag.location = 'plot',
            plot.tag.position = c(0.01,0.99),
            plot.tag = element_text(family = 'serif', size = 11)))
  
  plt.species.4.2 <- suppressMessages(
    plt.species.4 + 
      scale_y_continuous(trans = 'log10', limits = 10^yr, breaks = ybrk) +
      scale_x_continuous(breaks = xbrks2, labels = xlabs2) +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            plot.margin = unit(rep(0,4),'pt')))
  
  plt.species <- plt.species.2.2 - plt.species.4.2 + plot_layout(widths = w)
  
  
  sp <- 0.05
  x.title <- ggdraw() +
    draw_label('Species index', fontface = 'plain', fontfamily = 'serif',
               x = 0.5, size = 11) + 
    theme(plot.margin = unit(c(-5,0,0,0),'pt'))
  
  plt.species <- (plt.species - x.title) + plot_layout(ncol = 1, heights = c(1-sp, sp))
  
  # print(plt.species)
  
  
  # Samples per copepodite stage by month/season ----------------------------
  
  d <- dat %>%
    filter(Copepodite.Stage %in% paste0('C', 1:6)) %>%
    select(Sample.Event, Month, Species, Copepodite.Stage) %>%
    distinct()
  
  cs <- sort(unique(d$Copepodite.Stage))
  cs <- c(paste0('C', 1:6), cs[grepl('-', cs)])
  d$Copepodite.Stage <- factor(d$Copepodite.Stage, cs)
  
  d$Month <- factor(d$Month, month.abb)
  
  d_ <- d %>%
    select(Sample.Event, Month, Copepodite.Stage) %>%
    distinct()
  
  d.plt <- expand.grid(Month = levels(d$Month), 
                       Copepodite.Stage = levels(d$Copepodite.Stage))
  d.plt$number.of.events <- sapply(1:nrow(d.plt), function(z){
    x <- sum(d_$Month == d.plt$Month[z] & 
               d_$Copepodite.Stage == d.plt$Copepodite.Stage[z])
    return(x)})
  
  d_ <- d %>%
    select(Month, Species, Copepodite.Stage) %>%
    distinct()
  d.plt$number.of.species <- sapply(1:nrow(d.plt), function(z){
    x <- sum(d_$Month == d.plt$Month[z] &
               d_$Copepodite.Stage == d.plt$Copepodite.Stage[z])
    return(x)})
  
  d.plt$number.of.species <- as.character(d.plt$number.of.species)
  d.plt$number.of.species[d.plt$number.of.species == '0'] <- ''
  
  #' Use subscripts for copepodite stage plot labels
  # unique(dd$Copepodite.Stage)
  cop.stage.sub <- paste0('C', c('\U2081','\U2082','\U2083','\U2084','\U2085','\U2086'))
  
  d.plt$Copepodite.Stage <- factor(d.plt$Copepodite.Stage,
                                   levels(d.plt$Copepodite.Stage),
                                   labels = cop.stage.sub)
  
  nmon <- length(levels(d$Month))
  mon.colour <- colorRampPalette(brewer.pal(11, 'PRGn'))(nmon)
  mon.colour <- setNames(mon.colour, levels(d$Month))
  d.wid <- 0.9 #' controls space between main grouping
  b.wid <- 0.3 #' width of individual bars
  #' Tweak horizontal position of some numbers to avoid overlaps
  hj <- rep(0.5 , 6*12)
  hj[1] <- 0.35
  hj[2] <- 0.3
  hj[3] <- 0.6
  hj[4] <- 0.4
  hj[7] <- 0.65
  hj[8] <- 0.45
  hj[11] <- 0.4
  hj[13] <- 0.55
  hj[14] <- 0.45
  hj[19] <- 0.55
  hj[22] <- 0.45
  hj[23] <- 0.35
  hj[30] <- 0.6
  hj[31] <- 0.45
  hj[33] <- 0.4
  hj[35] <- 0.4
  hj[42] <- 0.55
  hj[43] <- 0.45
  hj[46] <- 0.45
  hj[47] <- 0.4
  hj[54] <- 0.65
  hj[57] <- 0.45
  hj[62] <- 0.45
  hj[63] <- 0.45
  hj[67] <- 0.6
  hj[68] <- 0.4
  hj[70] <- 0.9
  hj[71] <- 0.525
  hj[72] <- 0.8
  
  plt.copepodite.stage <- ggplot(
    data = d.plt,
    mapping = aes(x = Copepodite.Stage, y = number.of.events,
                  fill = Month, colour = Month)) + 
    geom_col(width = b.wid, position = position_dodge(width=d.wid)) +
    geom_text(mapping = aes(label = number.of.species, group = Month),
              colour = 'black',
              position = position_dodge(width=d.wid),
              vjust = -0.5,
              hjust = hj,
              size = 1.8, angle = 0) +
    scale_fill_manual(values = mon.colour) +
    scale_colour_manual(values = mon.colour) +
    xlab('Copepodite stage') +
    ylab('Sample events') + 
    scale_x_discrete(expand = expansion()) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    guides(colour = 'none',
           fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(panel.grid.major.x = element_blank(),
          legend.margin = margin(0,0,0,0,'cm'),
          legend.title.position = 'left',
          legend.position = 'inside',
          legend.position.inside = c(0.05,0.9),
          legend.justification.inside = c(0,1),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 9, family = 'serif'),
          legend.key.size = unit(0.3, 'cm'),
          axis.text.x = element_text(size = 10, family = 'serif'),
          axis.text.y = element_text(size = 8),
          axis.title = element_text(size = 11),
          axis.ticks.x = element_blank())
  
  # print(plt.copepodite.stage)
  

  # Combine plots -- species & copepodite stage -----------------------------

  plt.species.stage <-
    (plt.species + theme(plot.margin = unit(c(0,0,10,0),'pt'))) -
    (plt.copepodite.stage +
       labs(tag = 'B') + 
       theme(plot.tag.location = 'plot',
             plot.tag.position = c(0.01,0.99),
             plot.tag = element_text(family = 'serif', size = 11),
             plot.margin = unit(c(0,0,0,0), 'pt'))) +
    plot_layout(ncol = 1, heights = c(0.64,0.36))
  
  if(save.plots){
    plt.name <- 'species_and_copepodite_stage.png'
    wd <- 16
    ht <- 1 * wd
    
    png(filename = file.path(dir.plots, plt.name), width = wd, height = ht,
        units = 'cm', res = res)
    suppressWarnings(print(plt.species.stage))
    dev.off()
  }
  
  # Time series of sample events --------------------------------------------
  d <- dat %>% filter(Measurement %in% measures)
  
  measures.unit <- measures
  measures.unit[2:4] <- sapply(strsplit(measures.unit[2:4], ' '),
                               function(z) z[1])
  measures.unit[2:4] <- paste(measures.unit[2:4],
                              c('sample\U207B\U00B9', 'm\U207B\U00B2',
                                'm\U207B\U00B3'))
  d$Measurement <- factor(d$Measurement, levels = measures, labels = measures.unit)
  
  d <- d %>%
    select(Year, Measurement, Sample.Event) %>%
    distinct()
  
  yr <- range(d$Year)
  yr[1] <- floor(yr[1] / 5) * 5
  yr[2] <- ceiling(yr[2] / 5) * 5
  yr.fac <- seq(yr[1], yr[2], 5)
  yr.lim <- seq(yr[1]-5/2, yr[2]+5/2, 5)
  yr.brk <- yr.fac[yr.fac %% 10 == 0]
  
  d$Year.fac <- cut(d$Year, breaks = yr.lim, include.lowest = TRUE, labels = yr.fac)
  m.colour <- palette.colors(8, palette = 'Tableau 10')[c(2,5:7)]
  
  i.per.sample <- 2
  
  x <- TRUE
  while(x){
    x <- levels(d$Year.fac)[1]
    x <- !any(d$Year.fac == x)
    if(x) d$Year.fac <- factor(d$Year.fac, levels(d$Year.fac)[-1])}
  x <- TRUE
  while(x){
    x <- tail(levels(d$Year.fac), 1)
    x <- !any(d$Year.fac == x)
    if(x) d$Year.fac <- factor(d$Year.fac, head(levels(d$Year.fac), -1))}
  
  plt.timeseries.1 <- ggplot() + 
    geom_bar(data = d %>% filter(Measurement != levels(d$Measurement)[i.per.sample]),
             mapping = aes(x = Year.fac, fill = Measurement),
             width = 0.9, position = position_stack()) + 
    scale_x_discrete(breaks = yr.brk, drop = FALSE, expand = expansion()) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = m.colour[-i.per.sample]) + 
    xlab('Year') + 
    ylab('Sample events') + 
    guides(fill = guide_legend(reverse = FALSE)) +
    theme(strip.text = element_text(size = 11),
          axis.text = element_text(size = 6, hjust = 0.5),
          axis.title = element_text(size = 11),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9, family = 'serif'),
          legend.key.size = unit(0.3, 'cm'),
          legend.position = 'inside',
          legend.position.inside = c(0.05,0.95),
          legend.justification.inside = c(0,1),
          plot.margin = unit(c(5.5,5.5,5.5,15),'pt'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )
  
  # print(plt.timeseries.1)
  
  plt.timeseries.2 <- ggplot() + 
    geom_bar(data = d %>% filter(Measurement == levels(d$Measurement)[i.per.sample]),
             mapping = aes(x = Year.fac, fill = Measurement),
             width = 0.9, position = position_stack()) + 
    scale_x_discrete(breaks = yr.brk, drop = FALSE, expand = expansion()) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = m.colour[i.per.sample]) + 
    xlab('Year') + 
    ylab('Sample events') + 
    guides(fill = guide_legend(reverse = FALSE)) +
    theme(strip.text = element_text(size = 11),
          axis.text = element_text(size = 6, hjust = 0.5),
          axis.title = element_text(size = 11),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9, family = 'serif'),
          legend.key.size = unit(0.3, 'cm'),
          legend.position = 'inside',
          legend.position.inside = c(0.05,0.95),
          legend.justification.inside = c(0,1),
          plot.margin = unit(c(5.5,5.5,5.5,15),'pt'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )
  
  # print(plt.timeseries.2)
  
  plt.timeseries <- plot_grid(plt.timeseries.1 + theme(axis.title = element_blank()),
                    plt.timeseries.2 + theme(axis.title = element_blank()),
                    nrow = 1,
                    labels = c('A', 'B'), label_size = 11,
                    label_fontface = 'plain', label_fontfamily = 'serif')
  
  plt.timeseries <- plt.timeseries +
    annotate('text', x = 0.001, y = 0.5, label = 'Sample events', size = 4,
             angle = 90, family = 'serif', vjust = 1) 
  
  x.title <- ggdraw() +
    draw_label('Year', fontface = 'plain', fontfamily = 'serif',
               x = 0.515, size = 11)
  
  plt.timeseries <- plot_grid(plt.timeseries, x.title,
                              ncol = 1, rel_heights = c(0.95, 0.05))
  
  # print(plt.timeseries)
  
  # plt.name <- 'sample_times_histograms2.png'
  # wd <- 16
  # ht <- 5/8 * wd
  # 
  # png(filename = plt.name, width = wd, height = ht, units = 'cm', res = 200)
  # print(plt1)
  # dev.off()
  

  # Depth distribution of samples -------------------------------------------

  d <- dat %>% 
    select(Sample.Event, Depth) %>%
    distinct()
  d$Sample <- 1:nrow(d)
  d <- left_join(dat, d, by = c('Sample.Event','Depth'))
  
  d <- d %>% filter(Measurement %in% measures)
  
  measures.unit <- measures
  measures.unit[2:4] <- sapply(strsplit(measures.unit[2:4], ' '),
                               function(z) z[1])
  measures.unit[2:4] <- paste(measures.unit[2:4],
                              c('sample\U207B\U00B9', 'm\U207B\U00B2',
                                'm\U207B\U00B3'))
  
  d$Measurement <- factor(d$Measurement, levels = measures, labels = measures.unit)
  
  #' reorder factor to match previous plot
  d$Measurement <- factor(d$Measurement, levels(d$Measurement)[c(1,3:4,2)])
  
  #' `There are some NA depth values! Check the ELTANIN and BAS rmt records to find the problem...`
  #' `For now, just omit these records.`
  
  dep <- d$Depth[!is.na(d$Depth)]
  # range(dep)
  dep.breaks <- c(0, 10, 50, 100, 250, 500, 1000, 2000, max(dep))
  ndep <- length(dep.breaks) - 1
  
  dep.labels <- sapply(strsplit(as.character(dep.breaks), '\\.'),
                       function(z) z[1])
  dep.labels <- paste(paste(head(dep.labels, -1), tail(dep.labels, -1),
                            sep = '\U2013'), 'm')
  dep.labels[ndep] <- paste0('>', dep.breaks[ndep], ' m')
  dep.colour <- brewer.pal(9, 'BuPu')
  dep.colour <- setNames(tail(dep.colour, ndep), dep.labels)
  
  d <- d %>%
    select(Year, Measurement, Depth, Sample) %>%
    distinct() %>%
    filter(!is.na(Depth)) %>%
    mutate(Depth = cut(Depth, dep.breaks, include.lowest = TRUE,
                       labels = dep.labels))
  
  d$Depth <- factor(d$Depth, rev(levels(d$Depth)))
  
  d$Decade <- paste0(floor(d$Year / 10) * 10, 's')
  d$Decade <- factor(d$Decade, sort(unique(d$Decade)))
  d$Depth2 <- as.character(d$Depth)
  d$Depth2 <- factor(substr(d$Depth2, 1, nchar(d$Depth2) - 2),
                     substr(levels(d$Depth), 1, nchar(levels(d$Depth)) - 2))
  
  ndec <- length(levels(d$Decade))
  dec.colour <- colorRampPalette(brewer.pal(9, 'YlOrBr'))(ndec)
  dec.colour <- setNames(dec.colour, levels(d$Decade))
  
  d$Decade <- factor(d$Decade, rev(levels(d$Decade)))
  
  plt.depth <- ggplot() + 
    geom_bar(data = d,
             mapping = aes(y = Depth2, fill = Decade),
             width = 0.9) + 
    facet_wrap(vars(Measurement), scales = 'free_x', nrow = 1) + 
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_y_discrete(drop = FALSE, expand = expansion()) +
    scale_fill_manual(values = dec.colour) + 
    xlab('Number of samples') + 
    ylab('Depth (m)') + 
    guides(fill = guide_legend(reverse = TRUE, ncol=1)) +
    theme(strip.text = element_text(size = 9),
          axis.text.y = element_text(size = 6, angle = 0, hjust = 1),
          axis.text.x = element_text(size = 6),
          axis.title = element_text(size = 11),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 7),
          legend.key.size = unit(0.3, 'cm'),
          legend.position = 'right',
          legend.margin = margin(-0.2,0,0,0,'cm'),
          plot.margin = unit(c(5.5,5.5,5.5,1),'pt'),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
    )
  
  # print(plt.depth)
  
  plt.depth <- plt.depth + facetted_pos_scales(x = list(
    NULL, NULL, NULL,
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.05)),
      breaks = c(0, 20000, 40000))
  ))
  
  plt.depth <- plot_grid(plt.depth, labels = 'C', label_size = 11,
                    label_fontfamily = 'serif', label_fontface = 'plain',
                    label_x = 0.001, label_y = 0.96)
  
  
  

  # Combine plots -- times & depths -----------------------------------------

  plt.time.depth <- plot_grid(plt.timeseries, plt.depth, ncol = 1, rel_heights = c(0.45,0.55))
  # print(plt.time.depth)
  
  if(save.plots){
    plt.name <- 'sample_years_and_depths.png'
    wd <- 16
    ht <- 0.7 * wd
    
    png(filename = file.path(dir.plots, plt.name), width = wd, height = ht,
        units = 'cm', res = 200)
    print(plt.time.depth)
    dev.off()
  }
  
  # Basic summary statistics ------------------------------------------------

  print.out <- list()
  
  #' Number of data records/rows
  number.of.records <- nrow(dat)
  print.out$number.of.records <- paste('number of records:', number.of.records)
  
  #' Number of taxa -- total and resolved to species
  
  #' Index taxa not resolved to species
  i <- substr(dat$Species, 2, 2) == '.' #' abbreviated genus
  nc <- nchar(dat$Species)
  i <- i | substr(dat$Species, nc-2, nc) == ' sp' | 
    substr(dat$Species, nc-3, nc) == ' spp' #' one or more species of particular genus
  i <- i | grepl(' and ', dat$Species) #' multiple species explicitly recorded
  
  number.of.unique.species <- length(unique(dat$Species[!i]))
  number.of.non.unique.species <- length(unique(dat$Species[i]))
  number.of.taxa <- length(unique(dat$Species))
  
  print.out$number.of.taxa <- paste('number of taxa:', number.of.taxa)
  print.out$number.of.uniquely.resolved.species <- paste(
    'number of taxa resolved to species:', number.of.unique.species)
  print.out$number.of.ambiguous.species <- paste(
    'number of taxa recorded as nonspecific species:', number.of.non.unique.species)

  
  #' Records per species & stage
  records.per.taxa <- table(dat$Species)
  records.per.taxa <- sort(setNames(as.vector(records.per.taxa),
                                    names(records.per.taxa)))
  x <- 10^{0:5}
  taxa.per.record.bin <- cut(records.per.taxa, breaks = x,
                             include.lowest = TRUE, right = FALSE)
  taxa.per.record.bin <- table(taxa.per.record.bin)
  
  records.per.unique.species <- table(dat$Species[!i])
  records.per.nonspecific.species <- table(dat$Species[i])
  records.per.unique.species <- sort(setNames(as.vector(records.per.unique.species),
                                              names(records.per.unique.species)))
  records.per.nonspecific.species <- sort(setNames(as.vector(records.per.nonspecific.species),
                                                   names(records.per.nonspecific.species)))
  unique.species.per.record.bin <- cut(records.per.unique.species, breaks = x,
                                       include.lowest = TRUE, right = FALSE)
  nonspecific.species.per.record.bin <- cut(records.per.nonspecific.species,
                                            breaks = x, include.lowest = TRUE, right = FALSE)
  unique.species.per.record.bin <- table(unique.species.per.record.bin)
  nonspecific.species.per.record.bin <- table(nonspecific.species.per.record.bin)
  
  print.out$taxa.sample.frequency <- rbind(taxa.per.record.bin,
                                           unique.species.per.record.bin,
                                           nonspecific.species.per.record.bin)
  
  d <- dat %>%
    filter(Life.Stage == 'copepodite') %>%
    select(Copepodite.Stage)
  
  number.of.copepodite.records <- nrow(d)
  
  print.out$number.of.copepodite.records <- paste('number of copepodite records:',
                                                  number.of.copepodite.records)
  
  records.per.cop.stage <- table(d)
  cn <- unique(c(paste0('C', 1:6), sort(names(records.per.cop.stage))))
  records.per.cop.stage <- setNames(as.vector(records.per.cop.stage[cn]), cn)
  
  print.out$records.per.copepodite.stage <- records.per.cop.stage
  
  records.resolved.to.cop.stage <- records.per.cop.stage[cn[1:6]]
  
  percent.of.copepodite.records.resolved.to.stage <- 100 * sum(records.resolved.to.cop.stage) / 
    number.of.copepodite.records
  
  print.out$percent.resolved.to.stage <- paste0(
    'copepdite records resolved to developmental stage: ',
    round(percent.of.copepodite.records.resolved.to.stage, 1), '%')
  
  #' Number of sampling events
  n.events <- length(unique(dat$Sample.Event))
  
  #' Sample events recording different depths
  d <- dat %>% 
    select(Sample.Event, Depth) %>%
    distinct() %>%
    group_by(Sample.Event) %>%
    mutate(n.dep = length(Depth)) %>%
    ungroup() %>%
    mutate(Multiple.Depths = n.dep > 1) %>%
    select(Sample.Event, Multiple.Depths) %>%
    distinct()
  
  n.events.multiple.depths <- sum(d$Multiple.Depths)
  n.events.single.depth <- n.events - n.events.multiple.depths
  
  print.out$number.of.sample.events <- paste('number of sampling events:', n.events)
  print.out$number.of.events.sampling.multiple.depths <- paste('number of events sampling multiple depths:', n.events.multiple.depths)
  
  print.out <- print.out[c('number.of.records', 'number.of.sample.events',
    'number.of.events.sampling.multiple.depths', 'number.of.taxa',
    'number.of.uniquely.resolved.species', 'number.of.ambiguous.species',
    'taxa.sample.frequency', 'number.of.copepodite.records',
    'records.per.copepodite.stage', 'percent.resolved.to.stage')]
  
  


  # Function outputs --------------------------------------------------------

  rm(list = c('plot.data'), envir = .GlobalEnv)
  
  if(display.summary.stats) print(print.out)
  
  if(return.plots){
    #' map
    wd <- 16
    ht <- 0.7 * wd
    x11(width = 0.39*wd, height = 0.39*ht)
    print(plt.map)
    #' species & stage
    wd <- 16
    ht <- 1 * wd
    x11(width = 0.39*wd, height = 0.39*ht)
    suppressWarnings(print(plt.species.stage))
    #' time & depth
    wd <- 16
    ht <- 0.7 * wd
    x11(width = 0.39*wd, height = 0.39*ht)
    print(plt.time.depth)
  }
  
  return(invisible(NULL))
  
}


