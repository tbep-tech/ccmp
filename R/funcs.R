
# utility functions ---------------------------------------------------------------------------

# get rdata from github
# try simple load, download if fail
rdataload <- function(flurl){
  
  fl <- basename(flurl)
  obj <- gsub('\\.RData$', '', fl)
  
  # try simple load
  ld <- try(load(url(flurl)), silent = T)
  
  # return x if load worked
  if(!inherits(ld, 'try-error')){
    out <- get(obj)
  }
  
  # download x if load failed
  if(inherits(ld, 'try-error')){
    
    fl <- paste(tempdir(), fl, sep = '/')
    download.file(flurl, destfile = fl, quiet = T)
    load(file = fl)
    out <- get(obj)
    suppressMessages(file.remove(fl))
    
  }
  
  return(out)
  
}

# data-driven graphics ------------------------------------------------------------------------

# pyro map otb
pyro_plo <- function(yr = 2021, mo = c('Jun', 'Oct')){
  
  otbsta <- stations |> dplyr::filter(bay_segment == 'OTB') |> dplyr::pull(epchc_station)
  
  tomap <- phytodata |> 
    dplyr::filter(yr == !!yr) |> 
    dplyr::filter(name == 'Pyrodinium bahamense') |> 
    dplyr::filter(epchc_station %in% otbsta) |> 
    dplyr::mutate(epchc_station = as.numeric(epchc_station)) |> 
    dplyr::left_join(stations, by = c('epchc_station' = 'epchc_station')) |> 
    dplyr::filter(mo >= !!mo[1] & mo <= !!mo[2]) |> 
    dplyr::summarise(
      lat = mean(Latitude),
      lon = mean(Longitude), 
      count = max(count, na.rm = T), 
      .by = epchc_station
    ) |> 
    sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326) 

  dat_ext <- tomap |> 
    sf::st_bbox() |> 
    sf::st_as_sfc() |> 
    sf::st_buffer(dist = units::set_units(2, kilometer)) |>
    sf::st_bbox() |> 
    unname()
  
  m <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(zoom = 12, quiet = TRUE, progress = "none", type = 'cartolight', cachedir = system.file("rosm.cache", package = "ggspatial"))  
  
  ttl <- paste0('Maximum cell count (per 0.1mL), ', yr, ' ', mo[1], ' - ', mo[2])
  
  m <- m + 
    ggplot2::geom_sf(data = tomap, ggplot2::aes(size = count), fill = 'red', color = 'red', pch = 21, alpha = 0.5, inherit.aes = F) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(), 
      axis.title = ggplot2::element_blank(), 
      axis.text.y = ggplot2::element_text(size = 8), 
      axis.text.x = ggplot2::element_text(size = 8, angle = 30, hjust = 1),
      axis.ticks = ggplot2::element_line(colour = 'grey'),
      panel.background = ggplot2::element_rect(fill = NA, color = 'black')
    ) +
    ggplot2::scale_size_continuous(range = c(2, 10)) +
    ggplot2::labs(
      size = 'Cells (/0.1 mL)',
      subtitle = 'Old Tampa Bay, EPCHC stations',
      title = ttl
    ) 

  m <- m +
    ggspatial::annotation_scale(location = 'br', unit_category = 'metric')
  m <- m +
    ggspatial::annotation_north_arrow(location = 'tl', which_north = "true", height = grid::unit(0.75, "cm"), 
                                    width = grid::unit(0.75, "cm"))
  
  dat_ext <- dat_ext |> 
    sf::st_as_sfc(dat_ext) |> 
    sf::st_transform(crs = 4326) |> 
    sf::st_bbox()
  
  # set coordinates because vector not clipped
  m <- m +
    ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)
  
  return(m)
  
}

adload_plo <- function(){
  
  load(file = here::here('data/tnanndat.RData'))  
  
  cols <- c('#427355', '#5C4A42', '#958984', '#004F7E', '#00806E')

  toplo <- tnanndat |> 
    dplyr::filter(year >= 2017 & year <= 2021) |> 
    dplyr::filter(bay_segment == 'All Segments (- N. BCB)') |> 
    dplyr::summarise(
      tn_load = sum(tn_load), 
      .by = source
    ) |> 
    dplyr::mutate(
      source = factor(source, 
                      levels = c('AD', 'DPS', 'GWS', 'IPS', 'NPS'),
                      labels = c('Atmospheric\nDeposition', 'Municipal\nWastewater', 'Groundwater', 'Industrial\nWastewater', 'Stormwater\nRunoff')
                      ), 
      percent = paste0(round(tn_load / sum(tn_load) * 100, 0), '%'),
      toemph = ifelse(source == 'Atmospheric\nDeposition', 'a', 'b')
    )
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(y = reorder(source, tn_load), x = tn_load / 1000)) + 
    ggplot2::geom_col(ggplot2::aes(fill = toemph), color = 'black', show.legend = F) + 
    ggplot2::geom_text(ggplot2::aes(label = percent, size = toemph), hjust = -0.25, show.legend = F) +
    ggplot2::scale_fill_manual(values = c('#004F7E', '#958984')) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1* max(toplo$tn_load / 1000))) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(), 
      # axis.text = ggplot2::element_text(size = 14) 
    ) +
    ggplot2::scale_size_manual(values = c(6, 4)) +
    ggplot2::labs(
      x = 'Tons (x 1000)', 
      y = NULL,
      title = 'Sources of nitrogen loading to Tampa Bay', 
      subtitle = '2017 - 2021'
    )
  
  return(p)
  
}

# bacteria verified impaired wbids
bacwbid_plo <- function(){
  
  tbshed <- st_make_valid(tbshed)
  
  # verified wbid polygons, run 60
  # https://geodata.dep.state.fl.us/datasets/verified-list-waterbody-ids-wbids
  # first request gets whole state (long)
  # second request gets tampa bay and tampa bay tributaries group name, note that I can also search by parameter group, but query doesn't allow complex and/or logic
  # vwbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/IMPAIRED_WATERS/MapServer/5/query?outFields=*&where=1%3D1&f=geojson', quiet = T)
  vwbid = st_read("https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/IMPAIRED_WATERS/MapServer/5/query?where=GROUP_NAME%20%3D%20'Tampa%20Bay'%20OR%20GROUP_NAME%20%3D%20'Tampa%20Bay%20Tributaries'&outFields=GROUP_NAME,WATERBODY_CLASS,PARAMETER_ASSESSED,PARAMETER_GROUP,WBID&outSR=4326&f=geojson", quiet = T)

  vwbid <- st_make_valid(vwbid)
  
  # make sure subset by tb watershed, retain only bacteria and unique wbids
  tbvwbid <- vwbid[tbshed, ] |> 
    dplyr::filter(PARAMETER_GROUP %in% c('Bacteria')) |> 
    dplyr::select(-PARAMETER_GROUP, -PARAMETER_ASSESSED) |>
    unique()
  
  m <- ggplot() +
    annotation_map_tile(zoom = 10, type = 'cartolight', progress = 'none', quiet = T) +
    ggspatial::annotation_scale(location = 'br', unit_category = 'metric') +
    ggspatial::annotation_north_arrow(location = 'tl', which_north = "true", height = grid::unit(0.75, "cm"), width = grid::unit(0.75, "cm")) +
    geom_sf(data = tbvwbid, fill = 'red', col = 'red', alpha = 0.6) +
    theme_minimal()
  
  return(m)
  
}

# intertidal habitat 1950 and current
intertidal_plo <- function(curyr = 2020){
  
  nms <- c('Mangrove Forests', 'Salt Marshes', 'Salt Barrens')
           
  # from 2017 HMPU bh1 graphic
  ca1950 <- tibble::tibble(
    name = 'ca1950',
    HMPU_TARGETS = nms,
    Acres = c(15894, 6621, 1371)
  )
  
  # make sure year is valid
  if(!curyr %in% acres$name)
    stop('year not found')
  
  curr <- acres |> 
    dplyr::filter(name == as.character(curyr)) |> 
    dplyr::filter(HMPU_TARGETS %in% nms)
  
  toplo <- rbind(ca1950, curr) |> 
    dplyr::mutate(
      HMPU_TARGETS = factor(HMPU_TARGETS, levels = nms), 
      name = factor(name, levels = c('ca1950', as.character(curyr)))
    ) |> 
    dplyr::mutate(
      Acreslab = formatC(round(Acres, 0), format = 'd', big.mark = ',')
    ) |> 
    dplyr::arrange(name, HMPU_TARGETS)
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = 'x', y = Acres, fill = HMPU_TARGETS, group = name)) + 
    ggplot2::geom_bar(stat = 'identity', position = 'fill', color = 'black') + 
    ggplot2::geom_text(ggplot2::aes(label = Acreslab), position = ggplot2::position_fill(vjust = 0.5), size = 4.5, 
                       color = 'white', fontface = 'bold') +
    ggplot2::facet_wrap(~name) +
    ggplot2::coord_polar('y', start = 0) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values = c('#028576', '#427355', '#76923C')) + 
    ggplot2::theme(
      legend.position = 'bottom', 
      strip.text = ggplot2::element_text(size = 15), 
      legend.text = ggplot2::element_text(size = 13)
    ) +  
    ggplot2::labs(
      fill = NULL
    )
  
  return(p)
  
}

# map of ccha sites
cchasites_plo <- function(){

  tranloc <- rdataload('https://github.com/tbep-tech/ccha-sampling-effort/raw/main/data/tranloc.RData')
  
  tomap <- tranloc |> 
    st_centroid()
  tomap <- tomap |> 
    dplyr::mutate(
      lon = st_coordinates(tomap)[, 1], 
      lat = st_coordinates(tomap)[, 2]
    )

  dat_ext <- tomap |>
    st_buffer(dist = 10000) |>
    st_bbox() |>
    unname()

  m <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(zoom = 10, type = 'cartolight', quiet = T, progress = 'none') +
    ggspatial::annotation_scale(location = 'bl', unit_category = 'metric') +
    ggspatial::annotation_north_arrow(location = 'tr', which_north = "true", height = grid::unit(0.75, "cm"), width = grid::unit(0.75, "cm")) +
    ggplot2::geom_sf(data = tomap, inherit.aes = F, size = 2) +
    ggrepel::geom_text_repel(data = tomap, ggplot2::aes(label = site, x = lon, y = lat), inherit.aes = F) +
    ggplot2::labs(
      x = NULL, 
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)
  
  return(m)
  
}

# map of tidal creeks in tb watershed
tdlcrk_plo <- function(maxyr = 2022){
  
  cols <- c('#ADD8E6', '#2DC938', '#E9C318', '#EE7600', '#FF4040')
  names(cols) <- c('No Data', 'Monitor', 'Caution', 'Investigate', 'Prioritize')
  
  tomap <- tidalcreeks[tbshed, ] 
  scrs <- anlz_tdlcrk(tomap, iwrraw, yr = maxyr) |> 
    dplyr::select(id, score) |> 
    dplyr::mutate(
      score = factor(score, levels = names(cols))
    )
  
  tomap <- tomap |> 
    dplyr::left_join(scrs, by = c('id'))
  
  dat_ext <- tomap |>
    st_centroid() |> 
    st_buffer(dist = 10000) |>
    st_bbox() |>
    unname()
  
  m <- ggplot2::ggplot() +
    ggspatial::annotation_map_tile(zoom = 10, type = 'cartolight', quiet = T, progress = 'none') +
    ggspatial::annotation_scale(location = 'bl', unit_category = 'metric') +
    ggspatial::annotation_north_arrow(location = 'tr', which_north = "true", height = grid::unit(0.75, "cm"), width = grid::unit(0.75, "cm")) +
    ggplot2::geom_sf(data = tomap, ggplot2::aes(col = score, fill = score), inherit.aes = F, linewidth = 0.75) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_fill_manual(values = cols) + 
    ggplot2::labs(
      x = NULL, 
      y = NULL, 
      color = NULL, 
      fill = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(xlim = dat_ext[c(1, 3)], ylim = dat_ext[c(2, 4)], expand = FALSE, crs = 4326)
  
  return(m)
  
}  
  
# scallop search counts
scallop_plo <- function(){
  
  # data from dashboard, only back to 2007
  cntdat <- rdataload('https://github.com/tbep-tech/scallop-search/raw/master/data/cntdat.RData')
  
  # format cntdat for year tally only 
  toplo <- cntdat |> 
    dplyr::summarise(
      cnt = sum(`Scallops found`, na.rm = T), 
      .by = yr
    )
  
  # historical and missind data from 2017 ccmp graphic (FW-3)
  histdat <- tibble::tibble(
    yr = c(1996, 1997, 1998, 1999, 2000, 2004, 2005, 2006, 2012, 2013, 2016),
    cnt = c(75, 79, 27, 21, 18, 12, 1, 17, 12, 51, 54)
  )
  
  # combine
  toplo <- rbind(histdat, toplo) |> 
    dplyr::arrange(yr) |> 
    dplyr::mutate(
      ind = 1:dplyr::n()
    )
  
  # plot
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = ind, y = cnt)) +
    ggplot2::geom_line(color = 'grey') + 
    ggplot2::geom_point(pch = 21, fill = '#427355', size = 3) +
    ggplot2::scale_x_continuous(breaks = toplo$ind, labels = toplo$yr) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05 * max(toplo$cnt))) +
    ggplot2::geom_text(ggplot2::aes(label = cnt), vjust = -1, hjust = 0.5) +
    ggplot2::labs(
      x = NULL, 
      y = 'Scallops found'
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  
  return(p)
  
}

# fim catch plot
fim_plo <- function(){
  
  load(file = here::here('data/fimdat.RData'))  

  # cols <- c('#427355', '#5C4A42', '#958984', '#004F7E', '#00806E')
  cols <- c('#5C4A42', '#004F7E', '#00806E')
  
  toplo <- fimdat |> 
    dplyr::mutate(
      yr = lubridate::year(date),
      Commonname = factor(Commonname, levels = c('Pink Shrimp', 'Red Drum', 'Spotted Seatrout'))
    ) |> 
    dplyr::summarise(
      avev = mean(Number), 
      lov = t.test(Number)$conf.int[1],
      hive = t.test(Number)$conf.int[2],
      .by = c('yr', 'Commonname')
    ) |> 
    dplyr::filter(yr >= 1995)
  
  p <- ggplot2::ggplot(toplo, ggplot2::aes(x = yr, y = avev, color = Commonname, fill = Commonname)) +
    ggplot2::geom_line() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lov, ymax = hive), width = 0.25) +
    ggplot2::geom_point(pch = 21, color = 'black', size = 3) +
    ggplot2::facet_wrap(~Commonname, scales = 'free_y', ncol = 1) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(
      x = NULL, 
      y = 'Number per set'
    ) +
    ggplot2::theme_minimal() + 
    ggplot2::theme(
      legend.position = 'none', 
      panel.grid.minor.y = ggplot2::element_blank(), 
      strip.text = ggplot2::element_text(size = 12),
      # panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 12), 
      axis.text.x = ggplot2::element_text(size = 11)
    )
    
  return(p)
  
}

# activity table ------------------------------------------------------------------------------

# create expandable content for reactable
row_details <- function(datin, index){
  
  action <- datin[index, ] |> 
    dplyr::select(-Activity)
  
  # make names in actino an h3 header
  hds <- names(action) |> 
    lapply(htmltools::h3)
  
  ps <- action |> 
    lapply(htmltools::p)
  
  out <- NULL
  for(i in 1:ncol(action)){
    out <- c(out, hds[i], ps[i])
  }
  
  htmltools::div(out)
  
}

# activities table
act_tab <- function(ttl){
  
  load(file = here::here('data/activities.RData'))
  
  datin <- activities[[ttl]]
  
  if(is.null(datin))
    return()
  
  reactable::reactable(
    data.frame(datin[, 1]), 
    defaultColDef = reactable::colDef(
      name = 'Activities'
    ),
    details = function(index) row_details(datin, index)) 
  
}

# table functions -----------------------------------------------------------------------------

sw8_tab <- function(id, action){
  
  sht <- read_sheet(id, sheet = action, skip = 1, col_types = 'c')
  
  out <- flextable::flextable(sht) |> 
    flextable::autofit() 
  
  return(out)
  
}

ww1_tab <- function(id, action){
  
  sht <- read_sheet(id, sheet = action, skip = 1, col_types = 'c')
  
  out <- flextable::flextable(sht[-c(1), ]) |> 
    flextable::add_header(values = sht[1, ], top = F) |> 
    flextable::merge_at(i = 1, j = 2:4, part = 'header') |>
    flextable::merge_at(i = 1, j = 5:7, part = 'header') |> 
    flextable::bg(bg = "lightgray", part = "header") |> 
    flextable::border_remove() |> 
    flextable::hline(i = 4, part = 'body') |> 
    flextable::autofit()
  
  return(out)
  
}

coc1_tab <- function(id, action){
  
  sht <- read_sheet(id, sheet = action, skip = 1, col_types = 'c')

  out <- flextable::flextable(sht) |> 
    flextable::set_header_labels(`...1` = '') |>
    flextable::bg(bg = "lightgray", part = "header") |> 
    flextable::border_remove() |> 
    flextable::autofit()
  
  return(out)
  
}

ph5_tab <- function(id, action){
  
  sht <- read_sheet(id, sheet = action, skip = 1, col_types = 'c')
  
  out <- flextable::flextable(sht[-c(1), ]) |> 
    flextable::set_header_labels(i = 1, `...1` = '') |>
    flextable::add_header(values = sht[1, ], top = F) |> 
    flextable::merge_at(i = 1, j = 2:3, part = 'header') |>
    flextable::merge_at(i = 1, j = 4:5, part = 'header') |> 
    flextable::merge_at(i = 1, j = 6:7, part = 'header') |> 
    flextable::bg(bg = "lightgray", part = "header") |> 
    flextable::border_remove() |> 
    flextable::align(align = 'center', part = 'all', j = -1) |>
    flextable::vline(j = c(3, 5), part = 'all') |> 
    flextable::autofit()
  
  return(out)
  
}

# this gets seagrass and oyster coverage estimate by segment
bh4_tab <- function(maxyr = 2022){
  
  segs <- c('Old Tampa Bay', 'Hillsborough Bay', 'Middle Tampa Bay', 'Lower Tampa Bay', 'Boca Ciega Bay', 'Manatee River', 'Terra Ceia Bay')
  
  intseg <- sgseg |> 
    dplyr::filter(segment %in% segs) |>
    st_make_valid()
  
  sgurl <- paste0('https://github.com/tbep-tech/hmpu-workflow/raw/master/data/sgdat', maxyr, '.RData')
  sgdatraw <- rdataload(sgurl)
  
  sgdat <- sgdatraw |> 
    dplyr::filter(FLUCCSCODE %in% c(6540, 9113, 9116)) |> 
    dplyr::mutate(
      hab = dplyr::case_when(
        FLUCCSCODE == 6540 ~ 'Oyster',
        FLUCCSCODE == 9113 ~ 'Patchy seagrass',
        FLUCCSCODE == 9116 ~ 'Continuous seagrass'
      )
    ) |>
    st_transform(st_crs(intseg)) |> 
    st_intersection(intseg)
  
  sgdat <- sgdat |>
    dplyr::mutate(
      acres = st_area(x = sgdat),
      acres = units::set_units(acres, acres)
    ) |> 
    st_set_geometry(NULL) |> 
    dplyr::summarise(
      acres = sum(acres), 
      .by = c(segment, hab)
    )
  
  totab <- sgdat |> 
    dplyr::mutate(
      segment = factor(segment, levels = segs), 
      hab = factor(hab, levels = c('Patchy seagrass', 'Continuous seagrass', 'Oyster'), 
                   labels = c('Patchy seagrass (acres)', 'Continuous seagrass (acres)', 'Oyster (acres)')
      ), 
      acres = formatC(round(acres, 1), format = 'f', big.mark = ',', digits = 1)
    ) |> 
    tidyr::pivot_wider(names_from = hab, values_from = acres) |> 
    dplyr::arrange(segment)
  
  out <- flextable::flextable(totab) |> 
    flextable::set_header_labels(i = 1, `segment` = 'Bay segment') |>
    flextable::bg(bg = "lightgray", part = "header") |> 
    flextable::border_remove() |> 
    flextable::align(align = 'center', part = 'all', j = -1) |>
    flextable::autofit()
  
  return(out)
  
}

fw1_tab <- function(id, action){
  
  sht <- read_sheet(id, sheet = action, skip = 1, col_types = 'c')
  sht$`2016` <- as.numeric(sht$`2016`)
  sht$`2022` <- as.numeric(sht$`2022`)  
  sht$`% Increase` = ((sht$`2022` - sht$`2016`) / sht$`2016`) * 100

  out <- flextable::flextable(sht) |> 
    flextable::set_header_labels(i = 1, `...1` = '') |>
    flextable::colformat_double(j = 2:3, digits = 0, big.mark = ",") |>
    flextable::colformat_double(j = 4, digits = 1) |>
    flextable::bg(bg = "lightgray", part = "header") |> 
    flextable::align(align = 'left', part = 'all') |> 
    flextable::border_remove() |> 
    flextable::autofit()
  
  return(out)
  
}

fw6turtles_tab <- function(id, action){

  sht <- read_sheet(id, sheet = action, skip = 1, col_types = 'c')
  sht <- na.omit(sht)
  sht <- sht |> 
    dplyr::mutate_at(dplyr::vars(-County), as.numeric)
  
  out <- flextable::flextable(sht) |> 
    flextable::colformat_double(j = 2:ncol(sht), digits = 0, big.mark = ",") |>
    flextable::bg(bg = "lightgray", part = "header") |> 
    flextable::align(align = 'left', part = 'all') |> 
    flextable::bold(i = 4) |> 
    flextable::border_remove() |> 
    flextable::autofit()
  
  return(out)
  
}