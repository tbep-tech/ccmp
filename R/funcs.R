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

# pyro map otb
pyro_map <- function(phytodata, yr = 2021, mo = c('Jun', 'Oct')){
  
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
