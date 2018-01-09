visualize.visualize_map_thumbnail <- function(viz) {
  library(dplyr)
  library(sf)
  
  data <- readDepends(viz)
  required <- c("process_flowline_map_data",
                "process_boundary_map_data",
                "process_outlet_map_data")
  checkRequired(data, required)
  
  hu <- "140100051906"
  
  flines <- data[["process_flowline_map_data"]]
  flines <- flines[which(flines$id == hu),]$geometry
  
  boundary <- data[["process_boundary_map_data"]]
  boundary <- boundary[which(boundary$id == hu),]$geometry
  
  outlet <- data[["process_outlet_map_data"]]
  outlet <- outlet[which(outlet$id == hu),]$geometry

  height <- viz[["fig-height"]]
  width <- viz[["fig-width"]]
  
  png(filename = viz[['location']], width = width, height = height, units = 'px')
  
  par(mar=c(0, 0, 0, 0), oma=c(0, 0, 0, 0), bg='white')
  
  if(height == width) {
    xlim <- c(-800000, -467000)
    ylim <- c(-800000, -455000)
    x1 <- -760000
    y1 <- -475000
    x2 <- x1
    y2 <- -505000
    x3 <- x1
    y3 <- -535000
    text_size <- 3
  } else {
    xlim <- c(-1020000, -467000)
    ylim <- c(-790000, -465000)
    x1 <- -1015000
    y1 <- -530000
    x2 <- x1
    y2 <- -600000
    x3 <- x1
    y3 <- -670000
    text_size <- 4.5
  }
  plot(x = NULL, y = NULL, xlim = xlim, ylim = ylim)
  plot(boundary, col = "grey", add = T)
  plot(flines, col = "blue", add = T)
  plot(outlet, pch = 20, cex = 3, col = "red", add = T)
  text(x1, y1, "Precipitation", cex = text_size, pos = 4)
  text(x2, y2, "Evaporation", cex = text_size, pos = 4)
  text(x3, y3, "Runoff", cex = text_size, pos = 4)
  dev.off()
}

visualize.visualize_map <- function(viz = as.viz("visualize_map")){
  
  deps <- readDepends(viz)
  required <- c("visualize_svg_base_map", "fetch_usgs_watermark")
  checkRequired(deps, required)
  # Also expects alttext, title, id, and location from viz.
  
  svg <- deps[["visualize_svg_base_map"]]
  watermark <- deps[['fetch_usgs_watermark']]
  
  # add alttext and title before the children of the root element
  xml2::xml_add_sibling(xml2::xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml2::xml_add_sibling(xml2::xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  xml2::xml_attr(svg, "id") <- viz[['id']]
  vb <- strsplit(xml2::xml_attr(svg, 'viewBox'),'[ ]')[[1]] # can be used for portrait vs landscape, see Maria
  
  # get the big dog that has all the stuff that is geo:
  map_elements <- xml2::xml_find_first(svg, "//*[local-name()='g'][@id='map-elements']") 
  
  # add sibling element below map_elements (before in xml, below in svg layer)
  non_geo_bot <- xml2::xml_add_sibling(map_elements, 'g', 'id' = 'non-geo-bottom', .where='before')
  
  # Add sibling of map_elements (after in xml, above in svg layer)
  non_geo_top <- xml2::xml_add_sibling(map_elements, 'g', 'id' = 'non-geo-top', .where='after')
  
  # Add tooltip group
  g_tool <- xml2::xml_add_sibling(non_geo_top, 'g', id='tooltip-group')
  
  # Add defs
  d <- xml2::xml_add_child(svg, 'defs', .where='before')
  
  # Add background with pause class
  xml2::xml_add_child(non_geo_bot, 'rect', width="100%", height="100%", class='viz-background-color viz-pause', id='viz-background')
  
  # map-elements-{mode}-top is where all of the mouseovers, tips, and click events go
  map_elements_top <- xml2::xml_add_child(svg, 'g', id = "map-elements-mouser")
  
  # Could use "as_mouse_topper" here. See Maria.
  
  xml2::xml_add_child(g_tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml2::xml_add_child(g_tool, 'path', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml2::xml_add_child(g_tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="tooltip-text-label svg-text", " ")
  
  g_watermark <- xml2::xml_add_child(non_geo_top, watermark)
  xml2::xml_attr(g_watermark, "transform") <- sprintf('translate(%s,%s)scale(0.2)', 
                                                as.character(as.numeric(vb[1])+viz[["watermark_transform"]][1]), 
                                                as.character(as.numeric(vb[2])+viz[["watermark_transform"]][2]))
  
  # Hack job here because there is a problem in svg_base_map
  poly_lines <- xml2::xml_find_all(svg, "//*[local-name()='polyline']")
  xml2::xml_attr(poly_lines, "id") <- 'flowline'
  xml2::xml_attr(poly_lines, "class") <- 'nhdplus-flowline'
  
  xml2::write_xml(svg, viz[['location']])
  
}

#' remove mouser events from style geometries, and add them to a new invisible mouser group overlay
as_mouse_topper <- function(svg, style.group.id, mouser.parent.id){
  
  style.kids <- xml_children(
    xml_find_first(
      svg, sprintf("//*[local-name()='g'][@id='%s']", style.group.id)
    )
  )
  
  parent.element <- xml_find_first(
    svg, sprintf("//*[local-name()='g'][@id='%s']", mouser.parent.id)
  )
  g.mouser <- xml_add_child(parent.element, 'g', id = paste0(style.group.id, '-mousers'))
  
  
  transfers <- c('onmousemove', 'onmouseout', 'onclick')
  for (style.kid in style.kids){
    mouse.kid <- xml_add_child(g.mouser, 'use', 'xlink:href'=sprintf("#%s", xml_attr(style.kid, 'id')), class = 'mouser')
    .jnk <- lapply(X = transfers, FUN = function(x) {
      xml_attr(mouse.kid, x) <- xml_attr(style.kid, x)
      xml_attr(style.kid, x) <- NULL
    }
    )
  }
  # does it work to let the reference element keep its class? no
  
}
