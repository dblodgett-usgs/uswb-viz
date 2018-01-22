fetch.fetch_map_data <- function(viz = as.viz("fetch_map_data")) {
  
  deps <- readDepends(viz)
  required <- "fetch_HU_ids"
  checkRequired(deps, required)
  
  HU_ids <- deps[["fetch_HU_ids"]]
  
  base_url <- "https://opengeospatial.github.io/ELFIE/usgs/huc12/uswb/"
  
  hu_names <- setNames(HU_ids, HU_ids)
  md <- setNames(rep(list(list()), length(HU_ids)), HU_ids)
  
  map_data <- list("http://www.opengeospatial.org/standards/waterml2/hy_features/HY_CatchmentDivide" = md,
                   "http://www.opengeospatial.org/standards/waterml2/hy_features/HY_HydrographicNetwork" = md,
                   "http://www.opengeospatial.org/standards/waterml2/hy_features/HY_HydroNexus" = md)
  
  for(ws in HU_ids) {
    url <- paste0(base_url, ws, ".json")
    
    content <- rawToChar(httr::GET(url)$content)
    jl <- jsonlite::fromJSON(content, simplifyVector = F)
    
    urls <- c(jl$catchmentRealization, jl$outflow)
    
    for(f in 1:length(urls)) {
       map_data[[urls[[f]]$`@type`]][[ws]] <- parse_elfie_json(paste0(urls[[f]]$`@id`, ".json"))[[1]]
    }
    
    hu_names[[ws]] <- jl$name
    
  }
  
  for(i in 1:length(map_data)) {
    map_data[[i]] <- sf::st_sf(geometry = sf::st_sfc(map_data[[i]]), 
                               data.frame(huc12 = HU_ids, 
                                          row.names = HU_ids, 
                                          stringsAsFactors = F))
    
    sf::st_crs(map_data[[i]]) <- sf::st_crs("+init=epsg:4326")
    
    map_data[[i]]$name <- hu_names
  }
  
  saveRDS(map_data, viz[["location"]])
}

fetchTimestamp.fetch_map_data <- alwaysCurrent

parse_elfie_json <- function(url) {
  jl <- jsonlite::fromJSON(url)
  name <- jl$`@type`
  out <- list()
  if(!is.null(jl$geo) && !is.null(jl$geo$`@type`)) {
    if(jl$geo$`@type` == "schema:GeoCoordinates") {
      sfg <- sf::st_point(c(jl$geo$longitude, jl$geo$latitude))
      jl$geo <- sfg
      out[[name]] <- jl$geo
    }
    if(!is.null(jl$geometry)) {
      names(jl$geometry)[which(names(jl$geometry) == "@type")] <- "type"
      jl$geometry <- sf::read_sf(jsonlite::toJSON(jl$geometry, auto_unbox = T))$geometry[[1]]
      out[[name]] <- jl$geometry
    }
  }
  return(out)
}
