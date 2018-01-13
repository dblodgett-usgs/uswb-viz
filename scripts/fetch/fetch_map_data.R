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
    # ws <- HU_ids[1]
    url <- paste0(base_url, ws, ".json")
    
    content <- rawToChar(httr::GET(url)$content)
    jl <- jsonlite::fromJSON(content)
    
    urls <- c(jl$catchmentRealization, jl$outflow)
    
    out_data <- lapply(paste0(urls, ".json"), parse_elfie_json)
    
    if(length(out_data) != 3) stop("Expected three features.")
    
    for(i in 1:3) {
      map_data[[names(out_data[[i]])]][[ws]] <- out_data[[i]][[1]]
    }
    
    hu_names[[ws]] <- jl$name
    
  }
  
  for(i in 1:3) {
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
  content <- rawToChar(httr::GET(url)$content)
  jl <- jsonlite::fromJSON(content)
  name <- jl$`@type`
  if(!is.null(jl$geo) && !is.null(jl$geo$`@type`)) {
    if(jl$geo$`@type` == "schema:GeoCoordinates") {
      sfg <- sf::st_point(c(jl$geo$longitude, jl$geo$latitude))
    } else if(jl$geo$`@type` == "schema:GeoShape") {
      if(!is.null(jl$geo$`schema:polygon`)) {
        if(!is.list(jl$geo$`schema:polygon`$geometry$coordinates) && dim(jl$geo$`schema:polygon`$geometry$coordinates)[1]==1 && dim(jl$geo$`schema:polygon`$geometry$coordinates)[2]==1) {
          sfg <- sf::st_polygon(list(matrix(jl$geo$`schema:polygon`$geometry$coordinates[1,1,,], ncol = 2, byrow = F)))
        } else if(is.list(jl$geo$`schema:polygon`$geometry$coordinates)) {
          sfg <- sf::st_multipolygon(lapply(jl$geo$`schema:polygon`$geometry$coordinates[[1]], function(x) sf::st_polygon(list(x))))
        } else {
          stop("found a multipolygon or multiple features, not supported")
        }
      } else if(!is.null(jl$geo$`schema:line`)) {
        sfg <- sf::st_multilinestring(lapply(jl$geo$`schema:line`$geometry$coordinates[[1]], sf::st_linestring))
      }
    }
  }
  out <- list()
  out[[name]] <- sfg
  return(out)
}
