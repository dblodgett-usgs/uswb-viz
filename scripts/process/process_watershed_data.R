# Loads, projects, and simplifies geometries.
process.process_watershed_map_data <- function(viz = as.viz("process_watershed_map_data")) {

  deps <- readDepends(viz)
  required <- c("fetch_map_data",
                "spatial_metadata")
  checkRequired(deps, required)
  
  watershed_map_data <- deps$fetch_map_data
  
  watershed_map_data <- lapply(watershed_map_data, sf::st_transform, crs = deps$`spatial_metadata`$crs)
  
  # simplify geometries
  watershed_map_data <- lapply(watershed_map_data, sf::st_simplify, dTolerance = deps$`spatial_metadata`$simplify_tolerance_m)
  
  saveRDS(watershed_map_data, viz[['location']])
}

# Adds svg markup to outlets
process.process_outlet_map_data <- function(viz = as.viz("process_outlet_map_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_map_data")
  checkRequired(deps, required)
  
  library(dplyr)
  library(sf) # dplyr calls select out geometry with loading sf.
  
  outlets <- deps[["process_watershed_map_data"]]$`http://www.opengeospatial.org/standards/waterml2/hy_features/HY_HydroNexus` %>%
    dplyr::select(huc12) %>% 
    dplyr::mutate(r = '2',
           class = 'outlet-dots') %>%
    dplyr::rename(id = huc12)
    
  saveRDS(outlets, viz[['location']])
}

# Adds svg markup to boundaries
process.process_boundary_map_data <- function(viz = as.viz("process_boundary_map_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_map_data")
  checkRequired(deps, required)
  
  library(dplyr)
  library(sf) # dplyr calls select out geometry with loading sf.
  
  boundaries <- deps[["process_watershed_map_data"]]$`http://www.opengeospatial.org/standards/waterml2/hy_features/HY_CatchmentDivide` %>%
    dplyr::select(huc12) %>%
    dplyr::mutate(onmouseover = sprintf("setEmphasis('%s'); setShow('wb-id-%s');", huc12, huc12),
                  class = 'watershed-boundary') %>%
    dplyr::rename(id = huc12)
  
  saveRDS(boundaries, viz[['location']])
}

# Adds svg markup to flowlines
process.process_flowline_map_data <- function(viz = as.viz("process_flowline_map_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_map_data")
  checkRequired(deps, required)
  
  library(dplyr)
  library(sf) # dplyr calls select out geometry with loading sf.
  
  flowline <- sf::st_simplify(deps[["process_watershed_map_data"]]$`http://www.opengeospatial.org/standards/waterml2/hy_features/HY_HydrographicNetwork`, viz[["simp_tolerance"]])
  
  flowline <-  flowline %>%
    dplyr::mutate(class = 'nhdplus-flowline') %>%
    dplyr::rename(id = huc12)
  
  saveRDS(flowline, viz[['location']])
}

process.process_watershed_annual_wb_data <- function(viz = as.viz("process_watershed_annual_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("fetch_nwc_wb_data")
  checkRequired(deps, required)
  
  wb_data <- deps[["fetch_nwc_wb_data"]]
  
  wb_data <- lapply(wb_data, process_wb_ts)
  
  saveRDS(wb_data, viz[["location"]])
}

process_wb_ts <- function(wb_data) {
  
  an_e <- NWCEd::annualize(wb_data$et, method = sum)
  an_e$data <- an_e$data*0.0393701
  an_e$group <- "ET"
  
  an_p <- NWCEd::annualize(wb_data$prcp, method = sum)
  an_p$data <- an_p$data*0.0393701
  an_p$group <- "PR"
  
  da_sqft <- dataRetrieval::readNWISsite(wb_data$streamflow$site_no[1])$drain_area_va*27878400
  
  wb_data$streamflow <- dplyr::filter(wb_data$streamflow, data_00060_00003 > 0)
  
  an_q <- NWCEd::annualize(setNames(wb_data$streamflow[c("Date", "data_00060_00003")], 
                                    c("date", "data")),
                           method = mean)
  an_q$data <- an_q$data*(60 * 60 * 24 * 365.25) * 12/da_sqft
  an_q$group <- "Q"
  
  dplyr::filter(rbind(an_e, an_p, an_q), as.numeric(year) > 1999 & as.numeric(year) < 2016)
}

process.process_watershed_por_wb_data <- function(viz = as.viz("process_watershed_por_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_annual_wb_data")
  checkRequired(deps, required)
  
  wb_data <- deps$`process_watershed_annual_wb_data`
  
  wb_data <- lapply(wb_data, process_wb_por)
  
  saveRDS(wb_data, viz[["location"]])
}

process_wb_por <- function(wb_data) {
  library(magrittr)
  
  component_labels = data.frame(group = c( "PR", "ET", "Q"), 
                                label = c("Precipitation",
                                          "Evapotranspiration",
                                          "Streamflow"),
                                inout = c("In", "Out", "Out"),
                                stringsAsFactors = F)

  dplyr::group_by(wb_data, group) %>% 
    dplyr::summarise(value = mean(data)) %>%
    dplyr::left_join(component_labels, by = "group") %>%
    dplyr::mutate(group = label) %>%
    dplyr::select(-label)
  
}

# grabs the matches between huc and NWIS id
process.process_wb_huc_nwis <- function(viz = as.viz("process_wb_huc_nwis")) {
  
  deps <- readDepends(viz)
  required <- c("fetch_nwc_wb_data")
  checkRequired(deps, required)
  
  wb_data <- deps[["fetch_nwc_wb_data"]]
  
  saveRDS(lapply(wb_data, function(x) x$streamflow$site_no[[1]]), viz[["location"]])
}
