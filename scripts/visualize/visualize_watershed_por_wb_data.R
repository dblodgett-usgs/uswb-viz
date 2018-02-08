visualize.visualize_watershed_por_wb_data <- function(viz = as.viz("visualize_watershed_por_wb_data")) {
  
  deps <- readDepends(viz)
  required <- c("process_watershed_por_wb_data", "fetch_wb_bar_template", "process_watershed_map_data")
  checkRequired(deps, required)
  
  all_wb_data <- deps[["process_watershed_por_wb_data"]]
  
  library(sf)
  title_data <- deps[["process_watershed_map_data"]]$`http://www.opengeospatial.org/standards/waterml2/hy_features/HY_CatchmentDivide` %>%
    dplyr::select(huc12, name)
  sf::st_geometry(title_data) <- NULL 

  titles <- setNames(as.character(title_data$name), title_data$huc12)
  
  template <- deps[["fetch_wb_bar_template"]]
  
  wb_svg_size <- c(0, 0, 360, 300)
  
  svgs <- lapply(X = names(all_wb_data),
                 FUN = build_watershed_por_wb_svg_list,
                 all_wb_data,
                 view_box = wb_svg_size,
                 titles = titles,
                 scale_max = 70)
  
  wb_svg_size[4] <- 320
  
  template_list <- list(viewbox = paste(wb_svg_size, collapse = " "),
                        top_id = "visualize_watershed_por_wb_data",
                        title = viz[["title"]], 
                        alttext = viz[["alttext"]], 
                        barcharts = svgs)
  
  cat(whisker::whisker.render(template, template_list), file = viz[['location']])
}

build_watershed_por_wb_svg_list <- function(wb, all_wb_data, view_box, titles, scale_max = NULL) {
  min_x <- view_box[1]
  min_y <- view_box[2]
  max_x <- view_box[3]
  max_y <- view_box[4]  
  
  wb_data <- tidyr::spread(all_wb_data[[wb]][c("group", "value")], key = "group", value = "value")

  # these are just for code clarity

  pr <- wb_data$Precipitation 
  et <- wb_data$Evapotranspiration
  q <- wb_data$Streamflow
  
  u <- pr - et - q
  
  if(et > (pr - q)) {
    max_wb_bar <- et + q
  } else {
    max_wb_bar <- pr
  }
  
  max_bar <- scale_max
  
  # Constants
  # Fractions are relative to 288
  margin <- 20 # 0.052
  x_tick_allow <- 25 # 0.087
  x_tick_text_adj <- 3 # 0.01
  y_tick_allow <- 25 # 0.087
  y_tick_text_x <- 35
  
  title_x = margin - 5
  title_y = margin / 1.5
  
  rect_w <- 60 # 0.2
  
  # Ticks
  top_tick <- floor((scale_max-5) / 10) * 10
  bottom_tick <- top_tick / 2
  
  #fictitious "max_bar"
  max_bar_y <- margin
  max_bar_h <- (max_y - min_y) - (2 * margin + x_tick_allow)
 
  # Relative calculations
  p_rect_x <- margin + x_tick_allow 
  eq_rect_x <- p_rect_x + rect_w
  
  if(u >= 0) {
    u_rect_x <- eq_rect_x
    ueq_rect_h <- max_bar_h * u / max_bar
    up_rect_h <- 0
    u_rect_h <- ueq_rect_h
  } else {
    u_rect_x <- p_rect_x
    ueq_rect_h <- 0
    up_rect_h <- max_bar_h * -u / max_bar
    u_rect_h <- up_rect_h
    u <- -u
  }
  
  p_rect_y <- max_bar_y + ((max_bar - pr) / max_bar) * max_bar_h
  p_rect_h <- max_bar_h * pr / max_bar
  
  max_wb_bar_y <- max_bar_y + ((max_bar - max_wb_bar) / max_bar) * max_bar_h
  
  u_rect_y <- max_wb_bar_y
  
  e_rect_y <- u_rect_y + ueq_rect_h 
  e_rect_h <- max_bar_h * et / max_bar
  
  q_rect_y <- e_rect_y + e_rect_h
  q_rect_h <- max_bar_h * q / max_bar
  
  top_tick_text_y <- max_bar_y + max_bar_h - max_bar_h * top_tick / max_bar
  bottom_tick_text_y <- max_bar_y + max_bar_h - max_bar_h * bottom_tick / max_bar
  top_tick_y <- top_tick_text_y - 1
  bottom_tick_y <- bottom_tick_text_y - 1
  tick_x1 <- y_tick_text_x + 3
  tick_x2 <- tick_x1 + 5
  
  in_x <- p_rect_x + rect_w / 2
  in_y <- out_y <- max_bar_y + max_bar_h + y_tick_allow / 2
  out_x <- eq_rect_x + rect_w / 2
  
  y_units_x <- margin - 5
  y_units_y <- max_x / 2.5
  
  legend_boxes_x <- eq_rect_x + rect_w + 10
  legend_box_size <- 16
  legend_text_x <- legend_boxes_x + (legend_box_size / .8)
  legend_p_y <- max_bar_y + (max_bar_h / 2) - (legend_box_size * 3)
  legend_e_y <- legend_p_y + (legend_box_size * 2)
  legend_q_y <- legend_p_y + (legend_box_size * 4)
  legend_u_y <- legend_p_y + (legend_box_size * 7)
  
  legend_text_p_y <- legend_p_y + (legend_box_size / 2)
  legend_text_e_y <- legend_e_y + (legend_box_size / 2)
  legend_text_q_y <- legend_q_y + (legend_box_size / 2)
  legend_text_u_y <- legend_u_y + (legend_box_size / 2)
  
  legend_text_spacing <- 14
  legend_textn_p_y <- legend_text_p_y + legend_text_spacing
  legend_textn_q_y <- legend_text_q_y + legend_text_spacing
  legend_textn_e_y <- legend_text_e_y + legend_text_spacing
  legend_textn_u_y <- legend_text_u_y + legend_text_spacing
  
  legend_title_x <- legend_boxes_x
  legend_title_y <- legend_p_y - legend_text_spacing
  
  caption_text_y <- out_y + y_tick_allow/2
  
  template_list <-list(hu_id = paste0("wb-id-", wb), starting_class = "nill",
       title_x = title_x, title_y = title_y, title = titles[[wb]],
       rect_w = rect_w,
       p_rect_h = p_rect_h, p_rect_x = p_rect_x, p_rect_y = p_rect_y,
       max_rect_x = p_rect_x, max_rect_y = max_bar_y, max_rect_h = max_bar_h,
       eq_rect_x = eq_rect_x,
       u_rect_x = u_rect_x,
       u_rect_y = u_rect_y, u_rect_h = u_rect_h,
       q_rect_y = q_rect_y, q_rect_h = q_rect_h,
       e_rect_y = e_rect_y, e_rect_h = e_rect_h,
       top_tick_text_y = top_tick_text_y, bottom_tick_text_y = bottom_tick_text_y,
       y_tick_text_x = y_tick_text_x, bottom_tick_y = bottom_tick_y,
       top_tick_y = top_tick_y, bottom_tick = bottom_tick, top_tick = top_tick,
       tick_x1 = tick_x1, tick_x2 = tick_x2,
       in_x = in_x, in_y = in_y, out_x = out_x, out_y = out_y,
       y_units_x = y_units_x, y_units_y = y_units_y,
       legend_boxes_x = legend_boxes_x, legend_box_size = legend_box_size, 
       legend_p_y = legend_p_y, legend_e_y = legend_e_y, legend_q_y = legend_q_y, 
       legend_u_y = legend_u_y, legend_text_x = legend_text_x, 
       legend_text_p_y = legend_text_p_y, legend_text_e_y = legend_text_e_y, 
       legend_text_q_y = legend_text_q_y, legend_text_u_y = legend_text_u_y,
       legend_textn_p_y = legend_textn_p_y, legend_textn_e_y = legend_textn_e_y, 
       legend_textn_q_y = legend_textn_q_y, legend_textn_u_y = legend_textn_u_y,
       legend_p_val = round(pr, digits = 1), legend_e_val = round(et, digits = 1), 
       legend_q_val = round(q, digits = 1), legend_u_val = round(u, digits = 1),
       legend_title_x = legend_title_x, legend_title_y = legend_title_y,
       caption_text_x = y_units_x, caption_text_y = caption_text_y)
  
  template_list
}
